open Rresult
open Lwt.Infix
include Conduit_lwt_core
module Ke = Ke.Rke.Weighted

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error err -> Lwt.return (Error err)

let ok x = Lwt.return (Ok x)

let err x = Lwt.return (Error x)

module Flow (P : PROTOCOL) = struct
  type flow = P.t

  type error = P.error

  type write_error = [ Mirage_flow.write_error | `E of error ]

  let pp_error = P.pp_error

  let pp_write_error ppf = function
    | #Mirage_flow.write_error as err -> Mirage_flow.pp_write_error ppf err
    | `E err -> P.pp_error ppf err

  let read flow =
    let raw = Cstruct.create 0x1000 in
    P.recv flow raw >|= function
    | Ok `End_of_flow -> Ok `Eof
    | Ok (`Len len) -> Ok (`Data (Cstruct.sub raw 0 len))
    | Error _ as err -> err

  let write flow raw =
    let rec go x =
      if Cstruct.len x = 0
      then ok ()
      else
        P.send flow x >>= function
        | Error e -> err (`E e)
        | Ok len -> go (Cstruct.shift x len) in
    go raw

  let writev flow cs =
    let rec go = function
      | [] -> ok ()
      | x :: r -> (
          write flow x >>= function
          | Ok () -> go r
          | Error _ as err -> Lwt.return err) in
    go cs

  let close flow = P.close flow >>= fun _ -> Lwt.return_unit
end

module TCP (S : Mirage_stack.V4) = struct
  type endpoint = {
    stack : S.t;
    keepalive : Mirage_protocols.Keepalive.t option;
    nodelay : bool;
    ip : Ipaddr.V4.t;
    port : int;
  }

  let endpoint ?keepalive ?(nodelay = false) stack (ip, port) =
    { keepalive; nodelay; stack; ip; port }

  let pp_keepalive =
    let open Fmt.Dump in
    record
      [
        field "after" (fun t -> t.Mirage_protocols.Keepalive.after) Duration.pp;
        field "interval"
          (fun t -> t.Mirage_protocols.Keepalive.interval)
          Duration.pp;
        field "probes" (fun t -> t.Mirage_protocols.Keepalive.probes) Fmt.int;
      ]

  let pp_endpoint =
    let open Fmt.Dump in
    record
      [
        field "keepalive" (fun t -> t.keepalive) (option pp_keepalive);
        field "nodelay" (fun t -> t.nodelay) Fmt.bool;
        field "ip" (fun t -> t.ip) Ipaddr.V4.pp;
        field "port" (fun t -> t.port) Fmt.int;
      ]

  let src = Logs.Src.create "tuyau-mirage-tcpip"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t = {
    flow : S.TCPV4.flow;
    nodelay : bool;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.t;
    mutable closed : bool;
  }

  type a_flow += Flow of { endpoint : endpoint; flow : t }

  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Lwt.t

  type error =
    [ `Input_too_large
    | `TCP_error of S.TCPV4.error
    | `Write_error of S.TCPV4.write_error
    | `Connection_aborted
    | `Exn of exn
    | (* XXX(dinosaure): it appears that [Tcpip_stack_socket] can raise
         exception. We should handle them and consider our fd ressource
         as close. *)
      Conduit.error ]

  let pp_error ppf (e : error) =
    match e with
    | `Input_too_large -> Fmt.string ppf "Input too large"
    | `TCP_error err -> S.TCPV4.pp_error ppf err
    | `Write_error err -> S.TCPV4.pp_write_error ppf err
    | `Exn exn -> Fmt.pf ppf "Exception: %s" (Printexc.to_string exn)
    | `Connection_aborted -> Fmt.string ppf "Connection aborted"
    | #Conduit.error as e -> Conduit.pp_error ppf e

  let tcp_error : S.TCPV4.error -> error = fun err -> `TCP_error err

  let write_error : S.TCPV4.write_error -> error = fun err -> `Write_error err

  let connect { stack; keepalive; nodelay; ip; port } =
    let tcpv4 = S.tcpv4 stack in
    S.TCPV4.create_connection tcpv4 ?keepalive (ip, port)
    >|= R.reword_error tcp_error
    >>? fun flow ->
    let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
    ok { flow; nodelay; queue; closed = false }

  let length = Cstruct.len

  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let recv t raw =
    match Ke.N.peek t.queue with
    | [] ->
        if not t.closed
        then (
          Log.debug (fun m -> m "<- Read the TCP flow.")
          (* XXX(dinosaure): with [Tcpip_stack_socket], [read] can raise
             [Lwt.Canceled] if the ressource take a time (a [Timeout] is
             returned by [select]). To prevent that, we decide to
             protect [StackV4.TCPV4.read] with [Lwt.no_cancel]. *) ;
          Lwt.catch
            (fun () ->
              Lwt.no_cancel (S.TCPV4.read t.flow) >|= R.reword_error tcp_error)
            (fun exn -> err (`Exn exn))
          >>= function
          | Error err as v ->
              Log.err (fun m -> m "Got an error while reading: %a" pp_error err) ;
              t.closed <- true ;
              Lwt.return v
          | Ok `Eof ->
              t.closed <- true ;
              Log.debug (fun m -> m "<- End of input.") ;
              ok `End_of_flow
          | Ok (`Data buf) ->
              Log.debug (fun m -> m "<- Got %d byte(s)." (Cstruct.len buf)) ;
              (* XXX(dinosaure): [telnet] send '\004' (End Of
                 Transmission) to ask the service to close the
                 connection. [mirage-tcpip] does not handle this
                 _opcode_ so we handle it in this place. *)
              if Cstruct.len buf = 1 && Cstruct.get_char buf 0 = '\004'
              then (
                S.TCPV4.close t.flow >>= fun () ->
                Log.debug (fun m -> m "<- End of input (end of transmission)") ;
                ok `End_of_flow)
              else
                let max_buf = Cstruct.len buf in
                let max_raw = Cstruct.len raw in
                if max_buf <= max_raw
                then (
                  Cstruct.blit buf 0 raw 0 max_buf ;
                  ok (`Len max_buf))
                else (
                  Cstruct.blit buf 0 raw 0 max_raw ;
                  let len = min (max_buf - max_raw) (Ke.available t.queue) in
                  Log.debug (fun m -> m "<- Save %d into the queue." len) ;
                  let _ =
                    Ke.N.push_exn ~blit ~length ~off:max_raw ~len t.queue buf
                  in
                  if len = max_buf - max_raw
                  then ok (`Len max_raw)
                  else err `Input_too_large))
        else ok `End_of_flow
    | lst ->
        let rec go consumed raw = function
          | [] ->
              Log.debug (fun m -> m "<- Shift %d bytes." consumed) ;
              Ke.N.shift_exn t.queue consumed ;

              (* XXX(dinosaure): it's important to return what we can
                 instead to fill [raw] as much as we can. Into
                 details, it's pretty close to the TLS stack when
                 [Tls.Engine.state] expects to terminate the handshake
                 as soon as possible. In this case, pending payload
                 can serve the end of the handshake and ask then to
                 [Tls.Engine.state] to send something and go to the
                 next action (according the underlying server logic)
                 when the client side, at this step expect to read
                 some bytes. *)
              ok (`Len consumed)
          | x :: r ->
              let x = Cstruct.of_bigarray x in
              let len = min (Cstruct.len x) (Cstruct.len raw) in
              Cstruct.blit x 0 raw 0 len ;
              if len = Cstruct.len raw
              then (
                Log.debug (fun m -> m "<- Shift %d bytes." (consumed + len)) ;
                Ke.N.shift_exn t.queue (consumed + len) ;
                ok (`Len (consumed + len)))
              else go (consumed + len) (Cstruct.shift raw len) r in
        go 0 raw lst

  let send t raw =
    (* XXX(dinosaure): with [Tcpip_stack_socket], protect against SIGPIPE. *)
    if t.closed
    then err `Closed
    else (
      Log.debug (fun m -> m "-> Start to write %d byte(s)." (Cstruct.len raw)) ;
      let send flow raw =
        if t.nodelay
        then S.TCPV4.write_nodelay flow raw
        else S.TCPV4.write flow raw in
      Lwt.catch
        (fun () -> send t.flow raw >|= R.reword_error write_error)
        (fun exn -> err (`Exn exn))
      >|= function
      | Error err as v ->
          t.closed <- true ;
          Log.err (fun m -> m "-> Got an error when writing: %a" pp_error err) ;
          v
      | Ok () ->
          Log.debug (fun m -> m "-> Write %d byte(s)." (Cstruct.len raw)) ;
          Ok (Cstruct.len raw))

  let close t =
    if t.closed
    then (
      Log.debug (fun m -> m "Connection already closed.") ;
      ok ())
    else (
      Log.debug (fun m -> m "Close the connection") ;
      S.TCPV4.close t.flow >>= ok)
end

module Resolve
    (S : Mirage_stack.V4)
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK) =
struct
  include Dns_client_mirage.Make (R) (T) (C) (S)

  let resolve t ?nameserver ~port domain_name =
    gethostbyname ?nameserver t domain_name >|= function
    | Ok domain_name -> Some (domain_name, port)
    | Error _err -> None
end

module Server (S : Mirage_stack.V4) = struct
  include Conduit_lwt_core.Server

  module TCP = struct
    module Flow = TCP (S)

    type +'a io = 'a Lwt.t

    type input = Cstruct.t

    type output = Cstruct.t

    type config = {
      stack : S.t;
      keepalive : Mirage_protocols.Keepalive.t option;
      nodelay : bool;
      port : int;
    }

    let config ?keepalive ?(nodelay = false) stack port =
      { stack; port; keepalive; nodelay }

    type t = {
      stack : S.t;
      queue : S.TCPV4.flow Queue.t;
      condition : unit Lwt_condition.t;
      mutex : Lwt_mutex.t;
      nodelay : bool;
      mutable closed : bool;
    }

    type flow = Flow.t

    let recv = Flow.recv

    let send = Flow.send

    let close = Flow.close

    type a_flow += Flow of flow

    type error = Flow.error

    let pp_error = Flow.pp_error

    let init { stack; keepalive; nodelay; port } =
      let queue = Queue.create () in
      let condition = Lwt_condition.create () in
      let mutex = Lwt_mutex.create () in
      let listener flow =
        Lwt_mutex.lock mutex >|= fun () ->
        Queue.push flow queue ;
        Lwt_condition.signal condition () ;
        Lwt_mutex.unlock mutex in
      S.listen_tcpv4 ?keepalive stack ~port listener ;
      ok { stack; queue; condition; mutex; nodelay; closed = false }

    let rec accept ({ queue; condition; mutex; nodelay; closed; _ } as t) =
      Lwt_mutex.lock mutex >>= fun () ->
      let rec await () =
        if Queue.is_empty queue && not closed
        then Lwt_condition.wait condition ~mutex >>= await
        else Lwt.return () in
      await () >>= fun () ->
      match Queue.pop queue with
      | flow ->
          let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
          Lwt_mutex.unlock mutex ;
          ok { Flow.flow; nodelay; queue; closed = false }
      | exception Queue.Empty ->
          if closed
          then (
            Lwt_mutex.unlock mutex ;
            err `Connection_aborted)
          else (
            Lwt_mutex.unlock mutex ;
            accept t)

    let stop ({ stack; mutex; _ } as t) =
      Lwt_mutex.with_lock mutex (fun () ->
          S.disconnect stack >|= fun () ->
          t.closed <- true ;
          Ok ())
  end
end
