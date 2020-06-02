module Ke = Ke.Rke

let option_fold ~none ~some = function Some x -> some x | None -> none

(* NOTE(dinosaure): we use an unbound queue where TLS can produce
   something bigger than the given input. It seems hard to limit
   the internal queue and arbitrary limit (like a queue two times
   larger than the input) is not good. By this fact, we use [Ke.Rke]
   even if it an infinitely grow. *)

module type FLOW =
  Conduit.FLOW with type input = Cstruct.t and type output = Cstruct.t

module Flow (IO : Conduit.IO) (X : FLOW with type 'a io = 'a IO.t) = struct
  module IO = IO

  let ok x = IO.return (Ok x)

  let err x = IO.return (Error x)

  let ( >>= ) x f = IO.bind x f

  let ( >>| ) x f = x >>= fun x -> IO.return (f x)

  let ( >>? ) x f = x >>= function Ok x -> f x | Error e -> err e

  let reword_error f = function Ok v -> Ok v | Error err -> Error (f err)

  let src = Logs.Src.create "conduit-tls"

  module Log = (val Logs.src_log src : Logs.LOG)

  type flow = {
    mutable tls : Tls.Engine.state option;
    mutable closed : bool;
    raw : Cstruct.t;
    flow : X.t;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.t;
  }

  let raw { flow; _ } = flow

  let handshake_in_progress { tls; _ } =
    match tls with
    | Some tls -> Tls.Engine.handshake_in_progress tls
    | None -> false

  type +'a io = 'a IO.t

  type input = X.input

  type output = X.output

  type error =
    [ `Msg of string
    | `Flow of X.error
    | `TLS of Tls.Engine.failure
    | `Closed_by_peer
    | Conduit.error ]

  let pp_error : error Fmt.t =
   fun ppf -> function
    | `Msg err -> Fmt.string ppf err
    | `Flow err -> X.pp_error ppf err
    | `TLS failure -> Fmt.string ppf (Tls.Engine.string_of_failure failure)
    | `Closed_by_peer -> Fmt.string ppf "Closed by peer"
    | #Conduit.error as e -> Conduit.pp_error ppf e

  let flow_error err = `Flow err

  let err_closed = err `Closed

  let flow_wr_opt : X.t -> Cstruct.t option -> (unit, error) result io =
   fun flow -> function
    | None -> ok ()
    | Some raw ->
        Log.debug (fun m -> m "~> Send %d bytes" (Cstruct.len raw)) ;
        let rec go raw =
          X.send flow raw >>| reword_error flow_error >>? fun len ->
          let raw = Cstruct.shift raw len in
          if Cstruct.len raw = 0 then ok () else go raw in
        go raw

  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let queue_wr_opt queue = function
    | None -> ()
    | Some raw ->
        Log.debug (fun m ->
            m "Fill the queue with %d byte(s)." (Cstruct.len raw)) ;
        Ke.N.push queue ~blit ~length:Cstruct.len ~off:0 raw

  let handle_tls :
      Tls.Engine.state ->
      (char, Bigarray.int8_unsigned_elt) Ke.t ->
      X.t ->
      Cstruct.t ->
      (Tls.Engine.state option, error) result io =
   fun tls queue flow raw ->
    match Tls.Engine.handle_tls tls raw with
    | `Fail (failure, `Response resp) ->
        Log.debug (fun m -> m "|- TLS state: Fail") ;
        flow_wr_opt flow (Some resp) >>? fun () -> err (`TLS failure)
    | `Ok (`Alert _alert, `Response resp, `Data data) ->
        Log.debug (fun m -> m "|- TLS state: Alert") ;
        queue_wr_opt queue data ;
        flow_wr_opt flow resp >>? fun () -> ok (Some tls)
    | `Ok (`Eof, `Response resp, `Data data) ->
        Log.debug (fun m -> m "|- TLS state: EOF") ;
        queue_wr_opt queue data ;
        flow_wr_opt flow resp >>? fun () -> ok None
    | `Ok (`Ok tls, `Response resp, `Data data) ->
        (* XXX(dinosaure): it seems that decoding TLS inputs can produce
           something bigger than expected. For example, decoding 4096 bytes
           can produce 4119 byte(s). *)
        Log.debug (fun m -> m "|- TLS state: Ok") ;
        queue_wr_opt queue data ;
        flow_wr_opt flow resp >>? fun () -> ok (Some tls)

  let handle_handshake :
      Tls.Engine.state ->
      (char, Bigarray.int8_unsigned_elt) Ke.t ->
      X.t ->
      Cstruct.t ->
      (Tls.Engine.state option, error) result io =
   fun tls queue flow raw0 ->
    let rec go tls raw1 =
      match Tls.Engine.can_handle_appdata tls with
      | true ->
          Log.debug (fun m -> m "Start to talk with TLS (handshake is done).") ;
          handle_tls tls queue flow raw1
      | false -> (
          assert (Tls.Engine.handshake_in_progress tls = true) ;
          Log.debug (fun m -> m "Process TLS handshake.") ;

          (* XXX(dinosaure): assertion, [Tls.Engine.handle_tls] consumes all
             bytes of [raw1] and [raw1] is physically a subset of [raw0] (or
             is [raw0]). we can re-use [raw0] for [Flow.recv] safely. *)
          match Tls.Engine.handle_tls tls raw1 with
          | `Ok (`Ok tls, `Response resp, `Data data) ->
              Log.debug (fun m ->
                  m "-- TLS state: OK (data: %d byte(s))"
                    (option_fold ~none:0 ~some:Cstruct.len data)) ;
              queue_wr_opt queue data ;
              flow_wr_opt flow resp >>? fun () ->
              if Tls.Engine.handshake_in_progress tls
              then (
                Log.debug (fun m -> m "<- Read the TLS flow") ;
                X.recv flow raw0 >>| reword_error flow_error >>? function
                | `End_of_flow ->
                    Log.warn (fun m ->
                        m "Got EOF from underlying connection while handshake.") ;
                    ok None
                | `Len len ->
                    Log.debug (fun m ->
                        let uid =
                          Hashtbl.hash
                            (Cstruct.to_string (Cstruct.sub raw0 0 len)) in
                        m
                          "<~ [%04x] Got %d bytes (handshake in progress: \
                           true)."
                          uid len) ;
                    go tls (Cstruct.sub raw0 0 len))
              else (
                Log.debug (fun m -> m "Handshake is done.") ;
                ok (Some tls))
          | `Ok (`Eof, `Response resp, `Data data) ->
              Log.debug (fun m -> m "-- TLS state: EOF") ;
              queue_wr_opt queue data ;
              flow_wr_opt flow resp >>? fun () -> ok None
          | `Fail (failure, `Response resp) ->
              Log.debug (fun m -> m "-- TLS state: Fail") ;
              flow_wr_opt flow (Some resp) >>? fun () -> err (`TLS failure)
          | `Ok (`Alert _alert, `Response resp, `Data data) ->
              Log.debug (fun m -> m "-- TLS state: Alert") ;
              queue_wr_opt queue data ;
              flow_wr_opt flow resp >>? fun () -> ok (Some tls)) in
    go tls raw0

  let blit src src_off dst dst_off len =
    let dst = Cstruct.to_bigarray dst in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let rec recv t raw =
    Log.debug (fun m -> m "<~ Start to receive.") ;
    if t.closed
    then err_closed
    else
      match Ke.N.peek t.queue with
      | [] -> (
          Log.debug (fun m -> m "<~ TLS queue is empty.") ;
          match t.tls with
          | None ->
              Log.debug (fun m -> m "<~ Connection is close.") ;
              ok `End_of_flow
          | Some tls -> (
              Log.debug (fun m -> m "<- Read the TLS flow.") ;
              X.recv t.flow t.raw >>| reword_error flow_error >>? function
              | `End_of_flow ->
                  Log.warn (fun m ->
                      m "<- Connection closed by underlying protocol.") ;
                  t.tls <- None ;
                  ok `End_of_flow
              | `Len len ->
                  let handle =
                    if Tls.Engine.handshake_in_progress tls
                    then handle_handshake tls t.queue t.flow
                    else handle_tls tls t.queue t.flow in
                  Log.debug (fun m ->
                      let uid =
                        Hashtbl.hash
                          (Cstruct.to_string (Cstruct.sub t.raw 0 len)) in
                      m "<~ [%04x] Got %d bytes (handshake in progress: %b)."
                        uid len
                        (Tls.Engine.handshake_in_progress tls)) ;
                  handle (Cstruct.sub t.raw 0 len) >>? fun tls ->
                  t.tls <- tls ;
                  recv t raw))
      | _ ->
          let max = Cstruct.len raw in
          let len = min (Ke.length t.queue) max in
          Ke.N.keep_exn t.queue ~blit ~length:Cstruct.len ~off:0 ~len raw ;
          Ke.N.shift_exn t.queue len ;
          ok (`Len len)

  let rec send t raw =
    Log.debug (fun m -> m "~> Start to send.") ;
    if t.closed
    then err_closed
    else
      match t.tls with
      | None -> err `Closed_by_peer
      | Some tls when Tls.Engine.can_handle_appdata tls -> (
          let raw = [ raw ] in
          match Tls.Engine.send_application_data tls raw with
          | Some (tls, resp) ->
              t.tls <- Some tls ;
              flow_wr_opt t.flow (Some resp) >>? fun () -> ok (Cstruct.lenv raw)
          | None -> ok (Cstruct.lenv raw))
      | Some tls -> (
          X.recv t.flow t.raw >>| reword_error flow_error >>? function
          | `End_of_flow ->
              Log.warn (fun m -> m "[-] Underlying flow already closed.") ;
              t.tls <- None ;
              err `Closed_by_peer
          | `Len len -> (
              let res =
                handle_handshake tls t.queue t.flow (Cstruct.sub t.raw 0 len)
              in
              res >>= function
              | Ok tls ->
                  t.tls <- tls ;
                  send t raw (* recall to finish handshake. *)
              | Error _ as e ->
                  Log.err (fun m -> m "[-] Got an error during handshake.") ;
                  IO.return e))

  let close t =
    Log.debug (fun m -> m "!- Asking to close the TLS connection") ;
    if not t.closed
    then (
      match t.tls with
      | None ->
          Log.debug (fun m ->
              m "!- TLS state already reached EOF, close the connection.") ;
          X.close t.flow >>| reword_error flow_error >>= fun res ->
          Log.debug (fun m -> m "!- Underlying flow properly closed.") ;
          t.closed <- true ;
          IO.return res
      | Some tls ->
          let _tls, resp = Tls.Engine.send_close_notify tls in
          t.tls <- None ;
          Log.debug (fun m -> m "!- Close the connection.") ;
          flow_wr_opt t.flow (Some resp) >>? fun () ->
          X.close t.flow >>| reword_error flow_error >>? fun () ->
          t.closed <- true ;
          ok ())
    else ok ()
end

module type S =
  Conduit.S with type input = Cstruct.t and type output = Cstruct.t

module Make (S : S) (X : S.PROTOCOL) = struct
  include Flow (S.IO) (X)

  type t = flow

  type endpoint = { endpoint : X.endpoint; tls : Tls.Config.client }

  let no_tls =
    let null ~host:_ _ = Ok None in
    Tls.Config.client ~authenticator:null ()

  let endpoint ?(tls = no_tls) endpoint = { tls; endpoint }

  let pp_endpoint ppf e =
    (* FIXME(samoht): do we want to print the client configuration
       too? Probably not as it contains secrets. *)
    X.pp_endpoint ppf e.endpoint

  type S.a_flow += Flow of { endpoint : endpoint; flow : t }

  let connect { endpoint; tls } =
    X.connect endpoint >>| reword_error flow_error >>? fun flow ->
    let raw = Cstruct.create 0x1000 in
    let queue = Ke.create ~capacity:0x1000 Bigarray.Char in
    let tls, buf = Tls.Engine.client tls in
    let rec go buf =
      Log.debug (fun m -> m "Start handshake.") ;
      X.send flow buf >>| reword_error flow_error >>? fun len ->
      let buf = Cstruct.shift buf len in
      if Cstruct.len buf = 0
      then ok { tls = Some tls; closed = false; raw; queue; flow }
      else go buf in
    go buf
end

module type SERVER =
  Conduit.SERVER with type input = Cstruct.t and type output = Cstruct.t

module Server (S : SERVER) (X : S.SERVICE) = struct
  include Flow
            (S.IO)
            (struct
              include X

              type t = X.flow
            end)

  type t = { service : X.t; tls : Tls.Config.server }

  type config = { tls : Tls.Config.server; config : X.config }

  let config ~tls config = { tls; config }

  let load_file filename =
    let ic = open_in filename in
    let ln = in_channel_length ic in
    let rs = Bytes.create ln in
    really_input ic rs 0 ln ;
    close_in ic ;
    Cstruct.of_bytes rs

  let config_files ~cert ~key t =
    let cert = load_file cert in
    let key = load_file key in
    match
      ( X509.Certificate.decode_pem_multiple cert,
        X509.Private_key.decode_pem key )
    with
    | Ok certs, Ok (`RSA key) ->
        let tls = Tls.Config.server ~certificates:(`Single (certs, key)) () in
        config ~tls t
    | _ -> Fmt.failwith "Invalid key or certificate"

  type S.a_flow += Flow of flow

  let init { tls; config } =
    X.init config >>| reword_error flow_error >>? fun service ->
    Log.info (fun m -> m "Start a TLS service.") ;
    ok { service; tls }

  let accept { service; tls } =
    X.accept service >>| reword_error flow_error >>? fun flow ->
    let tls = Tls.Engine.server tls in
    let raw = Cstruct.create 0x1000 in
    let queue = Ke.create ~capacity:0x1000 Bigarray.Char in
    Log.info (fun m -> m "A TLS flow is coming.") ;
    ok { tls = Some tls; closed = false; raw; queue; flow }

  let stop { service; _ } = X.stop service >>| reword_error flow_error
end
