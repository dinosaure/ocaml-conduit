open Async

let ok x = return (Ok x)

let err e = return (Error (`Core (Core.Error.of_exn e)))

module IO = struct
  type +'a t = 'a Deferred.t

  let bind x f = Deferred.bind x ~f

  let return x = Deferred.return x
end

include Conduit.Make (IO) (Cstruct) (Cstruct)

let reader_and_writer_of_flow flow =
  let open Async in
  let recv flow writer =
    let tmp = Cstruct.create 0x1000 in
    let rec loop () =
      recv flow tmp >>= function
      | Ok (`Len len) ->
          Pipe.write writer (Cstruct.to_string (Cstruct.sub tmp 0 len)) >>= loop
      | Ok `End_of_flow ->
          Pipe.close writer ;
          return ()
      | Error err -> Fmt.failwith "%a" pp_error err in
    loop () in
  let send flow reader =
    let rec loop () =
      Pipe.read reader >>= function
      | `Eof -> return ()
      | `Ok v ->
          let rec go tmp =
            if Cstruct.len tmp = 0
            then return ()
            else
              send flow tmp >>= function
              | Ok shift -> go (Cstruct.shift tmp shift)
              | Error err -> Fmt.failwith "%a" pp_error err in
          go (Cstruct.of_string v) >>= loop in
    loop () in
  let preader = Pipe.create_reader ~close_on_exception:true (recv flow) in
  let pwriter = Pipe.create_writer (send flow) in
  Reader.of_pipe (Core.Info.of_string "reader") preader >>= fun reader ->
  Writer.of_pipe (Core.Info.of_string "writer") pwriter >>= fun (writer, _) ->
  return (reader, writer)

module TCP = struct
  type endpoint =
    | Inet of Socket.Address.Inet.t
    | Unix of Socket.Address.Unix.t

  let pp_endpoint ppf = function
    | Inet s -> Sexp.pp ppf (Socket.Address.Inet.sexp_of_t s)
    | Unix s -> Sexp.pp ppf (Socket.Address.Unix.sexp_of_t s)

  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Deferred.t

  type t =
    | Socket : {
        address : [< Socket.Address.t ];
        socket : ([ `Active ], [< Socket.Address.t ]) Socket.t;
        reader : Reader.t;
        writer : Writer.t;
      }
        -> t

  type a_flow += Flow of { endpoint : endpoint; flow : t }

  let address (Socket { address; _ }) =
    match address with #Socket.Address.t as addr -> addr

  let reader (Socket { reader; _ }) = reader

  let writer (Socket { writer; _ }) = writer

  type error = [ `Core of Core.Error.t | Conduit.error ]

  let pp_error ppf = function
    | `Core e -> Core.Error.pp ppf e
    | #Conduit.error as e -> Conduit.pp_error ppf e

  let connect edn =
    let connect = function
      | Inet address ->
          Tcp.connect (Tcp.Where_to_connect.of_inet_address address)
          >>| fun (socket, reader, writer) ->
          Socket { address; socket; reader; writer }
      | Unix address ->
          Tcp.connect (Tcp.Where_to_connect.of_unix_address address)
          >>| fun (socket, reader, writer) ->
          Socket { address; socket; reader; writer } in
    Monitor.try_with (fun () -> connect edn) >>= function
    | Ok _ as v -> return v
    | Error e -> err e

  let of_cstruct raw =
    let { Cstruct.buffer; off; len } = raw in
    Core.Bigsubstring.create ~pos:off ~len buffer

  (* XXX(dinosaure): as [lwt] and seems required for [conduit-tls],
     [recv] wants to read as much as possible. Due to underlying
     non-blocking socket, even if we reached [`Eof], we must retry to
     read until we have something or the underlying socket was
     closed. *)
  let rec recv (Socket { socket; reader; _ } as flow) raw =
    Monitor.try_with (fun () ->
        Reader.read_bigsubstring reader (of_cstruct raw))
    >>= function
    | Error e -> Reader.close reader >>= fun () -> err e
    | Ok (`Ok n) -> ok (`Len n)
    | Ok `Eof -> (
        Fd.ready_to (Socket.fd socket) `Read >>= function
        | `Bad_fd | `Closed -> ok `End_of_flow
        | `Ready -> Scheduler.yield () >>= fun () -> recv flow raw)

  let send (Socket { writer; _ }) raw =
    Writer.write_bigsubstring writer (of_cstruct raw) ;
    Writer.flushed writer >>= fun () -> ok (Cstruct.len raw)

  let close (Socket { socket; reader; writer; _ }) =
    (* XXX(dinosaure): we should be protected against the double-close. *)
    if Reader.is_closed reader
       && Writer.is_closed writer
       && Fd.is_closed (Socket.fd socket)
    then ok ()
    else (
      Socket.shutdown socket `Both ;
      Reader.close reader >>= fun () -> Writer.close writer >>= ok)

  let resolve ~port domain_name =
    Monitor.try_with (fun () ->
        Unix.Inet_addr.of_string_or_getbyname
          (Domain_name.to_string domain_name))
    >>= function
    | Ok inet_addr ->
        let inet_addr = Socket.Address.Inet.create inet_addr ~port in
        return (Some (Inet inet_addr))
    | _ -> return None
end

let tcp = protocol "tpc" (module TCP)

module Server = struct
  module IO = IO
  include Conduit.Server (IO) (Cstruct) (Cstruct)

  let ( >>? ) x f = Deferred.Result.bind x ~f

  let ok x = Deferred.ok (return x)

  let serve ~handler service cfg =
    let c = Condition.create () in
    let main =
      init service cfg >>= function
      | Error err -> Fmt.failwith "%a" pp_error err
      | Ok t -> (
          let rec loop () =
            let close = Condition.wait c >>| fun () -> Ok `Stop in
            let accept = accept service t >>? fun flow -> ok (`Flow flow) in
            Deferred.any [ close; accept ] >>= function
            | Ok (`Flow flow) ->
                don't_wait_for (handler service flow) ;
                Scheduler.yield () >>= fun () -> (loop [@tailcall]) ()
            | Ok `Stop -> stop service t
            | Error err0 -> (
                stop service t >>= function
                | Ok () -> return (Error err0)
                | Error _err1 -> return (Error err0)) in
          loop () >>= function
          | Ok () -> return ()
          | Error err -> Fmt.failwith "%a" pp_error err) in
    (c, main)

  module TCP = struct
    type config = Listen : ('a, 'b) Tcp.Where_to_listen.t -> config

    let config x = Listen x

    type +'a io = 'a Deferred.t

    type input = Cstruct.t

    type output = Cstruct.t

    type flow = TCP.t

    type error =
      [ `Exn of [ `Make | `Accept ] * exn
      | `Core of Core.Error.t
      | Conduit.error ]

    let pp_error ppf = function
      | `Core e -> Core.Error.pp ppf e
      | `Exn (`Make, exn) ->
          Format.fprintf ppf "Got an exception while making socket: %s"
            (Printexc.to_string exn)
      | `Exn (`Accept, exn) ->
          Format.fprintf ppf "Got an exception while accepting socket: %s"
            (Printexc.to_string exn)
      | #Conduit.error as e -> Conduit.pp_error ppf e

    type t =
      | Server :
          ([ `Passive ], ([< Socket.Address.t ] as 'a)) Socket.t * 'a
          -> t

    type a_flow += Flow of flow

    let close_socket_on_error ~process socket ~f =
      Monitor.try_with f >>| function
      | Ok v -> Ok v
      | Error exn ->
          don't_wait_for (Unix.close (Socket.fd socket)) ;
          Error (`Exn (process, exn))

    type socket_type =
      | Socket_type :
          ([< Socket.Address.t ] as 'a) Socket.Type.t * 'a
          -> socket_type

    let send = TCP.send

    let recv = TCP.recv

    let close = TCP.close

    let init (Listen where_to_listen) =
      let (Socket_type (socket_type, addr)) =
        match Tcp.Where_to_listen.address where_to_listen with
        | `Inet _ as addr -> Socket_type (Socket.Type.tcp, addr)
        | `Unix _ as addr -> Socket_type (Socket.Type.unix, addr) in
      let socket = Socket.create socket_type in
      let f () = Socket.bind socket addr >>| Socket.listen in
      close_socket_on_error ~process:`Make socket ~f >>? fun socket ->
      ok (Server (socket, addr))

    let accept (Server (socket, _)) =
      Socket.accept socket >>= function
      | `Ok (socket, address) ->
          let reader = Reader.create (Socket.fd socket) in
          let writer = Writer.create (Socket.fd socket) in
          let flow = TCP.Socket { socket; reader; writer; address } in
          ok flow
      | `Socket_closed -> return (Error `Closed)

    let stop (Server (socket, _)) = Fd.close (Socket.fd socket) >>= ok
  end

  let tcp = service "tcp" (module TCP)
end
