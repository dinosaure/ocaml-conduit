open Conduit_lwt
open Lwt.Infix

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Lwt.return err

let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

module TCP = struct
  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Lwt.t

  type error =
    [ Conduit.error | `Flow of TCP.error | `Verification_error of string ]

  let pp_error ppf = function
    | `Flow err -> TCP.pp_error ppf err
    | `Verification_error err -> Fmt.pf ppf "%s" err
    | #Conduit.error as e -> Conduit.pp_error ppf e

  type t = Lwt_ssl.socket

  let recv socket raw =
    let { Cstruct.buffer; off; len } = raw in
    Lwt_ssl.read_bytes socket buffer off len >>= function
    | 0 -> Lwt.return_ok `End_of_flow
    | len -> Lwt.return_ok (`Len len)

  let send socket raw =
    let { Cstruct.buffer; off; len } = raw in
    Lwt_ssl.write_bytes socket buffer off len >>= fun len -> Lwt.return_ok len

  let close socket =
    (* FIXME(samoht): check double close semantics. *)
    Lwt_ssl.ssl_shutdown socket >>= fun () ->
    Lwt_ssl.close socket >>= fun () -> Lwt.return_ok ()

  type verify =
    Ssl.context ->
    TCP.t ->
    (Lwt_ssl.socket, [ `Verification_error of string ]) result Lwt.t

  type endpoint = {
    ssl : Ssl.context;
    endpoint : TCP.endpoint;
    verify : verify;
  }

  let no_ssl = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context

  let endpoint ~file_descr ?(ssl = no_ssl) ?verify endpoint =
    let verify =
      match verify with
      | Some verify -> verify
      | None ->
          let verify ctx flow =
            let file_descr = file_descr flow in
            Lwt_ssl.ssl_connect file_descr ctx >>= fun v -> Lwt.return_ok v
          in
          verify in
    { ssl; endpoint; verify }

  let pp_endpoint ppf e = TCP.pp_endpoint ppf e.endpoint

  let connect { ssl; endpoint; verify } =
    TCP.connect endpoint >|= reword_error (fun err -> `Flow err) >>? fun flow ->
    verify ssl flow >>= function
    | Ok _ as v -> Lwt.return v
    | Error (`Verification_error _ as err) -> Lwt.return (Error err)

  let endpoint = endpoint ~file_descr:TCP.file_descr

  let resolve ?(port = 443) ?verify ?(ssl = no_ssl) () domain_name =
    Conduit_lwt.TCP.resolve ~port domain_name >|= function
    | Some edn -> Some (endpoint ~ssl ?verify edn)
    | None -> None

  type a_flow += Flow of { endpoint : endpoint; flow : t }
end

let tcp = protocol "ssl+tcp" (module TCP)

module Server = struct
  module Client = TCP
  open Conduit_lwt.Server

  module TCP = struct
    include Client

    type config = { config : TCP.config; ssl : Ssl.context }

    let config ~ssl ?capacity config =
      let config = TCP.config ?capacity config in
      { config; ssl }

    let config_files ~cert ~key ?capacity t =
      let ssl = Ssl.create_context Ssl.TLSv1_2 Ssl.Server_context in
      Ssl.use_certificate ssl cert key ;
      config ~ssl ?capacity t

    type t = { service : TCP.t; ssl : Ssl.context; mutable closed : bool }

    type flow = Lwt_ssl.socket

    type a_flow += Flow of flow

    let init { ssl; config } =
      TCP.init config >|= reword_error (fun err -> `Flow err) >>? fun service ->
      Lwt.return_ok { service; ssl; closed = false }

    let accept { service; ssl; closed } =
      if closed
      then Lwt.return (Error `Closed)
      else
        TCP.accept service >|= reword_error (fun err -> `Flow err)
        >>? fun flow ->
        let accept () = Lwt_ssl.ssl_accept (TCP.file_descr flow) ssl in
        let process socket = Lwt.return_ok socket in
        let error exn =
          Lwt_unix.close (TCP.file_descr flow) >>= fun () -> Lwt.fail exn in
        Lwt.try_bind accept process error

    let stop t =
      if t.closed
      then Lwt.return (Ok ())
      else (
        t.closed <- true ;
        TCP.stop t.service >|= reword_error (fun err -> `Flow err))
  end

  let tcp = service "ssl+tcp" (module TCP)
end
