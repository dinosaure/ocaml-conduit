open Rresult
open Lwt.Infix

let () = Mirage_crypto_rng_unix.initialize ()

let () = Printexc.record_backtrace true

let () = Ssl.init ()

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Lwt.return err

let failwith fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

(* Server part *)

let getline queue =
  let exists ~predicate queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter
      (fun chr ->
        if predicate chr then res := !pos ;
        incr pos)
      queue ;
    if !res = -1 then None else Some !res in
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
  match exists ~predicate:(( = ) '\n') queue with
  | Some pos ->
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
      Ke.Rke.N.shift_exn queue (pos + 1) ;
      Some (Bytes.unsafe_to_string tmp)
  | None -> None

let getline queue flow =
  let tmp = Cstruct.create 0x1000 in
  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len in
  let rec go () =
    match getline queue with
    | Some line -> Lwt.return_ok (`Line line)
    | None -> (
        Conduit_lwt.recv flow tmp >>? function
        | `End_of_input -> Lwt.return_ok `Close
        | `Input len ->
            Ke.Rke.N.push queue ~blit ~length:Cstruct.len ~off:0 ~len tmp ;
            go ()) in
  go ()

let pong = Cstruct.of_string "pong\n"

let ping = Cstruct.of_string "ping\n"

let transmission flow =
  let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let rec go () =
    getline queue flow >>= function
    | Ok `Close | Error _ -> Conduit_lwt.close flow
    | Ok (`Line "ping") ->
        Fmt.epr "[!] received ping.\n%!" ;
        Conduit_lwt.send flow pong >>? fun _ -> go ()
    | Ok (`Line "pong") ->
        Fmt.epr "[!] received pong.\n%!" ;
        Conduit_lwt.send flow ping >>? fun _ -> go ()
    | Ok (`Line line) ->
        Fmt.epr "[!] received %S.\n%!" line ;
        Conduit_lwt.send flow (Cstruct.of_string (line ^ "\n")) >>? fun _ ->
        Conduit_lwt.close flow in
  go () >>= function
  | Error err -> failwith "%a" Conduit_lwt.pp_error err
  | Ok () -> Lwt.return ()

let server :
    type cfg master flow.
    key:cfg Conduit_lwt.key ->
    cfg ->
    service:(master * flow) Conduit_lwt.Witness.service ->
    unit Lwt_condition.t * unit Lwt.t =
 fun ~key cfg ~service ->
  Conduit_lwt_unix.serve_with_handler
    ~handler:(fun protocol flow ->
      transmission (Conduit_lwt_unix.abstract protocol flow))
    ~key ~service cfg

(* Client part *)

let client ?key ~resolvers domain_name responses =
  Conduit_lwt.flow ?key resolvers domain_name >>? fun flow ->
  let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let rec go = function
    | [] -> Conduit_lwt.close flow
    | line :: rest -> (
        Conduit_lwt.send flow (Cstruct.of_string (line ^ "\n")) >>? fun _ ->
        getline queue flow >>? function
        | `Close -> Conduit_lwt.close flow
        | `Line "pong" -> go rest
        | `Line _ -> Conduit_lwt.close flow) in
  go responses

let client ?key ~resolvers filename =
  let rec go acc ic =
    match input_line ic with
    | line -> go (line :: acc) ic
    | exception End_of_file -> List.rev acc in
  let ic = open_in filename in
  let responses = go [] ic in
  close_in ic ;
  client ?key ~resolvers localhost responses >>= function
  | Ok () -> Lwt.return_unit
  | Error `Closed_by_peer -> Lwt.return_unit
  | Error (#Conduit_lwt.error as err) ->
      Fmt.epr "client: %a.\n%!" Conduit_lwt.pp_error err ;
      Lwt.return_unit

(* Composition *)

let tls_endpoint, tls_protocol, tls_configuration, tls_service =
  let open Conduit_lwt_unix_tls.TCP in
  (endpoint, protocol, configuration, service)

let ssl_endpoint, ssl_protocol, ssl_configuration, ssl_service =
  let open Conduit_lwt_unix_ssl.TCP in
  (endpoint, protocol, configuration, service)

(* Resolution *)

let resolve_ping_pong = Conduit_lwt_unix_tcp.resolv_conf ~port:4000

let resolve_tls_ping_pong =
  let null ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  Conduit_lwt_unix_tls.TCP.resolv_conf ~port:8000 ~config

let resolve_ssl_ping_pong =
  let context = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  Conduit_lwt_unix_ssl.TCP.resolv_conf ~port:6000 ~context ?verify:None

let resolvers =
  Conduit.empty
  |> Conduit_lwt.register_resolver ~priority:20
       ~key:Conduit_lwt_unix_tcp.endpoint resolve_ping_pong
  |> Conduit_lwt.register_resolver ~priority:10 ~key:tls_endpoint
       resolve_tls_ping_pong
  |> Conduit_lwt.register_resolver ~priority:10 ~key:ssl_endpoint
       resolve_ssl_ping_pong

(* Run *)

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Cstruct.of_bytes rs

let config cert key =
  let cert = load_file cert in
  let key = load_file key in
  match
    (X509.Certificate.decode_pem_multiple cert, X509.Private_key.decode_pem key)
  with
  | Ok certs, Ok (`RSA key) ->
      Tls.Config.server ~certificates:(`Single (certs, key)) ()
  | _ -> Fmt.failwith "Invalid key or certificate"

let run_with :
    type edn cfg master flow.
    ?key_edn:edn Conduit_lwt.key ->
    key_cfg:cfg Conduit_lwt.key ->
    cfg ->
    service:(master * flow) Conduit_lwt.Witness.service ->
    string list ->
    unit =
 fun ?key_edn ~key_cfg cfg ~service clients ->
  let stop, server = server ~key:key_cfg cfg ~service in
  let clients = List.map (client ?key:key_edn ~resolvers) clients in
  let clients =
    Lwt.join clients >>= fun () ->
    Lwt_condition.broadcast stop () ;
    Lwt.return_unit in
  Lwt_main.run (Lwt.join [ server; clients ])

let run_with_tcp clients =
  run_with ~key_cfg:Conduit_lwt_unix_tcp.configuration
    {
      Conduit_lwt_unix_tcp.sockaddr =
        Unix.ADDR_INET (Unix.inet_addr_loopback, 4000);
      capacity = 40;
    }
    ~service:Conduit_lwt_unix_tcp.service clients

let run_with_ssl cert key clients =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Server_context in
  Ssl.use_certificate ctx cert key ;
  run_with ~key_cfg:ssl_configuration
    ( ctx,
      {
        Conduit_lwt_unix_tcp.sockaddr =
          Unix.ADDR_INET (Unix.inet_addr_loopback, 6000);
        capacity = 40;
      } )
    ~service:ssl_service clients

let run_with_tls cert key clients =
  let ctx = config cert key in
  run_with ~key_edn:tls_endpoint ~key_cfg:tls_configuration
    ( {
        Conduit_lwt_unix_tcp.sockaddr =
          Unix.ADDR_INET (Unix.inet_addr_loopback, 8000);
        capacity = 40;
      },
      ctx )
    ~service:tls_service clients

let () =
  match Array.to_list Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: "--with-ssl" :: cert :: key :: clients -> run_with_ssl cert key clients
  | _ :: clients -> run_with_tcp clients
  | _ -> Fmt.epr "%s [--with-tls|--with-ssl] filename...\n%!" Sys.argv.(0)
