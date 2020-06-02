open Lwt.Infix
module P = Ping_pong.Make (Conduit_lwt)

let resolvers =
  let resolve_tcp = Conduit_lwt.TCP.resolve ~port:4000 in
  let resolve_tls = Conduit_lwt_tls.TCP.resolve ~port:8000 () in
  let resolve_ssl = Conduit_lwt_ssl.TCP.resolve ~port:6000 ?verify:None () in
  let open Conduit_lwt.Resolver in
  empty
  |> add ~priority:20 Conduit_lwt.tcp resolve_tcp
  |> add ~priority:10 Conduit_lwt_tls.tcp resolve_tls
  |> add ~priority:10 Conduit_lwt_ssl.tcp resolve_ssl

(* Run *)

let run_with ~service cfg clients =
  let serve () =
    Conduit_lwt.Server.serve
      ~handler:(fun s f -> P.transmission (Conduit_lwt.Server.flow s f))
      service cfg in
  let stop, server = serve () in
  let clients = List.map (P.client ~resolvers) clients in
  let clients = Lwt.join clients >|= fun () -> Lwt_condition.broadcast stop () in
  Lwt_main.run (Lwt.join [ server; clients ])

let run_with_tcp clients =
  let open Conduit_lwt.Server.TCP in
  let cfg = config (Unix.ADDR_INET (Unix.inet_addr_loopback, 4000)) in
  run_with cfg ~service:Conduit_lwt.Server.tcp clients

let run_with_ssl cert key clients =
  let open Conduit_lwt_ssl.Server.TCP in
  let cfg =
    config_files ~cert ~key (Unix.ADDR_INET (Unix.inet_addr_loopback, 6000))
  in
  run_with cfg ~service:Conduit_lwt_ssl.Server.tcp clients

let run_with_tls cert key clients =
  let open Conduit_lwt_tls.Server.TCP in
  let cfg =
    config_files ~cert ~key (Unix.ADDR_INET (Unix.inet_addr_loopback, 8000))
  in
  run_with cfg ~service:Conduit_lwt_tls.Server.tcp clients

let () =
  match Array.to_list Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: "--with-ssl" :: cert :: key :: clients -> run_with_ssl cert key clients
  | _ :: clients -> run_with_tcp clients
  | _ -> Fmt.epr "%s [--with-tls|--with-ssl] filename...\n%!" Sys.argv.(0)
