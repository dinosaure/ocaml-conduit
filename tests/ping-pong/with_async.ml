open Async
module P = Ping_pong.Make (Conduit_async)

let resolvers =
  let tcp = Conduit_async.TCP.resolve ~port:4000 in
  let tls = Conduit_async_tls.TCP.resolve ~port:8000 () in
  let ssl = Conduit_async_ssl.TCP.resolve ~port:6000 ?verify:None () in
  let open Conduit_async.Resolver in
  empty
  |> add ~priority:20 Conduit_async.tcp tcp
  |> add ~priority:10 Conduit_async_tls.tcp tls
  |> add ~priority:10 Conduit_async_ssl.tcp ssl

let run_with ~service cfg clients =
  let serve () =
    Conduit_async.Server.serve
      ~handler:(fun s f -> P.transmission (Conduit_async.Server.flow s f))
      service cfg in
  let stop, server = serve () in
  let clients =
    let clients = List.map (P.client ~resolvers) clients in
    Deferred.all_unit clients >>| fun () -> Condition.broadcast stop () in
  don't_wait_for (Deferred.all_unit [ server; clients ] >>| fun () -> shutdown 0) ;
  Core.never_returns (Scheduler.go ())

let run_with_tcp clients =
  let open Conduit_async.Server in
  let cfg = TCP.config (Tcp.Where_to_listen.of_port 5000) in
  run_with ~service:tcp cfg clients

let run_with_ssl cert key clients =
  let open Conduit_async_ssl.Server in
  let cfg =
    TCP.config ~crt_file:cert ~key_file:key (Tcp.Where_to_listen.of_port 7000)
  in
  run_with ~service:tcp cfg clients

let run_with_tls cert key clients =
  let open Conduit_async_tls.Server in
  let cfg = TCP.config_files ~cert ~key (Tcp.Where_to_listen.of_port 9000) in
  run_with ~service:tcp cfg clients

let () =
  match Array.to_list Stdlib.Sys.argv with
  | _ :: "--with-ssl" :: cert :: key :: clients -> run_with_ssl cert key clients
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: clients -> run_with_tcp clients
  | [] -> assert false
