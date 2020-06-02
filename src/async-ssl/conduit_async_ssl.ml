open Async_ssl
open Async
open Core

let ok x = return (Ok x)

let ( >>? ) x f = x >>= function Ok x -> f x | Error _ as err -> return err

let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

let teardown_connection reader writer =
  Writer.close ~force_close:Clock.(after (sec 30.)) writer >>= fun () ->
  Reader.close reader

let reader_writer_pipes reader writer =
  let reader_pipe_reader, reader_pipe_writer = Pipe.create () in
  let writer_pipe = Writer.pipe writer in
  upon (Reader.transfer reader reader_pipe_writer) (fun () ->
      teardown_connection reader writer >>> fun () ->
      Pipe.close reader_pipe_writer) ;
  upon (Pipe.closed writer_pipe) (fun () ->
      Deferred.choose
        [
          Deferred.choice Clock.(after (sec 30.)) (fun () -> ());
          Deferred.choice (Pipe.downstream_flushed writer_pipe)
            (fun (_ : Pipe.Flushed_result.t) -> ());
        ]
      >>> fun () -> don't_wait_for (teardown_connection reader writer)) ;
  (reader_pipe_reader, writer_pipe)

let reader_writer_of_pipes app_rd app_wr =
  Reader.of_pipe (Info.of_string "async-conduit-ssl-reader") app_rd
  >>= fun app_reader ->
  upon (Reader.close_finished app_reader) (fun () -> Pipe.close_read app_rd) ;
  Writer.of_pipe (Info.of_string "async-conduit-ssl-writer") app_wr
  >>| fun (app_writer, _) ->
  Writer.set_raise_when_consumer_leaves app_writer false ;
  (app_reader, app_writer)

type context = {
  version : Ssl.Version.t option;
  options : Ssl.Opt.t list option;
  name : string option;
  hostname : string option;
  allowed_ciphers :
    [ `Only of string list | `Openssl_default | `Secure ] option;
  ca_file : string option;
  ca_path : string option;
  crt_file : string option;
  key_file : string option;
  session : Ssl.Session.t option;
  verify_modes : Verify_mode.t list option;
  verify : (Ssl.Connection.t -> bool Deferred.t) option;
}

module TCP = struct
  open Conduit_async

  type t = {
    connection : Ssl.Connection.t;
    reader : Reader.t;
    writer : Writer.t;
    underlying : TCP.t;
  }

  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Deferred.t

  type endpoint = { ssl : context; endpoint : TCP.endpoint }

  let endpoint ?version ?options ?name ?hostname ?allowed_ciphers ?ca_file
      ?ca_path ?crt_file ?key_file ?session ?verify_modes ?verify endpoint =
    {
      ssl =
        {
          version;
          options;
          name;
          hostname;
          allowed_ciphers;
          ca_file;
          ca_path;
          crt_file;
          key_file;
          session;
          verify_modes;
          verify;
        };
      endpoint;
    }

  type a_flow += Flow of { endpoint : endpoint; flow : t }

  let pp_endpoint ppf e = TCP.pp_endpoint ppf e.endpoint

  exception Invalid_connection

  type error = [ Conduit.error | `Core of Core.Error.t | `TCP of TCP.error ]

  let pp_error ppf (e : error) =
    match e with
    | #Conduit.error as e -> Conduit.pp_error ppf e
    | `Core err -> Core.Error.pp ppf err
    | `TCP err -> TCP.pp_error ppf err

  let connect
      {
        ssl =
          {
            version;
            options;
            name;
            hostname;
            allowed_ciphers;
            ca_file;
            ca_path;
            crt_file;
            key_file;
            session;
            verify_modes;
            verify;
          };
        endpoint;
      } =
    TCP.connect endpoint >>| reword_error (fun err -> `TCP err)
    >>? fun underlying ->
    let reader = TCP.reader underlying in
    let writer = TCP.writer underlying in

    let net_to_ssl, ssl_to_net = reader_writer_pipes reader writer in
    let app_to_ssl, app_writer = Pipe.create () in
    let app_reader, ssl_to_app = Pipe.create () in
    let verify_connection =
      match verify with None -> Fn.const (return true) | Some verify -> verify
    in
    Monitor.try_with_join_or_error (fun () ->
        Ssl.client ?version ?options ?name ?hostname ?allowed_ciphers ?ca_file
          ?ca_path ?crt_file ?key_file ?session ?verify_modes ~app_to_ssl
          ~ssl_to_app ~net_to_ssl ~ssl_to_net ())
    >>| reword_error (fun err -> `Core err)
    >>= function
    | Error _ as e -> teardown_connection reader writer >>= fun () -> return e
    | Ok conn -> (
        verify_connection conn >>= function
        | true ->
            reader_writer_of_pipes app_reader app_writer
            >>= fun (app_reader, app_writer) ->
            ok
              {
                connection = conn;
                reader = app_reader;
                writer = app_writer;
                underlying;
              }
        | false ->
            teardown_connection reader writer >>| fun () ->
            Error (`Core (Core.Error.of_exn Invalid_connection)))

  let of_cstruct raw =
    let { Cstruct.buffer; off; len } = raw in
    Core.Bigsubstring.create ~pos:off ~len buffer

  let recv { reader; _ } raw =
    Reader.read_bigsubstring reader (of_cstruct raw) >>| function
    | `Eof -> Ok `End_of_flow
    | `Ok n -> Ok (`Len n)

  let send { writer; _ } raw =
    Writer.write_bigsubstring writer (of_cstruct raw) ;
    ok (Cstruct.len raw)

  let close { reader; writer; _ } =
    Reader.close reader >>= fun () ->
    Writer.close writer >>| fun () -> Ok ()

  let resolve ?(port = 443) ?version ?options ?name ?hostname ?allowed_ciphers
      ?ca_file ?ca_path ?crt_file ?key_file ?session ?verify_modes ?verify ()
      domain_name =
    TCP.resolve ~port domain_name >>| function
    | Some endpoint ->
        let ssl =
          {
            version;
            options;
            name;
            hostname;
            allowed_ciphers;
            ca_file;
            ca_path;
            crt_file;
            key_file;
            session;
            verify_modes;
            verify;
          } in
        Some { ssl; endpoint }
    | None -> None
end

let tcp = Conduit_async.protocol "ssl+tcp" (module TCP)

module Server = struct
  module Client = TCP

  module TCP = struct
    module Flow = TCP
    open Conduit_async.Server

    type +'a io = 'a Deferred.t

    type input = Cstruct.t

    type output = Cstruct.t

    type error =
      [ Conduit.error
      | `TCP of TCP.error
      | `Core of Core.Error.t
      | `Missing_crt_or_key ]

    let pp_error ppf (e : error) =
      match e with
      | #Conduit.error as e -> Conduit.pp_error ppf e
      | `TCP err -> TCP.pp_error ppf err
      | `Core err -> Core.Error.pp ppf err
      | `Missing_crt_or_key ->
          Format.fprintf ppf "Missing crt of key values into context"

    let config ?version ?options ?name ?hostname ?allowed_ciphers ?ca_file
        ?ca_path ?crt_file ?key_file ?session ?verify_modes ?verify x =
      let ssl =
        {
          version;
          options;
          name;
          hostname;
          allowed_ciphers;
          ca_file;
          ca_path;
          crt_file;
          key_file;
          session;
          verify_modes;
          verify;
        } in
      (ssl, TCP.config x)

    type config = context * TCP.config

    type t = context * TCP.t

    type flow = Flow.t

    type a_flow += Flow of flow

    let recv = Flow.recv

    let send = Flow.send

    let close = Flow.close

    let init ((context, edn) : config) : (t, error) result io =
      match (context.crt_file, context.key_file) with
      | None, None | Some _, None | None, Some _ ->
          return (Error `Missing_crt_or_key)
      | _ -> (
          TCP.init edn >>= function
          | Ok t -> ok (context, t)
          | Error err -> return (Error (`TCP err)))

    let accept
        ( {
            version;
            options;
            name;
            allowed_ciphers;
            ca_file;
            ca_path;
            crt_file;
            key_file;
            verify_modes;
            _;
          },
          service ) =
      TCP.accept service >>= function
      | Error err -> return (Error (`TCP err))
      | Ok flow -> (
          let crt_file, key_file =
            match (crt_file, key_file) with
            | Some crt_file, Some key_file -> (crt_file, key_file)
            | _ -> assert false in
          let reader = Conduit_async.TCP.reader flow in
          let writer = Conduit_async.TCP.writer flow in
          let net_to_ssl, ssl_to_net = reader_writer_pipes reader writer in
          let app_to_ssl, app_writer = Pipe.create () in
          let app_reader, ssl_to_app = Pipe.create () in
          Ssl.server ?version ?options ?name ?allowed_ciphers ?ca_file ?ca_path
            ~crt_file ~key_file ?verify_modes ~app_to_ssl ~ssl_to_app
            ~net_to_ssl ~ssl_to_net ()
          >>= function
          | Error error ->
              teardown_connection reader writer >>| fun () ->
              Error (`Core error)
          | Ok conn ->
              reader_writer_of_pipes app_reader app_writer
              >>| fun (app_reader, app_writer) ->
              Ok
                {
                  Flow.underlying = flow;
                  reader = app_reader;
                  writer = app_writer;
                  connection = conn;
                })

    let stop (_, t) =
      TCP.stop t >>| function Error err -> Error (`TCP err) | Ok _ as v -> v
  end

  let tcp = Conduit_async.Server.service "ssl+tcp" (module TCP)
end
