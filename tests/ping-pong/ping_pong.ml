module Make
    (C : Conduit.S with type input = Cstruct.t and type output = Cstruct.t) =
struct
  module Flow = C

  let ( >>= ) = C.IO.bind

  let return = C.IO.return

  let ok x = return (Ok x)

  let err x = return (Error x)

  let () = Mirage_crypto_rng_unix.initialize ()

  let () = Printexc.record_backtrace true

  let () = Ssl.init ()

  let ( >>? ) x f = x >>= function Ok x -> f x | Error _ as err -> return err

  (* XXX(samoht): add Conduit.domain_name_exn *)
  let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

  (* Server part *)

  (* XXX(samoht): add in Conduit directly *)
  let getline_ke queue =
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
      match getline_ke queue with
      | Some line -> ok (`Line line)
      | None -> (
          Flow.recv flow tmp >>? function
          | `End_of_flow -> ok `Close
          | `Len len ->
              Ke.Rke.N.push queue ~blit ~length:Cstruct.len ~off:0 ~len tmp ;
              go ()) in
    go ()

  let pong = Cstruct.of_string "pong\n"

  let ping = Cstruct.of_string "ping\n"

  let transmission flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    let rec go () =
      getline queue flow >>= function
      | Ok `Close | Error _ -> Flow.close flow
      | Ok (`Line "ping") ->
          Fmt.epr "[!] received ping.\n%!" ;
          Flow.send flow pong >>? fun _ -> go ()
      | Ok (`Line "pong") ->
          Fmt.epr "[!] received pong.\n%!" ;
          Flow.send flow ping >>? fun _ -> go ()
      | Ok (`Line line) ->
          Fmt.epr "[!] received %S.\n%!" line ;
          Flow.send flow (Cstruct.of_string (line ^ "\n")) >>? fun _ ->
          Flow.close flow in
    go () >>= function
    | Error err -> Fmt.failwith "%a" Flow.pp_error err
    | Ok () -> return ()

  (* Client part *)

  let connect ~resolvers domain_name responses =
    Flow.resolve resolvers domain_name >>? fun flow ->
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    let rec go = function
      | [] -> Flow.close flow
      | line :: rest -> (
          Flow.send flow (Cstruct.of_string (line ^ "\n")) >>? fun _ ->
          getline queue flow >>? function
          | `Close -> Flow.close flow
          | `Line "pong" -> go rest
          | `Line _ -> Flow.close flow) in
    go responses

  let client ~resolvers filename =
    let rec go acc ic =
      match input_line ic with
      | line -> go (line :: acc) ic
      | exception End_of_file -> List.rev acc in
    let ic = open_in filename in
    let responses = go [] ic in
    close_in ic ;
    connect ~resolvers localhost responses >>= function
    | Ok () -> return ()
    | Error `Closed -> return ()
    | Error err ->
        Fmt.epr "client: %a.\n%!" Flow.pp_error err ;
        return ()
end
