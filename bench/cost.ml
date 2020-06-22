external tick : unit -> (int64[@unboxed]) = "none" "get_tick" [@@noalloc]

module None = struct
  type +'a t = 'a 

  let bind x f = f x
  let return x = x
end

module Tuyau = Conduit.Make (None) (Bytes) (String)

let t1 = ref 0L

module Fake_protocol0 = struct
  type input = bytes and output = string and +'a s = 'a

  type endpoint = Unix.file_descr
  type flow = Unix.file_descr
  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .
  let connect x = Ok x
  let recv _ _ = Ok `End_of_flow
  let send fd v = t1 := tick () ; let _ = Unix.write_substring fd v 0 (String.length v) in Ok 0
  let close _ = Ok ()
end

module Fake_protocol1 = struct
  type input = bytes and output = string and +'a s = 'a

  type endpoint = Unix.file_descr
  type flow = Unix.file_descr
  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .
  let connect x = Ok x
  let recv _ _ = Ok `End_of_flow
  let send _ _ = Ok 0
  let close _ = Ok ()
end

module Fake_protocol2 = struct
  type input = bytes and output = string and +'a s = 'a

  type endpoint = Unix.file_descr
  type flow = Unix.file_descr
  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .
  let connect x = Ok x
  let recv _ _ = Ok `End_of_flow
  let send _ _ = Ok 0
  let close _ = Ok ()
end

let fake0 = Tuyau.register ~protocol:(module Fake_protocol0)
let fake1 = Tuyau.register ~protocol:(module Fake_protocol1)
let fake2 = Tuyau.register ~protocol:(module Fake_protocol2)
let hello_world = "Hello World!\n"

let fully_abstr () =
  let open Rresult in
  Tuyau.connect Unix.stdout fake0 >>= fun flow ->
  let t0 = tick () in
  Tuyau.send flow hello_world >>= fun _len ->
  let t2 = tick () in
  R.ok (Int64.sub !t1 t0, Int64.sub t2 !t1)

let abstr () =
  let open Rresult in
  let module Protocol = (val Tuyau.impl fake0) in
  Protocol.connect Unix.stdout
  |> R.reword_error (R.msgf "%a" Protocol.pp_error) >>= fun flow ->
  let t0 = tick () in
  Protocol.send flow hello_world
  |> R.reword_error (R.msgf "%a" Protocol.pp_error) >>= fun _len ->
  let t2 = tick () in
  R.ok (Int64.sub !t1 t0, Int64.sub t2 !t1)

let concrete () =
  let t0 = tick () in
  let _ = Unix.write_substring Unix.stdout hello_world 0 (String.length hello_world) in
  let t1 = tick () in
  Ok (Int64.sub t1 t0)

let () = match concrete (), fully_abstr (), abstr () with
  | Ok ts, Ok (ts0, ts1), Ok (tsa, tsb) ->
    Fmt.pr "fully-abstr: %Ldns, %Ldns.\n%!" ts0 ts1 ;
    Fmt.pr "abstr: %Ldns, %Ldns.\n%!" tsa tsb ;
    Fmt.pr "concrete: %Ldns.\n%!"  ts
  | _ -> assert false

