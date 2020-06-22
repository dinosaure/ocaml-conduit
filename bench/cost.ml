external tick : unit -> (int64[@unboxed]) = "none" "get_tick" [@@noalloc]

module None = struct
  type +'a t = 'a 

  let bind x f = f x
  let return x = x
end

module Tuyau = Conduit.Make (None) (Bytes) (String)

module Fake_protocol = struct
  type input = bytes and output = string and +'a s = 'a

  type endpoint = unit
  type flow = int64 ref
  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .
  let connect () = Ok (ref 0L)
  let recv v _ = v := tick () ; Ok `End_of_flow
  let send v _ = v := tick () ; Ok 0
  let close _ = Ok ()
end

let fake = Tuyau.register ~protocol:(module Fake_protocol)

module Repr = (val Tuyau.repr fake)

let with_tuyau () =
  let open Rresult in
  Tuyau.connect () fake >>= function
  | Repr.T (Conduit.Value v) as flow ->
    let t0 = tick () in
    Tuyau.send flow "Hello World!" >>= fun _len ->
    R.ok (Int64.sub !v t0)
  | _ -> R.error_msgf "Impossible to deconstruct [fake]"

let without_tuyau () =
  let open Rresult in
  let module Protocol = (val Tuyau.impl fake) in
  Protocol.connect ()
  |> R.reword_error (R.msgf "%a" Protocol.pp_error) >>= fun flow ->
  let t0 = tick () in
  Protocol.send flow "Hello World!"
  |> R.reword_error (R.msgf "%a" Protocol.pp_error) >>= fun _len ->
  R.ok (Int64.sub !flow t0)

let () = match without_tuyau (), with_tuyau () with
  | Ok ts0, Ok ts1 ->
    Fmt.pr "without conduit: %Ld tick(s).\n%!" ts0 ;
    Fmt.pr "with conduit: %Ld tick(s).\n%!" ts1
  | Error err, _ -> Fmt.epr "%a.\n%!" Tuyau.pp_error err
  | _, Error _ -> assert false (* safe *)
