(* (c) Daniel Bünzli *)

module Refl = struct
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module Type = struct
  type 'a t = ..
end

module type TYPE = sig
  type t

  type _ Type.t += T : t Type.t
end

type 'a t = (module TYPE with type t = 'a)

let tid () (type x) =
  let module X = struct
    type t = x

    type _ Type.t += T : t Type.t
  end in
  (module X : TYPE with type t = x)

let eq : type a b. a t -> b t -> (a, b) Refl.t option =
 fun a b ->
  let module A = (val a : TYPE with type t = a) in
  let module B = (val b : TYPE with type t = b) in
  match A.T with B.T -> Some Refl.Refl | _ -> None

type identifier = int

let identifier_equal a b = (compare : int -> int -> int) a b = 0

let identifier_compare a b = (compare : int -> int -> int) a b

module type T = sig
  type 'a t
end

module Make (K : T) (V : T) = struct
  module Key = struct
    type 'a info = 'a K.t

    type 'a key = { uid : identifier; tid : 'a t; info : 'a K.t }

    let uid =
      let x = ref (-1) in
      fun () ->
        incr x ;
        !x

    let create info =
      let uid = uid () in
      let tid = tid () in
      { uid; tid; info }

    let info { info; _ } = info

    type t = K : 'a key -> t

    let compare (K a) (K b) = (compare : int -> int -> int) a.uid b.uid
  end

  type 'a key = 'a Key.key

  module Map = Map.Make (Key)

  type binding = B : 'a key * 'a V.t -> binding

  type t = binding Map.t

  let empty = Map.empty

  let add k v m = Map.add (Key.K k) (B (k, v)) m

  type v = Value : 'a key * 'a V.t -> v

  let bindings m =
    Map.bindings m
    |> List.fold_left
         (fun a (Key.K k, B (k', v)) ->
           match eq k.Key.tid k'.Key.tid with
           | Some Refl.Refl -> Value (k, v) :: a
           | None -> a)
         []
end
