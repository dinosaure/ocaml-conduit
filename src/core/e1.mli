module Refl : sig
  type ('a, 'b) t = Refl : ('a, 'a) t
end

type identifier = private int

val identifier_equal : identifier -> identifier -> bool

val identifier_compare : identifier -> identifier -> int

module type T = sig
  type 'a t
end

module Make (K : T) (V : T) : sig
  type 'a key

  module Key : sig
    type 'a info = 'a K.t

    val create : 'a info -> 'a key

    val info : 'a key -> 'a info

    type t
  end

  type t

  val empty : t

  val add : 'a key -> 'a V.t -> t -> t

  type v = Value : 'a key * 'a V.t -> v

  val bindings : t -> v list
end
