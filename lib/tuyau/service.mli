module Make (IO : Sigs.IO) : sig
  module Resolver : module type of Resolver.Make(IO)

  type kind = UDP | TCP

  type desc =
    { name : string
    ; port : int
    ; kind : kind }

  type ('r, 'e) init = 'r -> 'e -> 'e IO.t

  type ('resolved, 'endpoint) service =
    { desc : desc
    ; init : ('resolved, 'endpoint) init }

  type 'e scheme

  type endpoint

  val add : 'r Resolver.resolver -> ('r, 'e) service -> 'e scheme
  val endpoint : 'e scheme -> 'e -> endpoint
  val resolve : Domain_name.t -> Resolver.t -> 'e scheme -> endpoint -> endpoint option IO.t

  val bind : 'e scheme -> endpoint -> ('e -> endpoint) -> endpoint option
  val map : 'a scheme -> 'b scheme -> endpoint -> ('a -> 'b) -> endpoint option
  val return : 'e scheme -> 'e -> endpoint
end
