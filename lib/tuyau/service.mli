type +'a io = 'a

type kind = UDP | TCP

type desc =
  { name : string
  ; port : int
  ; kind : kind }

type ('r, 'e) init = 'r -> 'e -> 'e io

type ('resolved, 'endpoint) service =
  { desc : desc
  ; init : ('resolved, 'endpoint) init }

type 'e scheme

type endpoint

val add : 'r Resolver.resolver -> ('r, 'e) service -> 'e scheme
val endpoint : 'e scheme -> 'e -> endpoint
val resolve : Domain_name.t -> Resolver.t -> 'e scheme -> endpoint -> endpoint option io

