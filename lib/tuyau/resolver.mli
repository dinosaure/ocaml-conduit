type +'a io = 'a

type t
type 'v resolver

val make : name:string -> 'v resolver
val name : 'v resolver -> string

val table : t
val add : 'v resolver -> resolve:(Domain_name.t -> 'v option io) -> t -> t
val rem : 'v resolver -> t -> t
val get : 'v resolver -> t -> (Domain_name.t -> 'v option io)

val resolve : Domain_name.t -> 'v resolver -> t -> 'v option io
