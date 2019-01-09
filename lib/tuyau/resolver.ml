open E1

type +'a io = 'a

let bind : 'a io -> ('a -> 'b io) -> 'b io = fun x f -> f x
let return : 'a -> 'a io = fun x -> x

module type RESOLVER = FUNCTOR with type 'a t = Domain_name.t -> 'a option io

module Desc = struct
  type 'a t = { name : string }

  let name : type a. a t -> string =
    fun { name; _ } -> name
end

module Resolver : RESOLVER = struct type 'a t = Domain_name.t -> 'a option io end
module Map = Make (Desc) (Resolver)

type 'a resolver = 'a Map.key

let make (type v) ~name : v resolver = Map.Key.create { Desc.name }

let name : 'a resolver -> string = fun resolver ->
  let { Desc.name } = Map.Key.info resolver in name

type t = Map.t

let table = Map.empty
let add k ~resolve t = Map.add k resolve t
let rem = Map.rem
let get k t = match Map.find k t with
  | Some v -> v
  | None -> raise Not_found

let resolve : type v. Domain_name.t -> v resolver -> Map.t -> v option io =
  fun domain key m -> match Map.find key m with
    | Some resolver -> resolver domain
    | None -> return None
