type +'a io = 'a

let bind : 'a io -> ('a -> 'b io) -> 'b io = fun x f -> f x
let return : 'a -> 'a io = fun x -> x
let ( >>= ) : 'a io -> ('a -> 'b io) -> 'b io = bind

type kind = UDP | TCP

type desc =
  { name : string
  ; port : int
  ; kind : kind }

type ('r, 'e) init = 'r -> 'e -> 'e io

type ('r, 'e) service =
  { desc : desc
  ; init : ('r, 'e) init }

module Service = struct type 'e t = B : 'r Resolver.resolver * ('r, 'e) service -> 'e t end

open E0

module Dispatch = Make(Service)

type 'e scheme = 'e Dispatch.extension

let add
  : type r e. r Resolver.resolver -> (r, e) service -> e scheme
  = fun resolver service ->
    let value = Service.B (resolver, service) in
    Dispatch.inj value

type endpoint = Dispatch.t

let endpoint : type e. e scheme -> e -> endpoint =
  fun scheme endpoint ->
    let module Scheme = (val scheme) in
    Scheme.T endpoint

let resolve
  : type e. Domain_name.t -> Resolver.t -> e scheme -> endpoint -> endpoint option io
  = fun domain m scheme endpoint ->
    match Dispatch.extract endpoint scheme with
    | None -> None
    | Some e ->
      let module Scheme = (val scheme) in
      let binding = Scheme.instance in
      let Service.B (resolver, service) = binding in
      match Resolver.resolve domain resolver m with
      | None -> None
      | Some v ->
        let _e = service.init v e in
        return (Some (Scheme.T e))
