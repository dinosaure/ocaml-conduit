module Make (IO : Sigs.IO) = struct
  module Resolver = Resolver.Make(IO)

  type kind = UDP | TCP

  type desc =
    { name : string
    ; port : int
    ; kind : kind }

  type ('r, 'e) init = 'r -> 'e -> 'e IO.t

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
    : type e. Domain_name.t -> Resolver.t -> e scheme -> endpoint -> endpoint option IO.t
    = fun domain m scheme endpoint ->
      match Dispatch.extract endpoint scheme with
      | None -> IO.return None
      | Some e ->
        let module Scheme = (val scheme) in
        let binding = Scheme.instance in
        let Service.B (resolver, service) = binding in
        IO.bind (Resolver.resolve domain resolver m) @@ function
        | None -> IO.return None
        | Some v ->
          IO.map (fun e -> Some (Scheme.T e)) (service.init v e)

  let bind : type e. e scheme -> endpoint -> (e -> endpoint) -> endpoint option
    = fun scheme endpoint f -> match Dispatch.extract endpoint scheme with
      | Some e -> Some (f e)
      | None -> None

  let return = endpoint

  let map : type a b. a scheme -> b scheme -> endpoint -> (a -> b) -> endpoint option
    = fun sx sy endpoint f -> match Dispatch.extract endpoint sx with
      | Some e ->
        let module Scheme = (val sy) in
        let e = f e in
        Some (Scheme.T e)
      | None -> None
end
