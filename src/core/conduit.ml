include S

type error = [ `Not_found of [ `host ] Domain_name.t | `Closed ]

let pp_error ppf = function
  | `Closed -> Fmt.string ppf "Closed"
  | `Not_found n -> Fmt.pf ppf "Domain `%a' not found" Domain_name.pp n

module Common (IO : IO) (Input : BUFFER) (Output : BUFFER) = struct
  module IO = IO

  type input = Input.t

  type output = Output.t

  type +'a io = 'a IO.t

  type a_flow = ..

  module type FLOW =
    FLOW with type 'a io = 'a io and type input = input and type output = output

  let ( >>= ) x f = IO.bind x f

  let ( >|= ) x f = x >>= fun x -> IO.return (f x)

  let err_msg pp_error e = Error (`Msg (Fmt.strf "%a" pp_error e))

  type flow = F : a_flow * (module FLOW with type t = 'a) * 'a -> flow

  let pp_error ppf = function
    | `Msg e -> Fmt.string ppf e
    | #error as e -> pp_error ppf e

  type nonrec error = [ error | `Msg of string ]

  let recv (F (_, (module M), t)) input =
    M.recv t input >|= function
    | Ok _ as v -> v
    | Error `Closed as e -> e
    | Error e -> err_msg M.pp_error e

  let send (F (_, (module M), t)) output =
    M.send t output >|= function
    | Ok _ as v -> v
    | Error `Closed as e -> e
    | Error e -> err_msg M.pp_error e

  let close (F (_, (module M), t)) =
    M.close t >|= function
    | Ok _ as v -> v
    | Error `Closed -> Ok () (* double-close is ok *)
    | Error e -> err_msg M.pp_error e

  let repr (F (f, _, _)) = f
end

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER) = struct
  include Common (IO) (Input) (Output)

  module type PROTOCOL = sig
    include
      PROTOCOL
        with type 'a io = 'a io
         and type input = input
         and type output = output

    type a_flow += Flow of { endpoint : endpoint; flow : t }
  end

  type ('a, 'b) protocol_impl =
    (module PROTOCOL with type endpoint = 'a and type t = 'b)

  type ('a, 'b) protocol = { name : string; impl : ('a, 'b) protocol_impl }

  let protocol name impl = { name; impl }

  let name t = t.name

  let impl t = t.impl

  let check : type a b. (a, b) protocol -> flow -> (a * b) option =
   fun t (F (flow, _, _)) ->
    let (module M) = t.impl in
    match flow with
    | M.Flow { endpoint; flow } -> Some (endpoint, flow)
    | _ -> None

  let connect : type a b. (a, b) protocol -> a -> (b, error) result io =
   fun e t ->
    let (module M) = e.impl in
    M.connect t >|= function Ok _ as x -> x | Error e -> err_msg M.pp_error e

  module Resolver = struct
    type +'a io = 'a IO.t

    type nonrec ('a, 'b) protocol = ('a, 'b) protocol

    type 'a fn = [ `host ] Domain_name.t -> 'a option io

    type 'a key = K : ('a, 'b) protocol -> 'a key

    type 'a value = V : { priority : int; resolve : 'a fn } -> 'a value

    module Map =
      E1.Make
        (struct
          type 'a t = 'a key
        end)
        (struct
          type 'a t = 'a value
        end)

    type t = Map.t

    let empty = Map.empty

    let add : ?priority:int -> ('a, _) protocol -> 'a fn -> t -> t =
     fun ?(priority = 0) k v ->
      let key = Map.Key.create (K k) in
      Map.add key (V { priority; resolve = v })

    let priority (Map.Value (_, V { priority = pa; _ })) = pa

    let compare x y = Stdlib.compare (priority x) (priority y)

    type endpoint = Endpoint : 'a Map.key * 'a -> endpoint

    let resolve_endpoints m domain_name =
      let rec go acc = function
        | [] -> IO.return (List.rev acc) (* XXX(dinosaure): keep order. *)
        | Map.Value (k, V { resolve; _ }) :: r -> (
            resolve domain_name >>= function
            | Some e -> go (Endpoint (k, e) :: acc) r
            | None -> go acc r) in
      go [] (List.sort compare (Map.bindings m))

    let flow_of_endpoint :
        type a b. (a, b) protocol -> a -> (flow, error) result io =
     fun edn e ->
      let (module M) = edn.impl in
      M.connect e >|= function
      | Error e -> err_msg M.pp_error e
      | Ok t ->
          let flow = M.Flow { endpoint = e; flow = t } in
          Ok (F (flow, (module M), t))

    let connect : 'a Map.key -> 'a -> (flow, error) result io =
     fun key e -> match Map.Key.info key with K edn -> flow_of_endpoint edn e
  end

  let resolve m domain_name =
    Resolver.resolve_endpoints m domain_name >>= fun l ->
    let rec go = function
      | [] -> IO.return (Error (`Not_found domain_name))
      | Resolver.Endpoint (key, edn) :: r -> (
          Resolver.connect key edn >>= function
          | Ok flow -> IO.return (Ok flow)
          | Error _err -> go r) in
    go l
end

module Server (IO : IO) (Input : BUFFER) (Output : BUFFER) = struct
  include Common (IO) (Input) (Output)

  module type SERVICE = sig
    include
      SERVICE
        with type 'a io = 'a io
         and type input = input
         and type output = output

    type a_flow += Flow of flow
  end

  type ('a, 'b, 'c) service_impl =
    (module SERVICE with type config = 'a and type flow = 'b and type t = 'c)

  type ('a, 'b, 'c) service = {
    name : string;
    impl : ('a, 'b, 'c) service_impl;
  }

  let service name impl = { name; impl }

  let name t = t.name

  let impl t = t.impl

  let flow : type a b c. (a, b, c) service -> b -> flow =
   fun service t ->
    let (module M) = service.impl in
    let module F = struct
      include M

      type t = M.flow
    end in
    F (M.Flow t, (module F), t)

  let init : type a b c. (a, b, c) service -> a -> (c, error) result io =
   fun { impl = (module M); _ } e ->
    M.init e >|= function Error e -> err_msg M.pp_error e | Ok _ as t -> t

  let accept : type a b c. (a, b, c) service -> c -> (b, error) result io =
   fun { impl = (module M); _ } e ->
    M.accept e >|= function Error e -> err_msg M.pp_error e | Ok _ as t -> t

  let stop : type a b c. (a, b, c) service -> c -> (unit, error) result io =
   fun { impl = (module M); _ } e ->
    M.stop e >|= function
    | Error `Closed | Ok () -> Ok ()
    | Error e -> err_msg M.pp_error e
end
