module type FLOW = sig
  (** [FLOW] is the signature for flow clients. *)

  (** A [flow] is an abstract value over which I/O functions such as {!write},
      {!read} and {!close} can be used.

      {[
        type input = bytes

        type output = string

        type +'a io = 'a

        let process t =
          let buf = Bytes.create 0x1000 in
          match Flow.recv t buf with
          | Ok (`Len len) ->
              let str = Bytes.sub_string buf 0 len in
              ignore (Flow.send t str)
          | _ -> failwith "Flow.read"
      ]}

      The given flow can be more complex than a simple TCP flow for example. It
      can be wrapped into a TLS layer. However the goal is to be able to
      implement a protocol without such complexity. *)

  type +'a io
  (** The type for I/O effects. *)

  type t
  (** The type for flow state. *)

  (** {3 Input & Output.}

      Depending on the I/O model, the type for inputs and outputs can differ ;
      for instance they could allow users the ability to define capabilities on
      them such as {i read} or {i write} capabilities.

      However, in most of the current Conduit backends:

      {[
        type input = Cstruct.t

        type output = Cstruct.t
      ]} *)

  type input
  (** The type for payload inputs.*)

  type output
  (** The type for payload outputs. *)

  (** {3 Errors} *)

  type error = private [> `Not_found of [ `host ] Domain_name.t | `Closed ]
  (** The type for errors. *)

  val pp_error : error Fmt.t
  (** [pp_error] is the pretty-printer for {!errors}. *)

  (** {3 Functions} *)

  val recv : t -> input -> ([ `End_of_flow | `Len of int ], error) result io
  (** [recv t i] is [Ok (`Len n)] iff [n] bytes of data has been received from
      the flow [t] and copied in [i]. *)

  val send : t -> output -> (int, error) result io
  (** [send t o] is [Ok n] iff [n] bytes of data from [o] has been sent over the
      flow [t]. *)

  val close : t -> (unit, error) result io
  (** [close t] closes [t]. Subsequent calls to {!recv} or {!send} on [t] will
      return [Error (`Closed)]. *)
end

module type PROTOCOL = sig
  (** [PROTOCOL] is the signature for client-side flows providing a [connect]
      function. *)

  include FLOW

  type endpoint
  (** The type for endpoints.

      For example, a TCP flow {i endpoint} is an IP and a {i port}, while the
      corresponding flow is a byte stream {b already} connected. *)

  val pp_endpoint : endpoint Fmt.t
  (** [pp_endpoint] is the pretty-printer for endpoints. *)

  val connect : endpoint -> (t, error) result io
  (** [connect e] is the flow connected to [e]. *)
end

module type RESOLVER = sig
  (** [RESOLVER] is the signature for cient-side resolvers. *)

  (** The most general description of an {i endpoint} is a domain name;
      [Conduit] thus let developers construct {i endpoints} as
      [\[ `host \] Domain_name.t] values. Resolvers allow domain names to be
      resolved into flows. *)

  type +'a io
  (** The type for I/O effects. *)

  type t
  (** Type for resolver maps. *)

  val empty : t
  (** [empty] is the empty resolver. *)

  type ('a, 'b) protocol
  (** The type for client protocols. *)

  type 'a fn = [ `host ] Domain_name.t -> 'a option io
  (** The type for resolver functions, which resolve domain names to endpoints.
      For instance, the DNS resolver function is:

      {[
        let http_resolver : Unix.sockaddr fn =
         fun domain_name ->
          match Unix.gethostbyname (Domain_name.to_string domain_name) with
          | { Unix.h_addr_list; _ } ->
              if Array.length h_addr_list > 0
              then Some (Unix.ADDR_INET (h_addr_list.(0), 80))
              else None
          | _ -> None
      ]} *)

  val add : ?priority:int -> ('a, _) protocol -> 'a fn -> t -> t
  (** [add ?priority e f t] adds a new resolver function [f] to [t].

      When the [resolver] is able to resolve the given domain name, it will try
      to connect to the specified client endpoint. Resolvers are iterated in
      priority order (lower to higher).

      {[
        let http_resolver = ...
        let https_resolver = ... (* deal with client-side certificates here. *)

        let resolver =
          empty
          |> add Conduit_tcp.endpoint http_resolver
          |> add Conduit_tcp_tls.endpoint https_resolver ~priority:10
          |> add Conduit_tcp_ssl.endpoint https_resolver ~priority:20
      ]} *)
end

module type SERVICE = sig
  (** [SERVICE] is the signature for server-side flows, providing an [init] and
      [accept] functions. *)

  type t
  (** The type for service state. *)

  type flow
  (** The type for flows. *)

  include FLOW with type t := flow

  type config
  (** The type for service configurations. *)

  val init : config -> (t, error) result io
  (** [init e] is the server configured with [e]. *)

  val accept : t -> (flow, error) result io
  (** [accept t] is a thread accepting incoming connections on [t]'s endpoint.
      Unblock the thread once a client is connected and return the underlying
      flow. *)

  val stop : t -> (unit, error) result io
  (** [stop t] releases the resources associated with the server [t]. Subsequent
      calls to {!accept} will return [Error `Closed]. *)
end

module type IO = sig
  (** I/O effects. *)

  type +'a t
  (** The type for I/O effects.

      Conduit does not make any assumption on the underlying I/O effect model.
      It provides implementation for Unix (using system threads), Lwt and Async. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind] is the monadic bind operator. *)

  val return : 'a -> 'a t
  (** [return] is the monadic return operator. *)
end

module type BUFFER = sig
  type t
end

module type Common = sig
  type +'a io
  (** The type for I/O effects. *)

  module IO : IO with type 'a t = 'a io
  (** @inline *)

  type input
  (** The type for input payloads. *)

  type output
  (** The type for output payloads. *)

  (** {1 Flows} *)

  type flow
  (** The type for generic flows. *)

  module type FLOW =
    FLOW with type 'a io = 'a io and type input = input and type output = output

  (** @inline *)
  include
    FLOW
      with type t := flow
       and type 'a io := 'a io
       and type input := input
       and type output := output

  type a_flow = ..
  (** The type for concrete flows. {!FLOW} implementations are extending this
      type. {!repr} allow users to extract the underlying flow implementation
      from a generic flow. *)

  val repr : flow -> a_flow
  (** [repr t] is [t]'s concrete flow representation.

      This can be used to extract the underlying flow implementation:

      {[
        match Flow.repr t with
        | Conduit_lwt.Flow t : Lwt_unix.file_descr) -> ...
        | Conduit.Loopback.Flow (t : Conduit.Loopback.t) -> ...
        | _ -> ... (* use Flow functions for the default case *)
      ]} *)
end

module type S = sig
  (** [S] is the signature for client-side and server-side conduit combinators. *)

  include Common
  (** @inline *)

  (** {1 Client-side Conduits}

      Client-side conduits allow users to create direct connections (using
      {!connect}) and to resolve domain-names into flows (using {!resolve}). *)

  (** {2 Protocols} *)

  (** The signature for protocols. *)
  module type PROTOCOL = sig
    include
      PROTOCOL
        with type 'a io = 'a io
         and type input = input
         and type output = output

    type a_flow += Flow of { endpoint : endpoint; flow : t }
  end

  type ('a, 'b) protocol
  (** The type for client protocols. ['a] is the type for endpoint parameters.
      ['b] is the type for underlying flows.

      Endpoints allow users to create flows by either connecting directly to a
      remote server or by resolving domain names. *)

  type ('a, 'b) protocol_impl =
    (module PROTOCOL with type endpoint = 'a and type t = 'b)
  (** The type for client implementations. *)

  val protocol : string -> ('a, 'b) protocol_impl -> ('a, 'b) protocol
  (** [protocol n m] is the protocol named [n] using the implementation [m]. [m]
      should provide a [connect] function to allow client flows to be created.

      For instance, on Unix, conduit clients will use [Unix.sockadrr] as flow
      endpoints, while [Unix.file_descr] would be used for the flow transport.

      {[
        module Conduit_tcp : sig
          val t: (Unix.sockaddr, Unix.file_descr) Client.protocol
        end = struct
          module TCP = ...
            let t = Client.protocol "tcp" (module TCP)
        end
      ]}

      Client endpoints can of course be more complex, for instance to hold TLS
      credentials, and [Conduit] allow all these kinds of flow to be used
      transparently:

      {[
        module Conduit_tcp_tls : sig
          val t: (Unix.sockaddr * Tls.Config.client, Unix.file_descr) Client.t
        end = struct
          module TLS = ...
          let t = Client.v "tcp+tls" (module TLS)
        end
      ]} *)

  val name : _ protocol -> string
  (** [name t] is [t]'s name. *)

  val impl : ('a, 'b) protocol -> ('a, 'b) protocol_impl
  (** [impl_of_protocol t] is [t]'s implementation. *)

  val check : ('a, 'b) protocol -> flow -> ('a * 'b) option
  (** [check p t] is [Some (e, f)] iff [t] is the flow [f] connected to the
      endpoint [e] using the protocol [p] and [None] otherwise. *)

  (** {2 Direct connection} *)

  val connect : ('a, 'b) protocol -> 'a -> ('b, error) result io
  (** [connect t e] is the flow created by connecting to to the endpoint [e].
      It's similar to call the corresponding [Client.connect] directly. *)

  (** {2 Domain-name resolvers} *)

  (** Domain name resolvers. *)
  module Resolver :
    RESOLVER
      with type 'a io = 'a io
       and type ('a, 'b) protocol = ('a, 'b) protocol

  val resolve : Resolver.t -> [ `host ] Domain_name.t -> (flow, error) result io
  (** [resolve t n] is the flow created by connecting to the domain name [n],
      using the resolvers [t]. Each resolver tries to resolve the given
      domain-name (they are ordered by the given priority). The first which
      connects successfuly wins.

      The resolver result is a flow connected to that winning endpoint.

      {[
        let mirage_io = domain_name_exn "mirage.io"

        val resolver_on_my_private_network : Unix.sockaddr resolver

        val resolver_on_internet : Unix.sockaddr resolver

        val resolver_with_tls : Tls.Config.client -> Unix.sockaddr resolver

        let resolvers =
          let open Client.Resolver in
          empty
          |> add tls ~priority:0 (resolver_with_tls tls_config)
          |> add tcp ~priority:10 resolver_on_my_private_network
          |> add tcp ~priority:20 resolver_on_internet

        let () =
          match Client.resolve resolvers mirage_io with
          | Error err -> failwithf "%a" pp_error err
          | Ok t ->
          match Flow.repr t with
          | TCP.Flow { flow; _ } ->
              let peername = Unix.getpeername t in
              ignore (TCP.send t ("Hello " ^ string_of_sockaddr peername))
          | _ -> ignore (Flow.send t "Hello World!")
      ]} *)
end

module type SERVER = sig
  (** {1 Server-side conduits} *)

  include Common
  (** @inline *)

  module type SERVICE = sig
    include
      SERVICE
        with type 'a io = 'a io
         and type input = input
         and type output = output

    type a_flow += Flow of flow
  end

  type ('a, 'b, 'c) service
  (** The type for services, e.g. service-side protocols. ['a] is the type for
      endpoint parameters. [\b] is the type for underlying flows. ['c] is the
      type for server states. *)

  type ('a, 'b, 'c) service_impl =
    (module SERVICE with type config = 'a and type flow = 'b and type t = 'c)

  val service : string -> ('a, 'b, 'c) service_impl -> ('a, 'b, 'c) service
  (** [service n m] is the service [n] using the implementation [m]. [m] should
      define [init] and [accept] function to be create server-side flows.

      For instance:

      {[
        module TCP_service :
          Server.S with type configuration = Unix.sockaddr
                   and type t = Unix.file_descr
                   and type flow = Unix.file_descr

        let service : (Unix.sockaddr, Unix.file_descr * TCP.t) Server.service =
          Server.service "tcp" (module TCP_service)
      ]} *)

  val name : _ service -> string
  (** [name t] is [t]'s name. *)

  val impl : ('a, 'b, 'c) service -> ('a, 'b, 'c) service_impl
  (** [impl t] is [t]'s underlying implementation. *)

  val flow : ('a, 'b, 'c) service -> 'b -> flow
  (** [flow s t] is the generic flow associated with the service [s] and service
      state [t]. *)

  (** FIXME(samoht): we probably want something to compose services on the
      server-side. *)

  val init : ('a, 'b, 'c) service -> 'a -> ('c, error) result io
  (** [init t e] initialise the service with the configuration [e]. *)

  val accept : ('a, 'b, 'c) service -> 'c -> ('b, error) result io
  (** [accept t s] waits for a connection on the service [t]. The result is a
      stream connected to the client. *)

  val stop : ('a, 'b, 'c) service -> 'c -> (unit, error) result io
  (** [stop t s] releases the resources associated to the server [s]. *)
end
