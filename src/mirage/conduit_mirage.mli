include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

module TCP (S : Mirage_stack.V4) : sig
  include PROTOCOL

  val endpoint :
    ?keepalive:Mirage_protocols.Keepalive.t ->
    ?nodelay:bool ->
    S.t ->
    Ipaddr.V4.t * int ->
    endpoint
end

(** An implementation of [conduit-lwt] according the interface [Mirage_flow.S].
    This module is deprecated when the current implementation of [read] has
    another behaviour:

    [conduit] provides:

    {[ val read : flow -> Cstruct.t -> (int or_eoi, error) result Lwt.t ]}

    where [mirage-flow] expects:

    {[ val read : flow -> (Cstruct.t or_eoi, error) result Lwt.t ]}

    This current implementation allocates an {b arbitrary} 4096 bytes buffer to
    fit under the [mirage-flow] interface. [conduit] did the choice to follow
    the POSIX interface and let the end-user to allocate by himself the input
    buffer. *)
module Flow (P : PROTOCOL) : Mirage_flow.S with type flow = P.t

module Resolve
    (S : Mirage_stack.V4)
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK) : sig
  include module type of Dns_client_mirage.Make (R) (T) (C) (S)

  val resolve :
    t ->
    ?nameserver:Transport.ns_addr ->
    port:int ->
    (Ipaddr.V4.t * int) Resolver.fn
end

module Server (S : Mirage_stack.V4) : sig
  include
    Conduit.SERVER
      with type input = Cstruct.t
       and type output = Cstruct.t
       and type +'a io = 'a Lwt.t

  module TCP : sig
    include SERVICE

    val config :
      ?keepalive:Mirage_protocols.Keepalive.t ->
      ?nodelay:bool ->
      S.t ->
      int ->
      config
  end

  val serve :
    handler:(('a, 'b, 'c) service -> 'b -> unit Lwt.t) ->
    ('a, 'b, 'c) service ->
    'a ->
    unit Lwt_condition.t * unit Lwt.t
end
