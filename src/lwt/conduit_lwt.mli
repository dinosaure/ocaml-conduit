(** Client-side conduit implementation using [Lwt_unix].

    That implementation differs sligthly from [Lwt_unix.send] and
    [Lwt_unix.recv]:

    - [recv] tries to fill the given buffer as much as possible until it has
      reached {i End_of_flow}. This means that [recv] can do a multiple call to
      [Lwt_unix.recv] to fill the given buffer.

    - [send] tries to send as much as it can the given buffer. However, if
      internal call of [Lwt_unix.send] returns something smaller than what we
      requested, we stop the process and return how many byte(s) were sent. This
      means that [send] can do a multiple call to [Lwt_unix.send] until the full
      buffer was transmitted. *)

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

val io_of_flow :
  flow -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel

module TCP : sig
  include
    PROTOCOL
      with type endpoint = Lwt_unix.sockaddr
       and type error =
            [ `Address_already_in_use of Unix.sockaddr
            | `Cannot_assign_requested_address of Unix.sockaddr
            | `Address_family_not_supported_by_protocol of Unix.sockaddr
            | `Operation_already_in_progress
            | `Bad_address
            | `Network_is_unreachable
            | `Connection_timed_out
            | `Connection_refused
            | `Transport_endpoint_is_not_connected
            | `Address_is_protected of Unix.sockaddr
            | `Operation_not_permitted
            | `Address_is_not_valid of Unix.sockaddr
            | `Too_many_symbolic_links of Unix.sockaddr
            | `Name_too_long of Unix.sockaddr
            | `Operation_not_supported
            | `Limit_reached
            | `Protocol_error
            | `Firewall_rules_forbid_connection
            | Conduit.error ]

  val file_descr : t -> Lwt_unix.file_descr
  (** [file_descr] returns the underlying [Lwt_unix.file_descr] used to
      communicate over TCP. *)

  val peer : t -> Unix.sockaddr
  (** [peer flow] retunrs the address of the peer connected to the given [flow]. *)

  val sock : t -> Unix.sockaddr
  (** [sock flow] returns the current addres to which the socket is bound. *)

  val resolve : port:int -> endpoint Resolver.fn
end

val tcp : (TCP.endpoint, TCP.t) protocol

module Server : sig
  (** {1 Server-side} *)

  include
    Conduit.SERVER
      with type input = Cstruct.t
       and type output = Cstruct.t
       and type +'a io = 'a Lwt.t
       and type flow = flow

  module TCP : sig
    include
      SERVICE
        with type t = Lwt_unix.file_descr
         and type error = TCP.error
         and type flow = TCP.t

    val file_descr : flow -> Lwt_unix.file_descr
    (** [file_descr] returns the underlying [Lwt_unix.file_descr] used to
        communicate over TCP. *)

    val config : ?capacity:int -> Lwt_unix.sockaddr -> config
  end

  val tcp : (TCP.config, TCP.flow, TCP.t) service

  val serve :
    handler:(('a, 'b, 'c) service -> 'b -> unit Lwt.t) ->
    ('a, 'b, 'c) service ->
    'a ->
    unit Lwt_condition.t * unit Lwt.t
end
