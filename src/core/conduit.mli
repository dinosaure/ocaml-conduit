(** [Conduit] is a library to compose protocol {i flows}. *)

(** {1 Flow} *)

(** A flow is a system that allows entities to transmit {i payloads}. These
    entities do not have to care about the underlying transport mechanism. Flows
    simply deal with routing and delivering of these paylods. That abstraction
    allow these protocols to compose.

    For example, the Transmission Control Protocol (TCP) is representable as a
    flow, because it is able to encapsulate some {i payload} without
    interpreting it. A counter-example is the Simple Mail Transfer Protocol
    (SMTP) which needs an interpretation of its {i payload}: tokens such as
    [EHLO] or [QUIT] have a direct incidence over the lifle-cycle of the
    connection.

    An other protocol representatble as a flow is the Transport Layer Security
    (TLS), as it deals only with privacy and data integrity. [Conduit] is able
    to compose flows together like [TCP ∘ TLS] to make a new flow.
    Higher-level protocols can be built in top of these abstract flows: For
    instance, Secure Simple Mail Transfer Protocol (SSMTP) or HyperText Transfer
    Protocol Secure (HTTPS) can be defined on top of both TCP and TLS. Using
    conduit, these can be abstracted to work over any flow implementations. *)

module type FLOW = S.FLOW
(** @inline *)

(** FIXME: Add Mirage_flow.stats *)

(** {1 Clients} *)

module type PROTOCOL = S.PROTOCOL
(** @inline *)

module type RESOLVER = S.RESOLVER
(** @inline *)

(** {1 Services} *)

module type SERVICE = S.SERVICE
(** @inline *)

(** {1 Builders} *)

module type IO = S.IO
(** @inline *)

module type BUFFER = S.BUFFER
(** @inline *)

module type S = S.S
(** @inline *)

module type SERVER = S.SERVER
(** @inline *)

type error = [ `Not_found of [ `host ] Domain_name.t | `Closed ]

val pp_error : error Fmt.t

module Common (IO : IO) (Input : BUFFER) (Output : BUFFER) : sig
  type a_flow = ..

  module type FLOW =
    FLOW
      with type 'a io = 'a IO.t
       and type input = Input.t
       and type output = Output.t

  type flow = F : a_flow * (module FLOW with type t = 'a) * 'a -> flow
end

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER) :
  S
    with type input = Input.t
     and type output = Output.t
     and type 'a io = 'a IO.t
     and module IO := IO
     and type flow = Common(IO)(Input)(Output).flow

module Server (IO : IO) (Input : BUFFER) (Output : BUFFER) :
  SERVER
    with type input = Input.t
     and type output = Output.t
     and type 'a io = 'a IO.t
     and module IO := IO
     and type flow = Common(IO)(Input)(Output).flow
