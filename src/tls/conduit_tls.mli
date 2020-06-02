(** Common TLS implementation with Conduit.

    The current implementation of the TLS layer over an underlying protocol
    respects some assumptions and it has a specific behaviour which is decribed
    here:

    The {i handshake} is not done when we initialize the flow. Only a call to
    [recv] or [send] really starts the handshake with your peer. In that
    context, a concurrent call of these actions should put some trouble into the
    handshake and they must be protected by an exclusion.

    In other words due to the non-atomicity of [recv] and [send], while the
    handshake, you should ensure to finish a call of one of them before to call
    the other. A mutex should be used in this context to protect the mutual
    exclusion between [recv] and [send]. In others words, such process is safe:

    {[
      let* _ = Conduit.send tls_flow raw in
      let* _ = Conduit.recv tls_flow raw in
    ]}

    Where such process is not safe:

    {[
      async (fun () -> Conduit.send tls_flow raw) ;
      async (fun () -> Conduit.recv tls_flow raw)
    ]}

    The non-atomicity of [recv] and [send] is due to the underlying handshake of
    TLS which can appear everytime. By this fact, [recv] or [send] (depends
    which is executed first) can start an handshake process which can call
    several times underlying [Flow.recv] and [Flow.send] processes (no 0-RTT).
    If you use [async], the scheduler can misleading/misorder handshake started
    with one to the other call to [send] and [recv].

    A solution such as a {i mutex} to ensure the exclusivity between [send] and
    [recv] can be used - it does not exists at this layer where such abstraction
    is not available. *)

module type S =
  Conduit.S with type input = Cstruct.t and type output = Cstruct.t

module Make (S : S) (X : S.PROTOCOL) : sig
  (** Cient-side TLS conduits. *)

  include S.PROTOCOL

  val endpoint : ?tls:Tls.Config.client -> X.endpoint -> endpoint
  (** [endpoint ?tls e] is the TLS endpoint using the configuration [tls] for
      the underlying endpoint [e]. If not [tls] configuration is used, use the
      authenticator [fun _ -> Ok None] which can connect to any host. *)

  val raw : t -> X.t
  (** [raw t] is the underlying raw flow used by TLS. *)

  val handshake_in_progress : t -> bool
  (** [handshake t] is [true] iff a handshake is in progress on [t]. *)
end

module type SERVER =
  Conduit.SERVER with type input = Cstruct.t and type output = Cstruct.t

(** {1 Server-side TLS conduits} *)
module Server (S : SERVER) (X : S.SERVICE) : sig
  include S.SERVICE

  val config : tls:Tls.Config.server -> X.config -> config

  val config_files : cert:string -> key:string -> X.config -> config
  (** [config_files ~cert ~key t] reads the service-side certifate and key from
      the [cert] and [key] files using [Stdlib.really_input]. *)
end
