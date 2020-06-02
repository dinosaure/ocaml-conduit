open Conduit_async

module TCP : sig
  include PROTOCOL

  val endpoint : ?tls:Tls.Config.client -> TCP.endpoint -> endpoint
  (** [endpoint ~config e] is the TLS endpoint using the configuration [config]
      for the underlying endpoint [e]. *)

  val raw : t -> TCP.t
  (** [raw t] is the underlying TCP flow used by TLS. *)

  val handshake_in_progress : t -> bool
  (** [handshake t] is [true] iff a handshake is in progress on [t]. *)

  val resolve :
    ?tls:Tls.Config.client -> ?port:int -> unit -> endpoint Resolver.fn
end

val tcp : (TCP.endpoint, TCP.t) protocol

module Server : sig
  open Conduit_async.Server

  module TCP : sig
    include SERVICE

    val config :
      tls:Tls.Config.server -> ('a, 'b) Async.Tcp.Where_to_listen.t -> config

    val config_files :
      cert:string ->
      key:string ->
      ('a, 'b) Async.Tcp.Where_to_listen.t ->
      config
  end

  val tcp : (TCP.config, TCP.flow, TCP.t) Server.service
end
