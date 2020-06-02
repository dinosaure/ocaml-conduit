open Async_ssl
open Conduit_async

type context

module TCP : sig
  include PROTOCOL

  val endpoint :
    ?version:Ssl.Version.t ->
    ?options:Ssl.Opt.t list ->
    ?name:string ->
    ?hostname:string ->
    ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ] ->
    ?ca_file:string ->
    ?ca_path:string ->
    ?crt_file:string ->
    ?key_file:string ->
    ?session:Ssl.Session.t ->
    ?verify_modes:Verify_mode.t list ->
    ?verify:(Ssl.Connection.t -> bool Async.Deferred.t) ->
    TCP.endpoint ->
    endpoint

  val resolve :
    ?port:int ->
    ?version:Ssl.Version.t ->
    ?options:Ssl.Opt.t list ->
    ?name:string ->
    ?hostname:string ->
    ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ] ->
    ?ca_file:string ->
    ?ca_path:string ->
    ?crt_file:string ->
    ?key_file:string ->
    ?session:Ssl.Session.t ->
    ?verify_modes:Verify_mode.t list ->
    ?verify:(Ssl.Connection.t -> bool Async.Deferred.t) ->
    unit ->
    endpoint Resolver.fn
end

val tcp : (TCP.endpoint, TCP.t) protocol
(** [tcp] returns a representation of the given protocol with SSL. *)

module Server : sig
  module TCP : sig
    include Server.SERVICE with type flow = TCP.t

    val config :
      ?version:Ssl.Version.t ->
      ?options:Ssl.Opt.t list ->
      ?name:string ->
      ?hostname:string ->
      ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ] ->
      ?ca_file:string ->
      ?ca_path:string ->
      ?crt_file:string ->
      ?key_file:string ->
      ?session:Ssl.Session.t ->
      ?verify_modes:Verify_mode.t list ->
      ?verify:(Ssl.Connection.t -> bool Async.Deferred.t) ->
      ('a, 'b) Async.Tcp.Where_to_listen.t ->
      config
  end

  val tcp : (TCP.config, TCP.flow, TCP.t) Server.service
end
