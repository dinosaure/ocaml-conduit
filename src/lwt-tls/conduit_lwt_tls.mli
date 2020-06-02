(** Implementation of the TLS support using [ocaml-tls] and [conduit-lwt].

    For more details about behaviours, you should look into [conduit-tls]. *)

open Conduit_lwt

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
    ?port:int -> ?tls:Tls.Config.client -> unit -> endpoint Resolver.fn
end

val tcp : (TCP.endpoint, TCP.t) protocol

module Server : sig
  open Conduit_lwt.Server

  module TCP : sig
    include SERVICE

    val config :
      tls:Tls.Config.server -> ?capacity:int -> Lwt_unix.sockaddr -> config

    val config_files :
      cert:string -> key:string -> ?capacity:int -> Lwt_unix.sockaddr -> config
    (** [config_files ~cert ~key t] reads the service-side certifate and key
        from the [cert] and [key] files using [Stdlib.really_input]. *)
  end

  val tcp : (TCP.config, TCP.flow, TCP.t) Server.service
end
