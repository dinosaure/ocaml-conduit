(** Implementation of the SSL support (according [Lwt_ssl]) with
    [conduit-lwt-unix].

    This implementation assumes that underlying protocol used to compose with
    SSL must deliver a [Lwt_unix.file_descr] - such as [Conduit_lwt_unix_tcp].
    From that, we are able to compose your protocol with [Lwt_ssl] such as:

    {[
      let ssl_endpoint, ssl_protocol =
        protocol_with_ssl ~key:TCP.endpoint TCP.protocol

      let ssl_configuration, ssl_service =
        service_with_ssl ~key:TCP.configuration TCP.service
          ~file_descr:TCP.file_descr ssl_protocol
    ]}

    Then, TCP + SSL is available as any others [conduit] protocols or services
    registered.

    {b NOTE}: [close] implementation properly closes an SSL connection with
    [Ssl.ssl_shutdown] AND properly closes the underlying file-descriptor by
    itself. From a given implementation of a protocol like [TCP], [TCP.close]
    will never be called by the SSL layer - but the file-descriptor will be
    closed.

    {b NOTE}: [verify] is called after a call to [flow] (which should do the
    [connect] call). So, nothing was exchanged between you and your peer at this
    time - even the handshake. *)

open Conduit_lwt

module TCP : sig
  include PROTOCOL with type t = Lwt_ssl.socket

  type verify =
    Ssl.context ->
    TCP.t ->
    (Lwt_ssl.socket, [ `Verification_error of string ]) result Lwt.t

  val endpoint : ?ssl:Ssl.context -> ?verify:verify -> TCP.endpoint -> endpoint
  (** [endpoint ~ssl ?verify edn] returns an {i endpoint} which can be used to
      initialize an SSL connection.

      [verify] is the function called just after the initialization of the
      underlying flow. It allo users to verify connection properties, for
      instance checking the {i hostname} of the connected client. *)

  val resolve :
    ?port:int ->
    ?verify:verify ->
    ?ssl:Ssl.context ->
    unit ->
    endpoint Resolver.fn
end

val tcp : (TCP.endpoint, Lwt_ssl.socket) protocol
(** [tcp] returns a representation of the given protocol with SSL. *)

module Server : sig
  open Conduit_lwt.Server

  module TCP : sig
    include SERVICE with type flow = Lwt_ssl.socket

    val config : ssl:Ssl.context -> ?capacity:int -> Lwt_unix.sockaddr -> config

    val config_files :
      cert:string -> key:string -> ?capacity:int -> Lwt_unix.sockaddr -> config
  end

  val tcp : (TCP.config, TCP.flow, TCP.t) service
end
