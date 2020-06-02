(** Conduit with Async. *)

open Async

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Deferred.t

val reader_and_writer_of_flow : flow -> (Reader.t * Writer.t) Deferred.t

module TCP : sig
  type endpoint =
    | Inet of Socket.Address.Inet.t
    | Unix of Socket.Address.Unix.t

  include PROTOCOL with type endpoint := endpoint

  val address : t -> Socket.Address.t

  val reader : t -> Reader.t

  val writer : t -> Writer.t

  val resolve : port:int -> endpoint Resolver.fn
end

val tcp : (TCP.endpoint, TCP.t) protocol

module Server : sig
  include
    Conduit.SERVER
      with type input = Cstruct.t
       and type output = Cstruct.t
       and type +'a io = 'a Async.Deferred.t
       and type flow = flow

  module TCP : sig
    type config

    val config : ('a, 'b) Tcp.Where_to_listen.t -> config

    include SERVICE with type config := config and type flow = TCP.t
  end

  val tcp : (TCP.config, TCP.flow, TCP.t) service

  val serve :
    handler:(('a, 'b, 'c) service -> 'b -> unit Deferred.t) ->
    ('a, 'b, 'c) service ->
    'a ->
    unit Condition.t * unit Deferred.t
end
