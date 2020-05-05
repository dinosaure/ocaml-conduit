(** Conduit with Async. *)

module Async_scheduler :
  Conduit.Sigs.SCHEDULER with type +'a t = 'a Async.Deferred.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a s = 'a Async.Deferred.t

val serve_with_handler :
  handler:('flow Witness.protocol -> 'flow -> unit Async.Deferred.t) ->
  key:'cfg key ->
  service:('master * 'flow) Witness.service ->
  'cfg ->
  unit Async.Condition.t * unit Async.Deferred.t

val reader_and_writer_of_flow :
  flow -> (Async.Reader.t * Async.Writer.t) Async.Deferred.t
