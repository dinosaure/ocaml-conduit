(** Conduit with LWT. *)

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

module Server : sig
  include
    Conduit.SERVER
      with type input = Cstruct.t
       and type output = Cstruct.t
       and type +'a io = 'a Lwt.t
       and type flow = flow

  val serve :
    handler:(('a, 'b, 'c) service -> 'b -> unit Lwt.t) ->
    ('a, 'b, 'c) service ->
    'a ->
    unit Lwt_condition.t * unit Lwt.t
end
