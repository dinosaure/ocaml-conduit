module Make (IO : Sigs.IO) : sig
  module Service : module type of Service.Make(IO)

  type t = Scheme : 'e Service.scheme -> t
end
