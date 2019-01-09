module Make (IO : Sigs.IO) = struct
  module Service = Service.Make (IO)

  type t = Scheme : 'a Service.scheme -> t
end
