open Async
open Conduit_async

module TCP = struct
  include Conduit_tls.Make (Conduit_async) (TCP)

  let resolve ?tls ?(port = 443) () domain_name =
    TCP.resolve ~port domain_name >>| function
    | Some e -> Some (endpoint ?tls e)
    | None -> None
end

let tcp = protocol "tls+tcp" (module TCP)

module Server = struct
  open Server

  module TCP = struct
    include Conduit_tls.Server (Server) (TCP)

    let config ~tls x = config ~tls (TCP.config x)

    let config_files ~cert ~key x = config_files ~cert ~key (TCP.config x)
  end

  let tcp = service "tls_tcp" (module TCP)
end
