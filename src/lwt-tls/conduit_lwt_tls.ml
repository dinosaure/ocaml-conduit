open Conduit_lwt

module TCP = struct
  include Conduit_tls.Make (Conduit_lwt) (Conduit_lwt.TCP)

  let resolve ?(port = 443) ?tls () domain_name =
    let open Lwt.Infix in
    TCP.resolve ~port domain_name >|= function
    | Some e -> Some (endpoint ?tls e)
    | None -> None
end

let tcp = protocol "tls+tcp" (module TCP)

module Server = struct
  open Conduit_lwt.Server

  module TCP = struct
    include Conduit_tls.Server (Server) (TCP)

    let config ~tls ?capacity t =
      let t = TCP.config ?capacity t in
      config ~tls t

    let config_files ~cert ~key ?capacity t =
      let t = TCP.config ?capacity t in
      config_files ~cert ~key t
  end

  let tcp = service "tls+tcp" (module TCP)
end
