open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module IO = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f

  let return x = Lwt.return x
end

module C = Conduit.Make (IO) (Cstruct) (Cstruct)
include C

module Server = struct
  module S = Conduit.Server (IO) (Cstruct) (Cstruct)
  module IO = IO
  include S

  let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

  let serve ~handler service cfg =
    let stop = Lwt_condition.create () in
    let main =
      S.init service cfg >>= function
      | Error err -> failwith "%a" S.pp_error err
      | Ok t -> (
          let rec loop () =
            let stop =
              Lwt_condition.wait stop >>= fun () -> Lwt.return_ok `Stop in
            let accept =
              S.accept service t >>? fun flow -> Lwt.return_ok (`Flow flow)
            in

            Lwt.pick [ stop; accept ] >>= function
            | Ok (`Flow flow) ->
                Lwt.async (fun () -> handler service flow) ;
                Lwt.pause () >>= loop
            | Ok `Stop -> S.stop service t
            | Error err0 -> (
                S.stop service t >>= function
                | Ok () -> Lwt.return_error err0
                | Error _err1 -> Lwt.return_error err0) in
          loop () >>= function
          | Ok () -> Lwt.return_unit
          | Error err -> failwith "%a" S.pp_error err) in
    (stop, main)
end
