open Lwt.Infix
include Conduit_lwt_core

let failf fmt = Fmt.kstr (fun err -> Lwt.fail (Failure err)) fmt

let io_of_flow flow =
  let module C = Conduit_lwt_core in
  let ic_closed = ref false and oc_closed = ref false in
  let close () =
    if !ic_closed && !oc_closed
    then
      C.close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failf "%a" C.pp_error err
    else Lwt.return_unit in
  let ic_close () =
    ic_closed := true ;
    close () in
  let oc_close () =
    oc_closed := true ;
    close () in
  let recv buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    recv flow raw >>= function
    | Ok (`Len len) -> Lwt.return len
    | Ok `End_of_flow -> Lwt.return 0
    | Error err -> failf "%a" C.pp_error err in
  let ic = Lwt_io.make ~close:ic_close ~mode:Lwt_io.input recv in
  let send buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    send flow raw >>= function
    | Ok len -> Lwt.return len
    | Error err -> failf "%a" C.pp_error err in
  let oc = Lwt_io.make ~close:oc_close ~mode:Lwt_io.output send in
  (ic, oc)

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX v -> Fmt.pf ppf "<%s>" v
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "<%s:%d>" (Unix.string_of_inet_addr inet_addr) port

module TCP = struct
  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Lwt.t

  type endpoint = Lwt_unix.sockaddr

  type error =
    [ `Address_already_in_use of Unix.sockaddr
    | `Cannot_assign_requested_address of Unix.sockaddr
    | `Address_family_not_supported_by_protocol of Unix.sockaddr
    | `Operation_already_in_progress
    | `Bad_address
    | `Network_is_unreachable
    | `Connection_timed_out
    | `Connection_refused
    | `Transport_endpoint_is_not_connected
    | `Address_is_protected of Unix.sockaddr
    | `Operation_not_permitted
    | `Address_is_not_valid of Unix.sockaddr
    | `Too_many_symbolic_links of Unix.sockaddr
    | `Name_too_long of Unix.sockaddr
    | `Operation_not_supported
    | `Limit_reached
    | `Protocol_error
    | `Firewall_rules_forbid_connection
    | Conduit.error ]

  let pp_error ppf (e : error) =
    match e with
    | `Address_already_in_use sockaddr ->
        Fmt.pf ppf "Address %a already in use" pp_sockaddr sockaddr
    | `Cannot_assign_requested_address sockaddr ->
        Fmt.pf ppf "Cannot assign request address %a" pp_sockaddr sockaddr
    | `Address_family_not_supported_by_protocol sockaddr ->
        Fmt.pf ppf "Address family %a not supported by protocol" pp_sockaddr
          sockaddr
    | `Operation_already_in_progress ->
        Fmt.pf ppf "Operation already in progress"
    | `Bad_address -> Fmt.pf ppf "Bad address"
    | `Network_is_unreachable -> Fmt.pf ppf "Network is unreachable"
    | `Connection_timed_out -> Fmt.pf ppf "Connection timed out"
    | `Connection_refused -> Fmt.pf ppf "Connection refused"
    | `Transport_endpoint_is_not_connected ->
        Fmt.pf ppf "Transport endpoint is not connected"
    | `Address_is_protected sockaddr ->
        Fmt.pf ppf "Address %a is protected" pp_sockaddr sockaddr
    | `Operation_not_permitted -> Fmt.pf ppf "Operation not permitted"
    | `Address_is_not_valid sockaddr ->
        Fmt.pf ppf "Address %a is not valid" pp_sockaddr sockaddr
    | `Too_many_symbolic_links sockaddr ->
        Fmt.pf ppf "Too many symbolic links on %a" pp_sockaddr sockaddr
    | `Name_too_long sockaddr ->
        Fmt.pf ppf "Name %a too long" pp_sockaddr sockaddr
    | `Operation_not_supported -> Fmt.pf ppf "Operation not supported"
    | `Limit_reached -> Fmt.pf ppf "Limit of file-descriptors reached"
    | `Protocol_error -> Fmt.pf ppf "Protocol error"
    | `Firewall_rules_forbid_connection ->
        Fmt.pf ppf "Firewill rules forbid connection"
    | `Closed -> Fmt.pf ppf "Closed"
    | #Conduit.error as e -> Conduit.pp_error ppf e

  let pp_endpoint = pp_sockaddr

  type t = {
    socket : Lwt_unix.file_descr;
    sockaddr : Lwt_unix.sockaddr;
    linger : Bytes.t;
    mutable closed : bool;
  }

  type a_flow += Flow of { endpoint : endpoint; flow : t }

  let peer { sockaddr; _ } = sockaddr

  let sock { socket; _ } = Lwt_unix.getsockname socket

  let file_descr { socket; _ } = socket

  let pp_error = pp_error

  let io_buffer_size = 65536

  let connect sockaddr =
    let socket =
      Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
    in
    let linger = Bytes.create io_buffer_size in
    let rec go () =
      let process () =
        Lwt_unix.connect socket sockaddr >>= fun () ->
        Lwt.return_ok { socket; sockaddr; linger; closed = false } in
      Lwt.catch process @@ function
      | Unix.(Unix_error ((EACCES | EPERM), _, _)) ->
          Lwt.return_error `Operation_not_permitted
      | Unix.(Unix_error (EADDRINUSE, _, _)) ->
          Lwt.return_error (`Address_already_in_use sockaddr)
      | Unix.(Unix_error (EADDRNOTAVAIL, _, _)) ->
          Lwt.return_error (`Cannot_assign_requested_address sockaddr)
      | Unix.(Unix_error (EAFNOSUPPORT, _, _)) ->
          Lwt.return_error (`Address_family_not_supported_by_protocol sockaddr)
      | Unix.(Unix_error (EALREADY, _, _)) ->
          Lwt.return_error `Operation_already_in_progress
      | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
      | Unix.(Unix_error (ENETUNREACH, _, _)) ->
          Lwt.return_error `Network_is_unreachable
      | Unix.(Unix_error (ETIMEDOUT, _, _)) ->
          Lwt.return_error `Connection_timed_out
      | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> go ()
      | Unix.(Unix_error (EINTR, _, _)) -> go ()
      | Unix.(Unix_error (ECONNREFUSED, _, _)) ->
          Lwt.return_error `Connection_refused
      | exn -> Lwt.fail exn
      (* | EPROTOTYPE: impossible *)
      (* | EISCONN: impossible *)
      (* | ENOTSOCK: impossible *)
      (* | EBADF: impossible *)
      (* | EINPROGRESS: TODO *) in
    go ()

  (* XXX(dinosaure): [recv] wants to fill [raw] as much as possible until
     it has reached [`End_of_flow]. *)
  let rec recv ({ socket; closed; _ } as t) raw =
    if closed
    then Lwt.return_ok `End_of_flow
    else
      let rec process filled raw =
        let max = Cstruct.len raw in
        Lwt_unix.read socket t.linger 0 (min max (Bytes.length t.linger))
        >>= fun len ->
        if len = 0
        then Lwt.return_ok (if filled = 0 then `End_of_flow else `Len filled)
        else (
          Cstruct.blit_from_bytes t.linger 0 raw 0 len ;
          if len = Bytes.length t.linger && max > Bytes.length t.linger
          then
            if Lwt_unix.readable t.socket
            then process (filled + len) (Cstruct.shift raw len)
            else
              Lwt.return_ok
                (if filled + len = 0 then `End_of_flow else `Len (filled + len))
          else
            Lwt.return_ok
              (if filled + len = 0 then `End_of_flow else `Len (filled + len)))
      in
      Lwt.catch (fun () -> process 0 raw) @@ function
      | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> recv t raw
      | Unix.(Unix_error (EINTR, _, _)) -> recv t raw
      | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
      | Unix.(Unix_error (ENOTCONN, _, _)) ->
          Lwt.return_error `Transport_endpoint_is_not_connected
      (* | Unix.(Unix_error (ECONNREFUSED, _, _)): TODO *)
      (* | EBADF: impossible *)
      | exn -> Lwt.fail exn

  (* XXX(dinosaure): [send] tries to send as much as it can [raw]. However,
     if [send] returns something smaller that what we requested, we stop
     the process and return how many byte(s) we sended.

     Try to send into a closed socket is an error. *)
  let rec send ({ socket; closed; _ } as t) raw =
    if closed
    then Lwt.return_error `Closed
    else
      let max = Cstruct.len raw in
      let len0 = min (Bytes.length t.linger) max in
      Cstruct.blit_to_bytes raw 0 t.linger 0 len0 ;
      let process () =
        Lwt_unix.write socket t.linger 0 len0 >>= fun len1 ->
        if len1 = len0
        then
          if max > len0
          then send t (Cstruct.shift raw len0)
          else Lwt.return_ok max
        else Lwt.return_ok len1
        (* worst case *) in
      Lwt.catch process @@ function
      | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> send t raw
      | Unix.(Unix_error (EINTR, _, _)) -> send t raw
      | Unix.(Unix_error (EACCES, _, _)) ->
          Lwt.return_error `Operation_not_permitted
      | Unix.(Unix_error (ECONNRESET, _, _)) ->
          Lwt_unix.shutdown t.socket Unix.SHUTDOWN_ALL ;
          t.closed <- true ;
          Lwt.return_error `Closed
      | Unix.(Unix_error (EPIPE, _, _)) ->
          Lwt_unix.shutdown t.socket Unix.SHUTDOWN_ALL ;
          t.closed <- true ;
          Lwt.return_error `Closed
      | Unix.(Unix_error (EDESTADDRREQ, _, _))
      | Unix.(Unix_error (ENOTCONN, _, _)) ->
          Lwt.return_error `Transport_endpoint_is_not_connected
      | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
      (* ENOTSOCK: impossible *)
      (* EISCONN: TODO *)
      (* EOPNOTSUPP: TODO *)
      (* ENOBUFS: TODO & impossible into Linux *)
      | exn -> Lwt.fail exn

  let rec close t =
    let process () =
      if not t.closed
      then (
        Lwt_unix.close t.socket >>= fun () ->
        t.closed <- true ;
        Lwt.return_ok ())
      else Lwt.return_ok () in
    Lwt.catch process @@ function
    | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> close t
    | Unix.(Unix_error (EINTR, _, _)) -> close t
    | exn -> Lwt.fail exn

  let resolve ~port domain_name =
    Lwt_unix.gethostbyname (Domain_name.to_string domain_name) >>= function
    | { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
        Lwt.return_some (Unix.ADDR_INET (h_addr_list.(0), port))
    | _ -> Lwt.return_none
end

let tcp = protocol "tcp" (module TCP)

module Server = struct
  include Conduit_lwt_core.Server

  module TCP = struct
    include TCP

    type config = { sockaddr : Lwt_unix.sockaddr; capacity : int }

    let config ?(capacity = 40) sockaddr = { capacity; sockaddr }

    type t = Lwt_unix.file_descr

    type flow = TCP.t

    type a_flow += Flow of flow

    let is_addr_inet = function
      | Unix.ADDR_INET _ -> true
      | Unix.ADDR_UNIX _ -> false

    let init { sockaddr; capacity } =
      let socket =
        Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
      in
      Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true ;
      let process () =
        Lwt_unix.bind socket sockaddr >>= fun () ->
        Lwt_unix.listen socket capacity ;
        Lwt.return_ok socket in
      Lwt.catch process @@ function
      (* bind *)
      | Unix.(Unix_error (EACCES, _, _)) when is_addr_inet sockaddr ->
          Lwt.return_error (`Address_is_protected sockaddr)
      | Unix.(Unix_error (EACCES, _, _)) (* when is_addr_unix sockaddr *) ->
          Lwt.return_error `Operation_not_permitted
      | Unix.(Unix_error (EADDRINUSE, _, _)) ->
          Lwt.return_error (`Address_already_in_use sockaddr)
      | Unix.(Unix_error (EINVAL, _, _)) ->
          Lwt.return_error (`Address_is_not_valid sockaddr)
      (* | ENOTSOCK: impossible *)
      | Unix.(Unix_error (EADDRNOTAVAIL, _, _)) ->
          Lwt.return_error (`Cannot_assign_requested_address sockaddr)
      | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
      | Unix.(Unix_error (ELOOP, _, _)) ->
          Lwt.return_error (`Too_many_symbolic_links sockaddr)
      | Unix.(Unix_error (ENAMETOOLONG, _, _)) ->
          Lwt.return_error (`Name_too_long sockaddr)
      (* listen *)
      (* | Unix.(Unix_error (EADDRINUSE, _, _)) -> *)
      | Unix.(Unix_error (EOPNOTSUPP, _, _)) ->
          Lwt.return_error `Operation_not_supported
      | exn -> Lwt.fail exn

    let rec accept master =
      (* FIXME(samoht): fail if closed? *)
      let process () =
        Lwt_unix.accept master >>= fun (socket, sockaddr) ->
        let linger = Bytes.create 0x1000 in
        Lwt.return_ok { TCP.socket; sockaddr; linger; closed = false } in
      Lwt.catch process @@ function
      | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> accept master
      | Unix.(Unix_error (EINTR, _, _)) -> accept master
      | Unix.(Unix_error (EMFILE, _, _))
      | Unix.(Unix_error ((ENOBUFS | ENOMEM), _, _)) ->
          Lwt.return_error `Limit_reached
      | Unix.(Unix_error (EPROTOTYPE, _, _)) -> Lwt.return_error `Protocol_error
      | Unix.(Unix_error (EPERM, _, _)) ->
          Lwt.return_error `Firewall_rules_forbid_connection
      | exn -> Lwt.fail exn

    let stop _master =
      (* XXX(dinosaure): it seems that on MacOS, try to close the [master]
         socket raises an error. *)
      Lwt.return_ok ()
  end

  let tcp = service "tcp" (module TCP)
end
