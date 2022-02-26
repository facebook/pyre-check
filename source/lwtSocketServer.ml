(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lwt.Infix

module PreparedSocket = struct
  type t = {
    address: Unix.sockaddr;
    socket: Lwt_unix.file_descr;
  }

  let socket_from_address address =
    let socket = Lwt_unix.socket (Unix.domain_of_sockaddr address) Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    socket


  let create_from_address address =
    let socket = socket_from_address address in
    Lwt_unix.bind socket address
    >>= fun () ->
    let backlog = Lwt_unix.somaxconn () in
    Lwt_unix.listen socket backlog;
    Lwt.return { socket; address }


  (* One of the biggest pain point of using a Unix-domain socket is that the socket file will not
     get auto-deleted if the server process is force-killed or crashes with fatal signals. If that
     happens, the now-defunct socket file on disk will block all future attempts at starting a new
     server process, until the file gets manually removed.

     It may seem that such an issue can be addressed by always removing the pre-existing socket file
     on disk whenever a new server is started. But blind file removal will incur a race condition
     among concurrent server starts: if the pre-existing socket file is not defunct (i.e. another
     living server is still listening on the socket), removing it will render another living server
     unreachable, leaking system resources unless the dangling server gets manually killed.

     To determine whether it is safe to remove the pre-existing socket file, we resort to file lock
     here. One nice property about file lock is that the lock is guaranteed to be automatically
     released from a process even if it gets forced-killed or crashes unexpectedly (ideally we want
     the same property to hold for the socket file itself -- but unfortunately Unix-domain sockets
     do not work this way due to historical reasons). This means that if we cannot grab a lock, we
     know that another living process may still own the socket file, in which case it's probably not
     a good idea to remove the file.

     Note that file locks are advisory, meaning that they cannot protect the socket file from being
     removed by another server process that does not follow the same locking protocol. Besides, file
     locks are not guaranteed to be functional on all OSes and all filesystems. Hence this function
     may not be able to remove defunct sockets in some cases even though it is safe to do so. *)
  let remove_existing_socket_file_if_needed socket_path =
    let lock_path = PyrePath.with_suffix socket_path ~suffix:".lock" in
    match
      Lock_file_blocking.create
        ~close_on_exec:true
        ~unlink_on_exit:true
        (PyrePath.absolute lock_path)
    with
    | true -> PyrePath.remove_if_exists socket_path
    | false -> ()


  let create_from_path path =
    remove_existing_socket_file_if_needed path;
    create_from_address (Lwt_unix.ADDR_UNIX (PyrePath.absolute path))
end

(* Implementation below are mostly copied from `Lwt_io`, with minor simplification and renaming. *)

type t = { shutdown: unit Lwt.t Lazy.t }

let shutdown server = Lazy.force server.shutdown

let do_establish_server ~f { PreparedSocket.address; socket } =
  (* This promise gets resolved with `Should_stop when the user calls Lwt_io.shutdown_server. This
     begins the shutdown procedure. *)
  let should_stop, notify_should_stop = Lwt.wait () in
  (* Some time after Lwt_io.shutdown_server is called, this function establish_server_generic will
     actually close the listening socket. At that point, this promise is resolved. This ends the
     shutdown procedure. *)
  let wait_until_listening_socket_closed, notify_listening_socket_closed = Lwt.wait () in

  let rec accept_loop () =
    let try_to_accept = Lwt_unix.accept socket >|= fun x -> `Accepted x in

    Lwt.pick [try_to_accept; should_stop]
    >>= function
    | `Accepted (client_socket, client_address) ->
        begin
          try Lwt_unix.set_close_on_exec client_socket with
          | Invalid_argument _ -> ()
        end;
        f client_address client_socket;
        accept_loop ()
    | `Should_stop ->
        Lwt_unix.close socket
        >>= fun () ->
        begin
          match address with
          | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' -> Unix.unlink path
          | _ -> ()
        end;
        Lwt.wakeup_later notify_listening_socket_closed ();
        Lwt.return_unit
  in

  let server =
    {
      shutdown =
        lazy
          begin
            Lwt.wakeup_later notify_should_stop `Should_stop;
            wait_until_listening_socket_closed
          end;
    }
  in
  (* Actually start the server. *)
  let server_has_started =
    Lwt.async accept_loop;
    Lwt.return_unit
  in
  server, server_has_started


let do_establish_stream_server ~f prepared_socket =
  let buffer_size = 4096 in
  let close_client_socket fd =
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () ->
            Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
            Lwt.return_unit)
          (function
            (* Occurs if the peer closes the connection first. *)
            | Unix.Unix_error (Unix.ENOTCONN, _, _) -> Lwt.return_unit
            | exn -> Lwt.fail exn))
      (fun () -> Lwt_unix.close fd)
  in
  let f client_address client_socket =
    Lwt.async (fun () ->
        let close = lazy (close_client_socket client_socket) in
        let input_channel =
          Lwt_io.of_fd
            ~buffer:(Lwt_bytes.create buffer_size)
            ~mode:Lwt_io.input
            ~close:(fun () -> Lazy.force close)
            client_socket
        in
        let output_channel =
          Lwt_io.of_fd
            ~buffer:(Lwt_bytes.create buffer_size)
            ~mode:Lwt_io.output
            ~close:(fun () -> Lazy.force close)
            client_socket
        in

        (* Not using Lwt.finalize here, to make sure that exceptions from [f] reach
           !Lwt.async_exception_hook before exceptions from closing the channels. *)
        Lwt.catch
          (fun () -> f client_address (input_channel, output_channel))
          (fun exn ->
            !Lwt.async_exception_hook exn;
            Lwt.return_unit)
        >>= fun () -> Lwt_io.close input_channel >>= fun () -> Lwt_io.close output_channel)
  in
  do_establish_server ~f prepared_socket


let establish ~f prepared_socket =
  let server, server_started = do_establish_stream_server prepared_socket ~f in
  server_started >>= fun () -> Lwt.return server
