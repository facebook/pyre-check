(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

module SocketAddress = struct
  type t = Unix.sockaddr

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


  let create_from_address address = address

  let create_from_path path =
    remove_existing_socket_file_if_needed path;
    Lwt_unix.ADDR_UNIX (PyrePath.absolute path)
end

(* Implementation below are mostly copied from `Lwt_io`, with minor simplification and renaming. *)

type t = Lwt_io.server

let shutdown = Lwt_io.shutdown_server

let establish ~handle_connection address =
  Lwt_io.establish_server_with_client_address address handle_connection


let with_server ~f ~handle_connection address =
  let%lwt server = establish ~handle_connection address in
  Lwt.finalize f (fun () -> shutdown server)
