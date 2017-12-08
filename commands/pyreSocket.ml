(** Copyright 2016-present Facebook. All rights reserved. **)

open Hack_parallel.Std

open Pyre


type t = Unix.file_descr


let initialize_unix_socket path =
  Socket.unix_socket (Path.absolute path)


let open_connection socket_path =
  Log.debug "Attempting to connect...";
  let socket_address = Unix.ADDR_UNIX (Path.absolute socket_path) in
  try `Success (Unix.open_connection socket_address) with
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _)
  | Unix.Unix_error (Unix.ENOENT, _, _) -> `Failure


(** We're using Hack's marshalling code to read and write on sockets. Please
    refer to hack/src/utils/marshal_tools.ml *)
let write =
  Marshal_tools.to_fd_with_preamble


let read =
  Marshal_tools.from_fd_with_preamble
