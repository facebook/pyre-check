(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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


let write_ignoring_epipe socket message =
  try
    Marshal_tools.to_fd_with_preamble socket message
  with
  | Unix.Unix_error (kind, name, parameter) ->
      match kind with
      | Unix.EPIPE -> Log.info "Ignoring error on write due to EPIPE"
      | _ -> raise (Unix.Unix_error (kind, name, parameter))
