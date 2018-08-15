(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Path.AppendOperator

exception ServerNotRunning

type t = {
  (* Server-specific configuration options *)
  socket_path: Path.t;
  socket_link: Path.t;
  lock_path: Path.t;
  pid_path: Path.t;
  log_path: Path.t;
  daemonize: bool;
  use_watchman: bool;
  watchman_creation_timeout: float;
  (* Analysis configuration *)
  configuration: Configuration.t;
}


(* Socket paths in OCaml are limited to a length of +-100 characters. We work around this by
   creating the socket in a temporary directory and symlinking to it from the pyre directory. *)
let socket_path ?(create=false) configuration =
  let link_path = Constants.Server.root configuration ^| "server.sock" in
  if Path.file_exists link_path || not create then
    try
      Unix.readlink (Path.absolute link_path)
      |> Path.create_absolute
    with
    | Unix.Unix_error _ -> raise ServerNotRunning
  else
    begin
      let socket_path =
        let pid = Pid.to_string (Unix.getpid ()) in
        Path.create_relative
          ~root:(Path.create_absolute Filename.temp_dir_name)
          ~relative:("pyre_" ^ pid ^ ".sock")
      in
      (try Unix.unlink (Path.absolute link_path) with | Unix.Unix_error _ -> ());
      Unix.symlink ~src:(Path.absolute socket_path) ~dst:(Path.absolute link_path);
      socket_path
    end


let create
    ?(daemonize = true)
    ?log_path
    ?(use_watchman = false)
    configuration =
  let server_root = Constants.Server.root configuration in
  (* Allow absolute log_path path (e.g., for /dev/null) *)
  let log_path =
    Option.value log_path ~default:(Constants.Server.log_path configuration)
  in
  {
    socket_path = socket_path ~create:true configuration;
    socket_link = server_root ^| "server.sock";
    lock_path = server_root ^| "server.lock";
    pid_path = server_root ^| "server.pid";
    log_path;
    daemonize;
    use_watchman;
    watchman_creation_timeout = 5.0 (* Seconds. *);
    configuration;
  }
