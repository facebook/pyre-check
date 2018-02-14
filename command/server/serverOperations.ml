(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Configuration
open Pyre
open Service
open ServerConfiguration
open ServerState

module Check = CommandCheck
module WatchmanConstants = CommandWatchmanConstants

let initialize ?old_state lock connections { configuration; _ } =
  Log.log ~section:`Server  "Initializing server...";
  let scheduler =
    match old_state with
    | Some { scheduler; _ } -> scheduler
    | None -> Scheduler.create ~is_parallel:configuration.parallel ()
  in
  SharedMem.collect `aggressive;
  let timer = Timer.start () in
  let { Check.handles; environment; errors = initial_errors } =
    Check.check configuration (Some scheduler) () in
  Statistics.performance ~name:"initialization" ~timer ~configuration ~normals:[] ();
  Log.log ~section:`Server "Server initialized";
  let handles = File.Handle.Set.of_list handles in
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      initial_errors
      ~f:(fun error ->
          let { Ast.Location.path; _ } = Error.location error in
          Hashtbl.add_multi
            errors
            ~key:(File.Handle.create path)
            ~data:error);
    errors
  in
  {
    deferred_requests = [];
    environment;
    initial_errors = Error.Hash_set.of_list initial_errors;
    errors;
    handles;
    scheduler;
    lock;
    last_request_time = Unix.time ();
    connections;
    lookups = String.Table.create ();
  }


let remove_server_files { lock_path; socket_path; pid_path; socket_link; _ } =
  Path.remove lock_path;
  Path.remove socket_path;
  Path.remove socket_link;
  Path.remove pid_path


let stop_server ~reason ({ configuration; _ } as server_configuration) socket =
  Statistics.event ~flush:true ~name:"stop server" ~configuration ~normals:["reason", reason] ();
  let watchman_pid =
    try
      WatchmanConstants.pid_path configuration
      |> Path.absolute
      |> Sys_utils.cat
      |> Int.of_string
      |> Pid.of_int
      |> fun pid -> Some (`Pid pid)
    with Sys_error _ ->
      None
  in
  watchman_pid >>| Signal.send_i Signal.int  |> ignore;
  remove_server_files server_configuration;
  Unix.close socket;
  Worker.killall ();
  exit 0
