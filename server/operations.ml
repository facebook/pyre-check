(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Path.AppendOperator
open Network
open State
open Service
open Constants


type version_mismatch = {
  server_version: string;
  expected_version: string;
}
[@@deriving show]


exception ServerNotRunning

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


let create_configuration
    ?(daemonize = true)
    ?log_path
    ?(use_watchman = false)
    ?saved_state_action
    configuration =
  let server_root = Constants.Server.root configuration in
  (* Allow absolute log_path path (e.g., for /dev/null) *)
  let log_path =
    Option.value log_path ~default:(Constants.Server.log_path configuration)
  in
  {
    Configuration.Server.socket_path = socket_path ~create:true configuration;
    socket_link = server_root ^| "server.sock";
    lock_path = server_root ^| "server.lock";
    pid_path = server_root ^| "server.pid";
    log_path;
    daemonize;
    use_watchman;
    watchman_creation_timeout = 5.0 (* Seconds. *);
    saved_state_action;
    configuration;
  }


exception ConnectionFailure
exception VersionMismatch of version_mismatch


let start_from_scratch ?old_state ~lock ~connections ~configuration () =
  Log.log ~section:`Server  "Initializing server...";

  let scheduler =
    match old_state with
    | Some { scheduler; _ } -> scheduler
    | None -> Scheduler.create ~configuration ()
  in
  SharedMem.collect `aggressive;

  let timer = Timer.start () in
  let { Check.handles; environment; errors } =
    Check.check
      ~scheduler:(Some scheduler)
      ~configuration
  in
  Ast.SharedMemory.HandleKeys.add ~handles;
  Statistics.performance
    ~name:"initialization"
    ~timer
    ~normals:["initialization method", "cold start"]
    ();
  Log.log ~section:`Server "Server initialized";
  Memory.init_done ();

  let errors =
    let table = File.Handle.Table.create () in
    let add_error error =
      Hashtbl.add_multi
        table
        ~key:(File.Handle.create (Error.path error))
        ~data:error
    in
    List.iter errors ~f:add_error;
    table
  in
  {
    deferred_requests = [];
    environment;
    errors;
    scheduler;
    lock;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    connections;
    lookups = String.Table.create ();
  }


let start
    ?old_state
    ~lock
    ~connections
    ~configuration:({
        Configuration.Server.configuration =
          ({ Configuration.Analysis.expected_version; _ } as configuration);
        saved_state_action;
        _;
      } as server_configuration)
    () =
  let ({ State.errors; _ } as state) =
    let matches_configuration_version =  Some (Version.version ()) = expected_version in
    match saved_state_action, matches_configuration_version with
    | Some (Load (LoadFromProject _)), true
    | Some (Load (LoadFromFiles _)), _ ->
        begin
          try
            let timer = Timer.start () in
            let state = SavedState.load ~server_configuration ~lock ~connections in
            Statistics.event ~name:"saved state success" ();
            Statistics.performance
              ~name:"initialization"
              ~timer
              ~normals:["initialization method", "saved state"]
              ();
            state
          (* Fall back to starting from scratch if we can't load a saved state. *)
          with SavedState.IncompatibleState reason ->
            Log.warning "Unable to load saved state, falling back to a full start.";
            Statistics.event
              ~name:"saved state failure"
              ~normals:["reason", reason]
              ();
            start_from_scratch ?old_state ~lock ~connections ~configuration ()
        end
    | _ ->
        start_from_scratch ?old_state ~lock ~connections ~configuration ()
  in
  begin
    match saved_state_action with
    | Some (Save saved_state_path) ->
        SavedState.save ~configuration ~errors ~saved_state_path
    | _ ->
        ()
  end;
  state


let stop
    ~reason
    ~configuration:{
    Configuration.Server.configuration;
    lock_path;
    socket_path;
    pid_path;
    socket_link;
    _;
  }
    ~socket =
  Statistics.event ~flush:true ~name:"stop server" ~normals:["reason", reason] ();
  let watchman_pid =
    try
      Watchman.pid_path configuration
      |> Path.absolute
      |> Sys_utils.cat
      |> Int.of_string
      |> Pid.of_int
      |> fun pid -> Some (`Pid pid)
    with Sys_error _ ->
      None
  in
  watchman_pid >>| Signal.send_i Signal.int  |> ignore;

  Path.absolute lock_path
  |> Lock.release
  |> ignore;

  (* Cleanup server files. *)
  Path.remove socket_path;
  Path.remove socket_link;
  Path.remove pid_path;

  Unix.close socket;
  Worker.killall ();
  exit 0


let connect
    ~retries
    ~configuration:({ Configuration.Analysis.expected_version; _ } as configuration) =
  let rec connect attempt =
    if attempt >= retries then begin
      Log.error "Could not connect to server after %d retries" attempt;
      raise ConnectionFailure
    end;
    (* The socket path is computed in each iteration because the server might set up a symlink
       after a connection attempt - in that case, we want to avoid using the stale file. *)
    try
      let path = socket_path configuration in
      if Path.file_exists path then
        begin
          match
            (Unix.handle_unix_error (fun () -> Socket.open_connection path))
          with
          | `Success socket ->
              Log.info "Connected to server";
              socket
          | `Failure ->
              Log.info "Client could not connect.";
              Unix.sleep 1;
              connect (attempt + 1)
        end
      else
        begin
          Log.info "No valid socket found.";
          Unix.sleep 1;
          connect (attempt + 1)
        end
    with
    | ServerNotRunning ->
        Log.info "Waiting for server...";
        Unix.sleep 1;
        connect (attempt + 1)
  in
  let socket =
    let in_channel, _ = connect 0 in
    Unix.descr_of_in_channel in_channel
  in
  Log.debug "Waiting for server response...";
  Socket.read socket
  |> fun (Handshake.ServerConnected server_version) ->
  Socket.write socket Handshake.ClientConnected;
  match expected_version with
  | Some version when version = server_version ->
      socket
  | None ->
      socket
  | Some expected_version ->
      Unix.close socket;
      raise (VersionMismatch { server_version; expected_version })
