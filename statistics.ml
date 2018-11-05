(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


let enabled = ref true


let cache = String.Table.create ()
let size = 500
let flush_timeout = 6.0 *. 3600.0 (* Seconds. *)


let username = Option.value (Sys.getenv "USER") ~default:(Unix.getlogin ())
let hostname = Option.value (Sys.getenv "HOSTNAME") ~default:(Unix.gethostname ())


let disable () =
  enabled := false


let sample
    ?(integers = [])
    ?(normals = [])
    ?(metadata = true)
    () =
  let open Configuration.Analysis in
  let local_root, start_time, log_identifier =
    match get_global () with
    | Some { local_root; start_time; log_identifier; _ } ->
        Path.last local_root, start_time, log_identifier
    | _ ->
        Log.warning "Trying to log without a global configuration";
        "LOGGED WITHOUT CONFIGURATION", 0.0, "no configuration"
  in
  let server_configuration_metadata =
    match Configuration.Server.get_global () with
    | Some { Configuration.Server.socket_path; saved_state_action; use_watchman; _ } ->
        let saved_state_metadata =
          match saved_state_action with
          | Some
              (Configuration.Server.Load
                 (Configuration.Server.LoadFromFiles {
                     Configuration.Server.shared_memory_path;
                     changed_files_path;
                   })) ->
              [
                "shared_memory_path", Path.absolute shared_memory_path;
                "changed_files_path", Path.absolute changed_files_path;
              ]
          | Some
              (Configuration.Server.Load
                 (Configuration.Server.LoadFromProject
                    name)) ->
              ["saved_state_project", name]
          | Some (Configuration.Server.Save project) ->
              ["save_state_to", project]
          | None ->
              []
        in
        ("socket_path", Path.absolute socket_path) ::
        ("use_watchman", Bool.to_string use_watchman) ::
        saved_state_metadata
    | None ->
        []
  in
  let normals =
    if metadata then
      [
        "binary", Sys.argv.(0);
        "root", local_root;
        "username", username;
        "hostname", hostname;
        "identifier", log_identifier;
      ] @
      server_configuration_metadata @
      normals
    else
      normals
  in
  let integers =
    if metadata then
      [
        "time", Unix.time () |> Int.of_float;
        "start_time", start_time |> Int.of_float;
      ] @ integers
    else
      integers
  in
  Yojson.Safe.to_string (
    `Assoc
      [
        "int", `Assoc (List.map ~f:(fun (label, data) -> label, `Int data) integers);
        "normal", `Assoc (List.map ~f:(fun (label, data) -> label, `String data) normals);
      ])


let last_flush_timestamp = ref (Unix.time ())


let flush () =
  let flush_category ~key ~data =
    Configuration.Analysis.get_global ()
    >>= (fun { Configuration.Analysis.logger = logger; _ } -> logger)
    >>| (fun logger -> Format.sprintf "%s %s" logger key)
    >>| (fun command ->
        let out_channel = Unix.open_process_out command in
        List.iter ~f:(Printf.fprintf out_channel "%s\n") data;
        Out_channel.flush out_channel;
        Unix.close_process_out out_channel
        |> ignore)
    |> ignore
  in
  if !enabled then
    Hashtbl.iteri ~f:flush_category cache;
  Hashtbl.clear cache;
  last_flush_timestamp := Unix.time ()


let flush_cache = flush


let log ?(flush = false) ?(randomly_log_every = 1) category sample =
  if Random.int randomly_log_every = 0 then
    begin
      match Hashtbl.find cache category with
      | Some samples -> Hashtbl.set ~key:category ~data:(sample :: samples) cache
      | _ -> Hashtbl.set ~key:category ~data:[sample] cache
    end;
  let samples_count () =
    Hashtbl.fold cache ~init:0 ~f:(fun ~key:_ ~data count -> count + List.length data)
  in
  let exceeds_timeout () =
    let current_time = Unix.time () in
    current_time -. (!last_flush_timestamp) >= flush_timeout
  in
  if flush || (samples_count () >= size) || (exceeds_timeout ()) then
    flush_cache ()


let performance
    ?(flush = false)
    ?randomly_log_every
    ?always_log_time_threshold
    ?(section = `Performance)
    ?(category = "perfpipe_pyre_performance")
    ~name
    ~timer
    ?(integers = [])
    ?(normals = []) () =
  let seconds = Timer.stop timer in
  let milliseconds = Int.of_float (seconds *. 1000.0) in
  let randomly_log_every =
    match always_log_time_threshold with
    | Some threshold ->
        let threshold_milliseconds = Int.of_float (threshold *. 1000.0) in
        if milliseconds > threshold_milliseconds then None else randomly_log_every
    | None ->
        randomly_log_every
  in
  Log.log ~section "%s: %fs" (String.capitalize name) seconds;
  sample
    ~integers:(("elapsed_time", milliseconds) :: integers)
    ~normals:(("name", name) :: normals)
    ()
  |> log ~flush ?randomly_log_every category


let coverage ?(flush = false) ~path ~coverage  () =
  Log.log
    ~section:`Coverage
    "%s [%s]"
    path
    (List.map coverage ~f:(fun (kind, value) -> Format.sprintf "%s: %d" kind value)
     |> String.concat ~sep:", ");
  sample ~integers:coverage ~normals:["file_name", path] ()
  |> log ~flush "perfpipe_pyre_coverage"


let event ?(flush = false) ?(section = `Event) ~name ?(integers = []) ?(normals = []) () =
  let integer (name, value) = Format.asprintf "%s: %d" name value in
  let normal (name, value) = Format.asprintf "%s: %s" name value in
  Log.log
    ~section
    "%s (%s)"
    (String.capitalize name)
    (List.map ~f:integer integers @ List.map ~f:normal normals
     |> String.concat ~sep:", ");
  sample ~integers:integers ~normals:(("name", name) :: normals) ()
  |> log ~flush "perfpipe_pyre_events"


let log_exception caught_exception ~fatal ~origin =
  event
    ~section:`Error
    ~flush:true
    ~name:"uncaught exception"
    ~integers:[]
    ~normals:[
      "exception", Exn.to_string caught_exception;
      "exception backtrace", Printexc.get_backtrace ();
      "exception origin", origin;
      "fatal", if fatal then "true" else "false";
    ]
    ()
