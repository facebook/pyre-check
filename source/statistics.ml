(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module GlobalState = struct
  type t = {
    mutable logger: string option;
    mutable last_flush_timestamp: float;
    mutable log_identifier: string;
    mutable project_name: string;
    mutable project_root: string;
    mutable start_time: float;
  }

  let flush_size = 500

  let flush_timeout = 6.0 *. 3600.0 (* Seconds. *)

  let username = Option.value (Sys.getenv "USER") ~default:(Unix.getlogin ())

  let hostname = Option.value (Sys.getenv "HOSTNAME") ~default:(Unix.gethostname ())

  let global_state =
    let current_time = Unix.time () in
    {
      logger = None;
      last_flush_timestamp = current_time;
      log_identifier = "";
      project_name = "";
      project_root = "";
      start_time = current_time;
    }


  let initialize ?logger ?log_identifier ?project_name ?project_root () =
    Option.iter logger ~f:(fun logger -> global_state.logger <- Some logger);
    Option.iter log_identifier ~f:(fun identifier -> global_state.log_identifier <- identifier);
    Option.iter project_name ~f:(fun name -> global_state.project_name <- name);
    Option.iter project_root ~f:(fun root -> global_state.project_root <- root);
    ()


  let get () = global_state

  let restore old_state =
    global_state.logger <- old_state.logger;
    global_state.last_flush_timestamp <- old_state.last_flush_timestamp;
    global_state.log_identifier <- old_state.log_identifier;
    global_state.project_name <- old_state.project_name;
    global_state.start_time <- old_state.start_time;
    global_state.project_root <- old_state.project_root
end

module Cache : sig
  val with_cache : f:(string list String.Table.t -> 'a) -> 'a
end = struct
  let cache = String.Table.create ()

  let lock = Error_checking_mutex.create ()

  let with_cache ~f = Error_checking_mutex.critical_section lock ~f:(fun () -> f cache)
end

let disable () = GlobalState.global_state.logger <- None

let format_as_json ~integers ~normals () =
  Yojson.Safe.to_string
    (`Assoc
      [
        "int", `Assoc (List.map ~f:(fun (label, data) -> label, `Int data) integers);
        "normal", `Assoc (List.map ~f:(fun (label, data) -> label, `String data) normals);
      ])


let sample ?(integers = []) ?(normals = []) ?(metadata = true) () =
  let normals =
    if metadata then
      [
        "binary", Sys.argv.(0);
        "root", GlobalState.global_state.project_name;
        "username", GlobalState.username;
        "host", GlobalState.hostname;
        "identifier", GlobalState.global_state.log_identifier;
        "project_root", GlobalState.global_state.project_root;
      ]
      @ normals
    else
      normals
  in
  let integers =
    if metadata then
      [
        "time", Unix.time () |> Int.of_float;
        "start_time", GlobalState.global_state.start_time |> Int.of_float;
      ]
      @ integers
    else
      integers
  in
  format_as_json ~integers ~normals ()


let flush () =
  match GlobalState.global_state.logger with
  | None -> ()
  | Some logger ->
      let flush_category ~key ~data =
        let command = Format.sprintf "%s %s" logger key in
        let out_channel = Unix.open_process_out command in
        List.iter ~f:(Printf.fprintf out_channel "%s\n") data;
        Out_channel.flush out_channel;
        Unix.close_process_out out_channel |> ignore
      in
      Cache.with_cache ~f:(fun cache ->
          Hashtbl.iteri ~f:flush_category cache;
          Hashtbl.clear cache);
      GlobalState.global_state.last_flush_timestamp <- Unix.time ();
      ()


let flush_cache = flush

let log ?(flush = false) category sample =
  Cache.with_cache ~f:(fun cache ->
      match Hashtbl.find cache category with
      | Some samples -> Hashtbl.set ~key:category ~data:(sample :: samples) cache
      | _ -> Hashtbl.set ~key:category ~data:[sample] cache);
  let samples_count () =
    Cache.with_cache ~f:(fun cache ->
        Hashtbl.fold cache ~init:0 ~f:(fun ~key:_ ~data count -> count + List.length data))
  in
  let exceeds_timeout () =
    let current_time = Unix.time () in
    Float.(
      current_time -. GlobalState.global_state.last_flush_timestamp >= GlobalState.flush_timeout)
  in
  if flush || samples_count () >= GlobalState.flush_size || exceeds_timeout () then
    flush_cache ()


let should_log = function
  | None -> true
  | Some randomly_log_every -> Int.equal (Random.int randomly_log_every) 0


let performance
    ?(flush = false)
    ?randomly_log_every
    ?always_log_time_threshold
    ?(section = `Performance)
    ~name
    ~timer
    ?phase_name
    ?(integers = [])
    ?(normals = [])
    ()
  =
  let time_in_seconds = Timer.stop_in_sec timer in
  let integer_time_in_microseconds = time_in_seconds *. 1e6 |> Int.of_float in
  Log.log ~section "%s: %.3fs" (String.capitalize name) time_in_seconds;
  Profiling.log_performance_event (fun () ->
      let tags =
        List.map ~f:(fun (name, value) -> name, string_of_int value) integers
        |> List.rev_append normals
      in
      let tags =
        match phase_name with
        | None -> tags
        | Some name -> ("phase_name", name) :: tags
      in
      Profiling.Event.create name ~event_type:(Duration integer_time_in_microseconds) ~tags);
  let randomly_log_every =
    match always_log_time_threshold with
    | Some threshold -> if Float.(time_in_seconds > threshold) then None else randomly_log_every
    | None -> randomly_log_every
  in
  match should_log randomly_log_every with
  | false -> ()
  | true ->
      sample
        ~integers:(("elapsed_time", integer_time_in_microseconds) :: integers)
        ~normals:(("name", name) :: normals)
        ()
      |> log ~flush "perfpipe_pyre_performance"


let event
    ?(flush = false)
    ?randomly_log_every
    ?(section = `Event)
    ~name
    ?(integers = [])
    ?(normals = [])
    ()
  =
  let integer (name, value) = Format.asprintf "%s: %d" name value in
  let normal (name, value) = Format.asprintf "%s: %s" name value in
  Log.log
    ~section
    "%s (%s)"
    (String.capitalize name)
    (List.map ~f:integer integers @ List.map ~f:normal normals |> String.concat ~sep:", ");
  match should_log randomly_log_every with
  | false -> ()
  | true ->
      sample ~integers ~normals:(("name", name) :: normals) () |> log ~flush "perfpipe_pyre_events"


let log_model_query_outputs
    ?(flush = false)
    ?(section = `Info)
    ~model_query_name
    ~generated_models_count
    ()
  =
  Log.log ~section "Model Query `%s` generated %d models." model_query_name generated_models_count;
  let integers = ["models_generated", generated_models_count] in
  let normals = ["model_query_name", model_query_name] in
  let integers =
    match Sys.getenv "SANDCASTLE_INSTANCE_ID" with
    | Some sandcastle_instance_id ->
        ("sandcastle_instance_id", int_of_string sandcastle_instance_id) :: integers
    | None -> integers
  in
  let normals =
    match Sys.getenv "SANDCASTLE_ALIAS" with
    | Some sandcastle_alias -> ("sandcastle_alias", sandcastle_alias) :: normals
    | None -> normals
  in
  sample ~integers ~normals () |> log ~flush "perfpipe_pysa_dsl_model_query_output"


let log_exception caught_exception ~fatal ~origin =
  event
    ~section:`Error
    ~flush:true
    ~name:"uncaught exception"
    ~integers:[]
    ~normals:
      [
        ( "exception",
          Exn.to_string caught_exception |> fun message -> String.drop_suffix message 512 );
        "exception backtrace", Printexc.get_backtrace ();
        "exception origin", origin;
        ("fatal", if fatal then "true" else "false");
      ]
    ()


let buck_event ?(flush = false) ?(integers = []) ?(normals = []) () =
  let default_normals =
    [
      "host", GlobalState.hostname;
      "user", GlobalState.username;
      "project_root", GlobalState.global_state.project_root;
      "root", GlobalState.global_state.project_name;
    ]
  in
  let default_integers = ["time", Unix.time () |> Int.of_float] in
  format_as_json
    ~integers:(List.append default_integers integers)
    ~normals:(List.append default_normals normals)
    ()
  |> log ~flush "perfpipe_pyre_buck_events"


let log_worker_exception ~pid ~origin status =
  let message =
    match status with
    | Caml.Unix.WEXITED exit_code ->
        Printf.sprintf "Worker process %d exited with code %d" pid exit_code
    | Caml.Unix.WSTOPPED signal_number ->
        Printf.sprintf "Worker process %d was stopped by signal %d" pid signal_number
    | Caml.Unix.WSIGNALED signal_number ->
        Printf.sprintf "Worker process %d was kill by signal %d" pid signal_number
  in
  event
    ~section:`Error
    ~flush:true
    ~name:"Worker exited abnormally"
    ~integers:[]
    ~normals:
      [
        "exception", message;
        "exception backtrace", Printexc.get_backtrace ();
        "exception origin", origin;
        "fatal", "true";
      ]
    ()
