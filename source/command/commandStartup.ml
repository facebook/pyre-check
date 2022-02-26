(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* These configurations are essential in setting up global states and triggering full type checking. *)
module BaseConfiguration = struct
  type t = {
    (* Source file discovery *)
    source_paths: Configuration.SourcePaths.t;
    search_paths: SearchPath.t list;
    excludes: string list;
    checked_directory_allowlist: PyrePath.t list;
    checked_directory_blocklist: PyrePath.t list;
    extensions: Configuration.Extension.t list;
    (* Auxiliary paths *)
    log_path: PyrePath.t;
    global_root: PyrePath.t;
    local_root: PyrePath.t option;
    (* Type checking controls *)
    debug: bool;
    enable_type_comments: bool;
    python_version: Configuration.PythonVersion.t;
    (* Parallelism controls *)
    parallel: bool;
    number_of_workers: int;
    (* Memory controls *)
    shared_memory: Configuration.SharedMemory.t;
    (* Logging controls *)
    remote_logging: Configuration.RemoteLogging.t option;
    profiling_output: string option;
    memory_profiling_output: string option;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    (* Parsing logic *)
    try
      let source_paths =
        json
        |> member "source_paths"
        |> Configuration.SourcePaths.of_yojson
        |> Result.ok_or_failwith
      in
      let search_paths =
        json
        |> list_member
             "search_paths"
             ~f:(fun element -> to_string element |> SearchPath.create)
             ~default:[]
      in
      let excludes = json |> string_list_member "excludes" ~default:[] in
      let checked_directory_allowlist =
        json |> path_list_member "checked_directory_allowlist" ~default:[]
      in
      let checked_directory_blocklist =
        json |> path_list_member "checked_directory_blocklist" ~default:[]
      in
      let extensions =
        json
        |> string_list_member "extensions" ~default:[]
        |> List.map ~f:Configuration.Extension.create_extension
      in
      let log_path = json |> path_member "log_path" in
      let global_root = json |> path_member "global_root" in
      let local_root = json |> optional_path_member "local_root" in
      let debug = json |> bool_member "debug" ~default:false in
      let enable_type_comments = json |> bool_member "enable_type_comments" ~default:true in
      let python_version =
        json
        |> member "python_version"
        |> function
        | `Null -> Configuration.PythonVersion.default
        | _ as json -> Configuration.PythonVersion.of_yojson json |> Result.ok_or_failwith
      in
      let parallel = json |> bool_member "parallel" ~default:false in
      let number_of_workers = json |> int_member "number_of_workers" ~default:1 in
      let shared_memory =
        json
        |> member "shared_memory"
        |> function
        | `Null -> Configuration.SharedMemory.default
        | _ as json -> Configuration.SharedMemory.of_yojson json |> Result.ok_or_failwith
      in
      let remote_logging =
        json
        |> member "remote_logging"
        |> function
        | `Null -> None
        | _ as json ->
            Configuration.RemoteLogging.of_yojson json |> Result.ok_or_failwith |> Option.some
      in
      let profiling_output = json |> optional_string_member "profiling_output" in
      let memory_profiling_output = json |> optional_string_member "memory_profiling_output" in
      Result.Ok
        {
          source_paths;
          search_paths;
          excludes;
          checked_directory_allowlist;
          checked_directory_blocklist;
          extensions;
          log_path;
          global_root;
          local_root;
          debug;
          enable_type_comments;
          python_version;
          parallel;
          number_of_workers;
          shared_memory;
          remote_logging;
          profiling_output;
          memory_profiling_output;
        }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)
end

(* Read a JSON file from the given path. Return [Result.Ok json] if both file read and JSON parsing
   succeeds, and [Result.Error error_message] otherwise. *)
let read_json file_path =
  try Result.Ok (Yojson.Safe.from_file file_path) with
  | Yojson.Json_error message
  | Sys_error message ->
      Result.Error message
  | other_exception -> Result.Error (Exn.to_string other_exception)


(* [read_and_parse_json ~f file_path] reads a JSON out of [file_path] using {! read_json}, and
   invoke [f] on the result JSON. *)
let read_and_parse_json ~f file_path =
  match read_json file_path with
  | Result.Error message ->
      let message = Format.sprintf "Cannot read JSON file. %s" message in
      Result.Error message
  | Result.Ok json -> (
      match f json with
      | Result.Error message ->
          let message = Format.sprintf "Malformed server specification JSON. %s" message in
          Result.Error message
      | Result.Ok _ as result -> result)


(* A convenient wrapper to set up all relevant global states for a Pyre command. *)
let setup_global_states
    ~global_root
    ~local_root
    ~debug
    ~additional_logging_sections
    ~remote_logging
    ~profiling_output
    ~memory_profiling_output
    ()
  =
  Log.GlobalState.initialize ~debug ~sections:additional_logging_sections;
  let logger, log_identifier =
    match remote_logging with
    | None -> None, ""
    | Some { Configuration.RemoteLogging.logger; identifier } -> Some logger, identifier
  in
  let relative_local_root =
    match local_root with
    | None -> ""
    | Some local_root ->
        PyrePath.get_relative_to_root ~root:global_root ~path:local_root |> Option.value ~default:""
  in
  Statistics.GlobalState.initialize
    ~log_identifier
    ?logger
    ~project_root:(PyrePath.absolute global_root)
    ~project_name:relative_local_root
    ();
  Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ()
