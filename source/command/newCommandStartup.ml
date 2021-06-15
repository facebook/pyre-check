(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

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
      | Result.Ok _ as result -> result )


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
