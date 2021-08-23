(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = PyrePath

(* Analyze command uses the same exit code scheme as check command. *)
module ExitStatus = NewCheckCommand.ExitStatus

module AnalyzeConfiguration = struct
  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    dump_call_graph: bool;
    dump_model_query_results: bool;
    find_missing_flows: string option;
    inline_decorators: bool;
    maximum_tito_depth: int option;
    maximum_trace_length: int option;
    no_verify: bool;
    repository_root: Path.t option;
    rule_filter: int list option;
    save_results_to: Path.t option;
    strict: bool;
    taint_model_paths: Path.t list;
    use_cache: bool;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    (* Parsing logic *)
    try
      match NewCommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let dump_call_graph = bool_member "dump_call_graph" ~default:false json in
          let dump_model_query_results =
            bool_member "dump_model_query_results" ~default:false json
          in
          let find_missing_flows = optional_string_member "find_missing_flows" json in
          let inline_decorators = bool_member "inline_decorators" ~default:false json in
          let maximum_tito_depth = optional_int_member "maximum_tito_depth" json in
          let maximum_trace_length = optional_int_member "maximum_trace_length" json in
          let no_verify = bool_member "no_verify" ~default:false json in
          let repository_root = optional_path_member "repository_root" json in
          let rule_filter =
            member "rule_filter" json
            |> function
            | `Null -> None
            | _ as json -> Some (convert_each to_int json)
          in
          let save_results_to = optional_path_member "save_results_to" json in
          let strict = bool_member "strict" ~default:false json in
          let taint_model_paths = json |> path_list_member "taint_model_paths" ~default:[] in
          let use_cache = bool_member "use_cache" ~default:false json in

          Result.Ok
            {
              base;
              dump_call_graph;
              dump_model_query_results;
              find_missing_flows;
              inline_decorators;
              maximum_tito_depth;
              maximum_trace_length;
              no_verify;
              repository_root;
              rule_filter;
              save_results_to;
              strict;
              taint_model_paths;
              use_cache;
            }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)
end

let run_analyze _analyze_configuraiton =
  Log.warning "Coming soon...";
  Lwt.return ExitStatus.Ok


let run_analyze configuration_file =
  let exit_status =
    match
      NewCommandStartup.read_and_parse_json configuration_file ~f:AnalyzeConfiguration.of_yojson
    with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ({
           AnalyzeConfiguration.base =
             {
               NewCommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           _;
         } as analyze_configuration) ->
        NewCommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections:[]
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();
        Lwt_main.run
          (Lwt.catch (fun () -> run_analyze analyze_configuration) NewCheckCommand.on_exception)
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs taint analysis"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_analyze filename))
