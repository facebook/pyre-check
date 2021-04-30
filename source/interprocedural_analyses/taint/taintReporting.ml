(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Interprocedural
module Json = Yojson.Safe

let get_model callable = Fixpoint.get_model callable >>= Result.get_model TaintResult.kind

let get_result callable = Fixpoint.get_result callable |> Result.get_result TaintResult.kind

let extract_errors scheduler callables =
  let extract_errors callables =
    List.filter_map ~f:(fun callable -> get_result callable >>| TaintResult.get_errors) callables
    |> List.concat_no_order
  in
  Scheduler.map_reduce
    scheduler
    ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
    ~initial:[]
    ~map:(fun _ callables -> extract_errors callables)
    ~reduce:List.cons
    ~inputs:callables
    ()
  |> List.concat_no_order


let externalize ~filename_lookup callable =
  let model = get_model callable |> Option.value ~default:TaintResult.empty_model in
  let result_option = get_result callable in
  TaintResult.externalize ~filename_lookup callable result_option model


let emit_externalization ~filename_lookup emitter callable =
  externalize ~filename_lookup callable |> List.iter ~f:emitter


let save_results_to_directory
    ~result_directory
    ~local_root
    ~filename_lookup
    ~skipped_overrides
    ~callables
  =
  let emit_json_array_elements out_buffer =
    let seen_element = ref false in
    fun json ->
      if !seen_element then (
        Bi_outbuf.add_string out_buffer "\n";
        Json.to_outbuf out_buffer json )
      else (
        seen_element := true;
        Json.to_outbuf out_buffer json )
  in
  let timer = Timer.start () in
  let models_path analysis_name = Format.sprintf "%s-output.json" analysis_name in
  let root = local_root |> Path.absolute in
  let save_models () =
    let filename = models_path TaintResult.name in
    let output_path = Path.append result_directory ~element:filename in
    let out_channel = open_out (Path.absolute output_path) in
    let out_buffer = Bi_outbuf.create_channel_writer out_channel in
    let array_emitter = emit_json_array_elements out_buffer in
    let header_with_version =
      `Assoc ["file_version", `Int 2; "config", `Assoc ["repo", `String root]]
    in
    Json.to_outbuf out_buffer header_with_version;
    Bi_outbuf.add_string out_buffer "\n";
    Callable.Set.iter (emit_externalization ~filename_lookup array_emitter) callables;
    Bi_outbuf.flush_output_writer out_buffer;
    close_out out_channel
  in
  let save_metadata () =
    let filename = Format.sprintf "%s-metadata.json" TaintResult.name in
    let output_path = Path.append result_directory ~element:filename in
    let out_channel = open_out (Path.absolute output_path) in
    let out_buffer = Bi_outbuf.create_channel_writer out_channel in
    let filename_spec = models_path TaintResult.name in
    let statistics =
      let global_statistics =
        `Assoc
          [
            ( "skipped_overrides",
              `List
                (List.map skipped_overrides ~f:(fun override -> `String (Reference.show override)))
            );
          ]
      in
      Json.Util.combine global_statistics (TaintResult.statistics ())
    in
    let toplevel_metadata =
      `Assoc
        [
          "filename_spec", `String filename_spec;
          "root", `String root;
          "tool", `String "pysa";
          "version", `String (Version.version ());
          "stats", statistics;
        ]
    in
    let analysis_metadata = TaintResult.metadata () in
    Json.Util.combine toplevel_metadata analysis_metadata |> Json.to_outbuf out_buffer;
    Bi_outbuf.flush_output_writer out_buffer;
    close_out out_channel
  in
  save_models ();
  save_metadata ();
  Log.info "Analysis results were written to `%s`." (Path.absolute result_directory);
  Statistics.performance
    ~name:"Wrote analysis results"
    ~phase_name:"Writing analysis results"
    ~timer
    ()


let report
    ~scheduler
    ~static_analysis_configuration:
      {
        Configuration.StaticAnalysis.result_json_path;
        configuration = { local_root; show_error_traces; _ };
        _;
      }
    ~filename_lookup
    ~callables
    ~skipped_overrides
    ~fixpoint_timer
    ~fixpoint_iterations
  =
  let errors = extract_errors scheduler (Callable.Set.elements callables) in
  (* Log and record stats *)
  Log.info "Found %d issues" (List.length errors);
  ( match fixpoint_iterations with
  | Some iterations ->
      Log.info "Fixpoint iterations: %d" iterations;
      Statistics.performance
        ~name:"Analysis fixpoint complete"
        ~phase_name:"Static analysis fixpoint"
        ~timer:fixpoint_timer
        ~integers:
          [
            "pysa fixpoint iterations", iterations;
            "pysa heap size", SharedMem.heap_size ();
            "pysa issues", List.length errors;
          ]
        ()
  | None -> () );
  (* Dump results to output directory if one was provided, and return a list of json (empty whenever
     we dumped to a directory) to summarize *)
  match result_json_path with
  | Some result_directory ->
      save_results_to_directory
        ~result_directory
        ~local_root
        ~filename_lookup
        ~skipped_overrides
        ~callables;
      []
  | _ ->
      let error_to_json error =
        error
        |> Interprocedural.Error.instantiate ~show_error_traces ~lookup:filename_lookup
        |> Interprocedural.Error.Instantiated.to_yojson
      in
      List.map errors ~f:error_to_json
