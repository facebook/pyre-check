(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Interprocedural
module Json = Yojson.Safe

(* Patch the forward reference to access the final summaries in trace info generation. *)
let has_significant_summary ~fixpoint_state ~port:root ~path ~callee =
  match Fixpoint.get_model fixpoint_state callee with
  | None -> false
  | Some { Model.forward; backward; _ } -> (
      match root with
      | AccessPath.Root.LocalResult ->
          let _, tree =
            Domains.ForwardState.read_tree_raw
              ~use_precise_labels:true
              ~root
              ~path
              forward.source_taint
          in
          let taint = Domains.ForwardState.Tree.get_root tree in
          not (Domains.ForwardTaint.is_bottom taint)
      | _ ->
          let _, tree =
            Domains.BackwardState.read_tree_raw
              ~use_precise_labels:true
              ~root
              ~path
              backward.sink_taint
          in
          let taint = Domains.BackwardState.Tree.get_root tree in
          not (Domains.BackwardTaint.is_bottom taint))


let issues_to_json ~fixpoint_state ~filename_lookup ~override_graph issues =
  let issue_to_json issue =
    let json =
      Issue.to_json
        ~expand_overrides:(Some override_graph)
        ~is_valid_callee:(has_significant_summary ~fixpoint_state)
        ~filename_lookup
        issue
    in
    `Assoc ["kind", `String "issue"; "data", json]
  in
  List.map ~f:issue_to_json issues


let metadata () =
  let codes = Issue.code_metadata () in
  `Assoc ["codes", codes]


let statistics () =
  let model_verification_errors =
    ModelVerificationError.get () |> List.map ~f:ModelVerificationError.to_json
  in
  `Assoc ["model_verification_errors", `List model_verification_errors]


let extract_errors ~scheduler ~callables ~fixpoint_state =
  let extract_errors callables =
    List.map
      ~f:(fun callable -> Fixpoint.get_result fixpoint_state callable |> List.map ~f:Issue.to_error)
      callables
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


let externalize ~fixpoint_state ~filename_lookup ~override_graph callable result model =
  let issues = issues_to_json ~fixpoint_state ~filename_lookup ~override_graph result in
  if not (Model.should_externalize model) then
    issues
  else
    Model.to_json
      ~expand_overrides:(Some override_graph)
      ~is_valid_callee:(has_significant_summary ~fixpoint_state)
      ~filename_lookup:(Some filename_lookup)
      callable
      model
    :: issues


let fetch_and_externalize ~fixpoint_state ~filename_lookup ~override_graph callable =
  let model =
    Fixpoint.get_model fixpoint_state callable |> Option.value ~default:Model.empty_model
  in
  let result = Fixpoint.get_result fixpoint_state callable in
  externalize ~fixpoint_state ~filename_lookup ~override_graph callable result model


let emit_externalization ~fixpoint_state ~filename_lookup ~override_graph emitter callable =
  fetch_and_externalize ~fixpoint_state ~filename_lookup ~override_graph callable
  |> List.iter ~f:emitter


let save_results_to_directory
    ~result_directory
    ~local_root
    ~filename_lookup
    ~override_graph
    ~skipped_overrides
    ~callables
    ~fixpoint_state
    ~errors
  =
  let emit_json_array_elements out_buffer =
    let seen_element = ref false in
    fun json ->
      if !seen_element then (
        Bi_outbuf.add_string out_buffer "\n";
        Json.to_outbuf out_buffer json)
      else (
        seen_element := true;
        Json.to_outbuf out_buffer json)
  in
  let timer = Timer.start () in
  let models_path analysis_name = Format.sprintf "%s-output.json" analysis_name in
  let root = local_root |> PyrePath.absolute in
  let save_models () =
    let filename = "taint-output.json" in
    let output_path = PyrePath.append result_directory ~element:filename in
    let out_channel = open_out (PyrePath.absolute output_path) in
    let out_buffer = Bi_outbuf.create_channel_writer out_channel in
    let array_emitter = emit_json_array_elements out_buffer in
    let header_with_version =
      `Assoc ["file_version", `Int 3; "config", `Assoc ["repo", `String root]]
    in
    Json.to_outbuf out_buffer header_with_version;
    Bi_outbuf.add_string out_buffer "\n";
    Target.Set.iter
      (emit_externalization ~fixpoint_state ~filename_lookup ~override_graph array_emitter)
      callables;
    Bi_outbuf.flush_output_writer out_buffer;
    close_out out_channel
  in
  let save_errors () =
    let filename = "errors.json" in
    let output_path = PyrePath.append result_directory ~element:filename in
    let out_channel = open_out (PyrePath.absolute output_path) in
    let out_buffer = Bi_outbuf.create_channel_writer out_channel in
    Json.to_outbuf out_buffer (`List errors);
    Bi_outbuf.flush_output_writer out_buffer;
    close_out out_channel
  in
  let save_metadata () =
    let filename = "taint-metadata.json" in
    let output_path = PyrePath.append result_directory ~element:filename in
    let out_channel = open_out (PyrePath.absolute output_path) in
    let out_buffer = Bi_outbuf.create_channel_writer out_channel in
    let filename_spec = models_path "taint" in
    let statistics =
      let global_statistics =
        `Assoc
          [
            ( "skipped_overrides",
              `List
                (List.map skipped_overrides ~f:(fun override ->
                     `String (Target.show_pretty override))) );
          ]
      in
      Json.Util.combine global_statistics (statistics ())
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
    let analysis_metadata = metadata () in
    Json.Util.combine toplevel_metadata analysis_metadata |> Json.to_outbuf out_buffer;
    Bi_outbuf.flush_output_writer out_buffer;
    close_out out_channel
  in
  save_models ();
  save_metadata ();
  save_errors ();
  Log.info "Analysis results were written to `%s`." (PyrePath.absolute result_directory);
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
    ~override_graph
    ~callables
    ~skipped_overrides
    ~fixpoint_timer
    ~fixpoint_state
  =
  let errors =
    extract_errors ~scheduler ~callables:(Target.Set.elements callables) ~fixpoint_state
  in
  (* Log and record stats *)
  let () = Log.info "Found %d issues" (List.length errors) in
  let iterations = Fixpoint.get_iterations fixpoint_state in
  let () = Log.info "Fixpoint iterations: %d" iterations in
  let () =
    Statistics.performance
      ~name:"Analysis fixpoint complete"
      ~phase_name:"Static analysis fixpoint"
      ~timer:fixpoint_timer
      ~integers:
        [
          "pysa fixpoint iterations", iterations;
          "pysa heap size", SharedMemory.heap_size ();
          "pysa issues", List.length errors;
        ]
      ()
  in
  (* Dump results to output directory if one was provided, and return a list of json (empty whenever
     we dumped to a directory) to summarize *)
  let error_to_json error =
    error
    |> Interprocedural.Error.instantiate ~show_error_traces ~lookup:filename_lookup
    |> Interprocedural.Error.Instantiated.to_yojson
  in
  let errors = List.map errors ~f:error_to_json in
  match result_json_path with
  | Some result_directory ->
      save_results_to_directory
        ~result_directory
        ~local_root
        ~filename_lookup
        ~override_graph
        ~skipped_overrides
        ~callables
        ~fixpoint_state
        ~errors;
      []
  | _ -> errors
