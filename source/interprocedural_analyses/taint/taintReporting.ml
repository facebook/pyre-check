(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TaintReporting: implements the logic that writes the result of the taint
 * analysis to a directory. *)

open Core
open Pyre
open Interprocedural

(* Patch the forward reference to access the final summaries in trace info generation. *)
let has_significant_summary ~fixpoint_state ~port:root ~path ~callee =
  match TaintFixpoint.get_model fixpoint_state callee with
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


let issues_to_json ~taint_configuration ~fixpoint_state ~resolve_module_path ~override_graph issues =
  let issue_to_json issue =
    let json =
      Issue.to_json
        ~taint_configuration
        ~expand_overrides:(Some override_graph)
        ~is_valid_callee:(has_significant_summary ~fixpoint_state)
        ~resolve_module_path
        issue
    in
    { NewlineDelimitedJson.Line.kind = Issue; data = json }
  in
  List.map ~f:issue_to_json issues


let statistics ~model_verification_errors =
  let model_verification_errors =
    List.map ~f:ModelVerificationError.to_json model_verification_errors
  in
  `Assoc ["model_verification_errors", `List model_verification_errors]


let extract_errors ~scheduler ~taint_configuration ~callables ~fixpoint_state =
  let extract_errors callables =
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    List.map
      ~f:(fun callable ->
        TaintFixpoint.get_result fixpoint_state callable
        |> IssueHandle.SerializableMap.data
        |> List.map ~f:(Issue.to_error ~taint_configuration))
      callables
    |> List.concat_no_order
  in
  Scheduler.map_reduce
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_size
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:100
         ~preferred_chunk_size:2500
         ())
    ~initial:[]
    ~map:extract_errors
    ~reduce:List.cons
    ~inputs:callables
    ()
  |> List.concat_no_order


let externalize
    ~taint_configuration
    ~fixpoint_state
    ~resolve_module_path
    ~override_graph
    callable
    result
    model
  =
  let issues =
    issues_to_json ~taint_configuration ~fixpoint_state ~resolve_module_path ~override_graph result
  in
  if not (Model.should_externalize model) then
    issues
  else
    {
      NewlineDelimitedJson.Line.kind = Model;
      data =
        Model.to_json
          ~expand_overrides:(Some override_graph)
          ~is_valid_callee:(has_significant_summary ~fixpoint_state)
          ~resolve_module_path:(Some resolve_module_path)
          ~export_leaf_names:Domains.ExportLeafNames.OnlyOnLeaves
          callable
          model;
    }
    :: issues


let fetch_and_externalize
    ~taint_configuration
    ~fixpoint_state
    ~resolve_module_path
    ~override_graph
    ~dump_override_models
  = function
  | Target.Override _ when not dump_override_models -> []
  | callable ->
      let model =
        TaintFixpoint.get_model fixpoint_state callable |> Option.value ~default:Model.empty_model
      in
      let result =
        TaintFixpoint.get_result fixpoint_state callable |> IssueHandle.SerializableMap.data
      in
      externalize
        ~taint_configuration
        ~fixpoint_state
        ~resolve_module_path
        ~override_graph
        callable
        result
        model


let save_results_to_directory
    ~scheduler
    ~taint_configuration
    ~result_directory
    ~output_format
    ~local_root
    ~resolve_module_path
    ~override_graph
    ~skipped_overrides
    ~callables
    ~model_verification_errors
    ~fixpoint_state
    ~errors
    ~cache
  =
  let timer = Timer.start () in
  let root = local_root |> PyrePath.absolute in
  let open_file ~filename =
    let path = PyrePath.append result_directory ~element:filename in
    Out_channel.create (PyrePath.absolute path)
  in
  let save_models () =
    let model_to_json callable =
      let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
      fetch_and_externalize
        ~taint_configuration
        ~fixpoint_state
        ~resolve_module_path
        ~override_graph
        ~dump_override_models:false
        callable
    in
    match output_format with
    | Configuration.TaintOutputFormat.Json ->
        NewlineDelimitedJson.write_file
          ~path:(PyrePath.append result_directory ~element:"taint-output.json")
          ~configuration:(`Assoc ["repo", `String root])
          ~to_json_lines:model_to_json
          (Target.Set.elements callables)
    | Configuration.TaintOutputFormat.ShardedJson ->
        NewlineDelimitedJson.write_sharded_files
          ~scheduler
          ~directory:result_directory
          ~filename_prefix:"taint-output"
          ~configuration:(`Assoc ["repo", `String root])
          ~to_json_lines:model_to_json
          (Target.Set.elements callables)
  in
  let remove_existing_models () =
    NewlineDelimitedJson.remove_sharded_files
      ~directory:result_directory
      ~filename_prefix:"taint-output"
  in
  let save_errors () =
    let out_channel = open_file ~filename:"errors.json" in
    Yojson.Safe.to_channel out_channel (`List errors);
    Out_channel.close out_channel
  in
  let save_metadata () =
    let out_channel = open_file ~filename:"taint-metadata.json" in
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
      Yojson.Safe.Util.combine global_statistics (statistics ~model_verification_errors)
    in
    let metadata_json =
      `Assoc
        [
          ( "filename_spec",
            match output_format with
            | Configuration.TaintOutputFormat.Json -> `String "taint-output.json"
            | Configuration.TaintOutputFormat.ShardedJson -> `String "taint-output@*.json" );
          "root", `String root;
          "tool", `String "pysa";
          "version", `String (Version.version ());
          "stats", statistics;
          ( "codes",
            taint_configuration
            |> TaintConfiguration.SharedMemory.get
            |> TaintConfiguration.code_metadata );
          "cache", Cache.metadata_to_json cache;
        ]
    in
    Yojson.Safe.to_channel out_channel metadata_json;
    Out_channel.close out_channel
  in
  remove_existing_models ();
  save_models ();
  save_metadata ();
  save_errors ();
  Log.info "Analysis results were written to `%s`." (PyrePath.absolute result_directory);
  Statistics.performance
    ~name:"Wrote analysis results"
    ~phase_name:"Writing analysis results"
    ~timer
    ()


let produce_errors
    ~scheduler
    ~static_analysis_configuration:
      Configuration.StaticAnalysis.{ configuration = { show_error_traces; _ }; _ }
    ~resolve_module_path
    ~taint_configuration
    ~callables
    ~fixpoint_timer
    ~fixpoint_state
  =
  let errors =
    extract_errors
      ~taint_configuration
      ~scheduler
      ~callables:(Target.Set.elements callables)
      ~fixpoint_state
  in
  (* Log and record stats *)
  let () = Log.info "Found %d issues" (List.length errors) in
  let iterations = TaintFixpoint.get_iterations fixpoint_state in
  let () =
    Statistics.performance
      ~name:"Analysis fixpoint complete"
      ~phase_name:"Static analysis fixpoint"
      ~timer:fixpoint_timer
      ~integers:
        [
          "iterations", iterations;
          "heap size", Hack_parallel.Std.SharedMemory.heap_size ();
          "issues", List.length errors;
        ]
      ()
  in
  let filename_lookup qualifier =
    resolve_module_path qualifier >>= fun { RepositoryPath.filename; _ } -> filename
  in
  let error_to_json error =
    error
    |> Error.instantiate ~show_error_traces ~lookup:filename_lookup
    |> Error.Instantiated.to_yojson
  in
  let errors = List.map errors ~f:error_to_json in
  errors
