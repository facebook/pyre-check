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
let has_significant_summary ~fixpoint_state ~trace_kind ~port:root ~path ~callee =
  match TaintFixpoint.State.ReadOnly.get_model fixpoint_state callee with
  | None -> false
  | Some { Model.forward; backward; _ } -> (
      match trace_kind with
      | Some Domains.TraceKind.Source ->
          let _, tree =
            Domains.ForwardState.read_tree_raw
              ~use_precise_labels:true
              ~root
              ~path
              forward.generations
          in
          let taint = Domains.ForwardState.Tree.get_root tree in
          not (Domains.ForwardTaint.is_bottom taint)
      | Some Domains.TraceKind.Sink ->
          let _, tree =
            Domains.BackwardState.read_tree_raw
              ~use_precise_labels:true
              ~root
              ~path
              backward.sink_taint
          in
          let taint = Domains.BackwardState.Tree.get_root tree in
          not (Domains.BackwardTaint.is_bottom taint)
      | None -> false)


let statistics ~model_verification_errors =
  let model_verification_errors =
    List.map ~f:ModelVerificationError.to_json model_verification_errors
  in
  `Assoc ["model_verification_errors", `List model_verification_errors]


(* Mapping from non-parameterized callables to all parameterized versions *)
module CallableToParameters : sig
  type t

  val from_callables : Target.t list -> t

  val get : t -> Target.t -> Target.t list
end = struct
  type t = Target.t list Target.Map.t

  let from_callables callables =
    let callables = List.filter ~f:Target.is_parameterized callables in
    List.fold
      ~init:Target.Map.empty
      ~f:(fun sofar target ->
        Target.Map.update
          (Target.strip_parameters target)
          (function
            | None -> Some [target]
            | Some existing -> Some (target :: existing))
          sofar)
      callables


  let get map target = Target.Map.find_opt target map |> Option.value ~default:[]
end

let merge_issues_ignoring_callable_parameters issues =
  (* We deduplicate issues in the following cases.
   * - Case 1: If `foo` has an issue with a non-parameterized sink callable `bar`, then there may
   * exist the same issue in `foo` with a parameterized `bar`.
   * - Case 2: If `foo` has an issue, then there may exist the same issue in a parameterized `foo`,
   * with exactly the same sink handle.
   * - Case 3: Derived from Case 1 and 2 -- if `foo` has an issue with a non-parameterized sink
   * callable `bar`, then there may exist the same issue in any parameterized `foo` with a
   * parameterized `bar`.
   * We realize the deduplication by using the "canonical" versions of the issue handles. *)
  let group_by_stripped_handle map issue =
    let issue =
      { issue with Issue.handle = IssueHandle.strip_all_callable_parameters issue.Issue.handle }
    in
    IssueHandle.SerializableMap.update
      issue.Issue.handle
      (function
        | None -> Some [issue]
        | Some existing_issue -> Some (issue :: existing_issue))
      map
  in
  issues
  |> List.fold ~init:IssueHandle.SerializableMap.empty ~f:group_by_stripped_handle
  |> IssueHandle.SerializableMap.data
  |> List.filter_map ~f:(function
         | [] -> None
         | [issue] -> Some issue
         | head :: tail ->
             (* Merge various sink traces and use a canonical issue handle. *)
             Some (Algorithms.fold_balanced tail ~f:Issue.join ~init:head))


let extract_errors ~scheduler ~scheduler_policies ~taint_configuration ~callables ~fixpoint_state =
  let extract_errors ~fixpoint_state callables =
    callables
    |> List.map ~f:(fun callable ->
           callable
           |> TaintFixpoint.State.ReadOnly.get_result fixpoint_state
           |> IssueHandle.SerializableMap.data)
    |> List.concat_no_order
  in
  let scheduler_policy =
    Scheduler.Policy.from_configuration_or_default
      scheduler_policies
      Configuration.ScheduleIdentifier.TaintCollectErrors
      ~default:
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:1
           ~preferred_chunk_size:2500
           ())
  in
  let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
  Scheduler.map_reduce
    scheduler
    ~policy:scheduler_policy
    ~initial:[]
    ~map:(extract_errors ~fixpoint_state:(TaintFixpoint.State.read_only fixpoint_state))
    ~reduce:List.cons
    ~inputs:callables
    ()
  |> List.concat_no_order
  |> merge_issues_ignoring_callable_parameters
  |> List.map ~f:(Issue.to_error ~taint_configuration)


let filter_overrides ~dump_override_models callables =
  if not dump_override_models then
    List.filter ~f:(fun callable -> not (Target.is_override callable)) callables
  else
    callables


let model_to_newline_delimited_json
    ~fixpoint_state
    ~resolve_module_path
    ~resolve_callable_location
    ~override_graph
    callable
  =
  let model =
    callable
    |> TaintFixpoint.State.ReadOnly.get_model fixpoint_state
    |> Option.value ~default:Model.empty_model
  in
  if not (Model.should_externalize model) then
    None
  else
    Some
      {
        NewlineDelimitedJson.Line.kind = Model;
        data =
          Model.to_json
            ~expand_overrides:(Some override_graph)
            ~is_valid_callee:(has_significant_summary ~fixpoint_state)
            ~resolve_module_path:(Some resolve_module_path)
            ~resolve_callable_location:(Some resolve_callable_location)
            ~export_leaf_names:Domains.ExportLeafNames.OnlyOnLeaves
            callable
            model;
      }


let issues_to_newline_delimited_json
    ~fixpoint_state
    ~taint_configuration
    ~resolve_module_path
    ~override_graph
    ~callable_to_parameters
    ~sorted
    callable
  =
  if Target.is_parameterized callable then
    [] (* Issues will be exported in the non-parameterized version *)
  else
    let fetch_issues callable =
      callable
      |> TaintFixpoint.State.ReadOnly.get_result fixpoint_state
      |> IssueHandle.SerializableMap.data
    in
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
    let callables = callable :: CallableToParameters.get callable_to_parameters callable in
    let issues =
      List.map ~f:fetch_issues callables
      |> List.concat_no_order
      |> merge_issues_ignoring_callable_parameters
    in
    let issues =
      if sorted then
        List.sort
          ~compare:(fun { Issue.handle = left; _ } { Issue.handle = right; _ } ->
            IssueHandle.deterministic_compare left right)
          issues
      else
        issues
    in
    List.map ~f:issue_to_json issues


let callable_to_newline_delimited_json
    ~fixpoint_state
    ~taint_configuration
    ~resolve_module_path
    ~resolve_callable_location
    ~override_graph
    ~callable_to_parameters
    ~sorted
    callable
  =
  let model =
    model_to_newline_delimited_json
      ~fixpoint_state
      ~resolve_module_path
      ~resolve_callable_location
      ~override_graph
      callable
  in
  let issues =
    issues_to_newline_delimited_json
      ~fixpoint_state
      ~taint_configuration
      ~resolve_module_path
      ~override_graph
      ~callable_to_parameters
      ~sorted
      callable
  in
  (* SAPP summary explorer expects issues for a callable to be stored next to the model for that
     callable. *)
  match model with
  | Some model -> model :: issues
  | None -> issues


let fetch_and_externalize
    ~taint_configuration
    ~fixpoint_state
    ~resolve_module_path
    ~resolve_callable_location
    ~override_graph
    ~dump_override_models
    ~sorted
    callables
  =
  let callables = filter_overrides ~dump_override_models callables in
  let callable_to_parameters = CallableToParameters.from_callables callables in
  callables
  |> List.map
       ~f:
         (callable_to_newline_delimited_json
            ~fixpoint_state
            ~taint_configuration
            ~resolve_module_path
            ~resolve_callable_location
            ~override_graph
            ~callable_to_parameters
            ~sorted)
  |> List.concat


let save_results_to_directory
    ~scheduler
    ~taint_configuration
    ~result_directory
    ~output_format
    ~local_root
    ~resolve_module_path
    ~resolve_callable_location
    ~override_graph
    ~skipped_overrides
    ~callables
    ~model_verification_errors
    ~fixpoint_state
    ~errors
    ~cache
    ~file_coverage
    ~rule_coverage
  =
  let step_logger =
    StepLogger.start
      ~start_message:
        (Format.sprintf "Writing analysis results to `%s`" (PyrePath.absolute result_directory))
      ~end_message:"Wrote analysis results"
      ()
  in
  let root = local_root |> PyrePath.absolute in
  let open_file ~filename =
    let path = PyrePath.append result_directory ~element:filename in
    Out_channel.create (PyrePath.absolute path)
  in
  let save_models () =
    let callables =
      callables |> Target.Set.elements |> filter_overrides ~dump_override_models:false
    in
    let callable_to_parameters = CallableToParameters.from_callables callables in
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    match output_format with
    | Configuration.TaintOutputFormat.Json ->
        NewlineDelimitedJson.write_file
          ~path:(PyrePath.append result_directory ~element:"taint-output.json")
          ~configuration:(`Assoc ["repo", `String root])
          ~to_json_lines:
            (callable_to_newline_delimited_json
               ~fixpoint_state
               ~taint_configuration
               ~resolve_module_path
               ~resolve_callable_location
               ~override_graph
               ~callable_to_parameters
               ~sorted:false)
          callables
    | Configuration.TaintOutputFormat.ShardedJson ->
        NewlineDelimitedJson.write_sharded_files
          ~scheduler
          ~directory:result_directory
          ~filename_prefix:"taint-output"
          ~configuration:(`Assoc ["repo", `String root])
          ~to_json_lines:
            (callable_to_newline_delimited_json
               ~fixpoint_state
               ~taint_configuration
               ~resolve_module_path
               ~resolve_callable_location
               ~override_graph
               ~callable_to_parameters
               ~sorted:false)
          callables
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
  let save_file_coverage () =
    if not (FileCoverage.is_empty file_coverage) then
      FileCoverage.write_to_file
        ~path:(PyrePath.append result_directory ~element:"file_coverage.txt")
        file_coverage
  in
  let save_rule_coverage () =
    if not (RuleCoverage.is_empty rule_coverage) then
      RuleCoverage.write_to_file
        ~path:(PyrePath.append result_directory ~element:"rule_coverage.json")
        rule_coverage
  in
  remove_existing_models ();
  save_models ();
  save_metadata ();
  save_errors ();
  save_file_coverage ();
  save_rule_coverage ();
  StepLogger.finish step_logger;
  ()


let produce_errors
    ~scheduler
    ~static_analysis_configuration:
      Configuration.StaticAnalysis.
        { configuration = { show_error_traces; _ }; scheduler_policies; _ }
    ~resolve_module_path
    ~taint_configuration
    ~callables
    ~fixpoint_step_logger
    ~fixpoint:{ TaintFixpoint.fixpoint_reached_iterations; state }
  =
  let errors =
    extract_errors
      ~taint_configuration
      ~scheduler
      ~scheduler_policies
      ~callables:(Target.Set.elements callables)
      ~fixpoint_state:state
  in
  (* Log and record stats *)
  let () = Log.info "Found %d issues" (List.length errors) in
  let () =
    StepLogger.finish
      ~integers:
        [
          "iterations", fixpoint_reached_iterations;
          "heap size", Hack_parallel.Std.SharedMemory.heap_size ();
          "issues", List.length errors;
        ]
      fixpoint_step_logger
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
