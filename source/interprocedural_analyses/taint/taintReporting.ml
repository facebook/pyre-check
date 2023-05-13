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
module Json = Yojson.Safe

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


let issues_to_json ~taint_configuration ~fixpoint_state ~filename_lookup ~override_graph issues =
  let issue_to_json issue =
    let json =
      Issue.to_json
        ~taint_configuration
        ~expand_overrides:(Some override_graph)
        ~is_valid_callee:(has_significant_summary ~fixpoint_state)
        ~filename_lookup
        issue
    in
    `Assoc ["kind", `String "issue"; "data", json]
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
    ~map:(fun _ callables -> extract_errors callables)
    ~reduce:List.cons
    ~inputs:callables
    ()
  |> List.concat_no_order


let externalize
    ~taint_configuration
    ~fixpoint_state
    ~filename_lookup
    ~override_graph
    callable
    result
    model
  =
  let issues =
    issues_to_json ~taint_configuration ~fixpoint_state ~filename_lookup ~override_graph result
  in
  if not (Model.should_externalize model) then
    issues
  else
    Model.to_json
      ~expand_overrides:(Some override_graph)
      ~is_valid_callee:(has_significant_summary ~fixpoint_state)
      ~filename_lookup:(Some filename_lookup)
      ~export_leaf_names:Domains.ExportLeafNames.OnlyOnLeaves
      callable
      model
    :: issues


let fetch_and_externalize
    ~taint_configuration
    ~fixpoint_state
    ~filename_lookup
    ~override_graph
    callable
  =
  let model =
    TaintFixpoint.get_model fixpoint_state callable |> Option.value ~default:Model.empty_model
  in
  let result =
    TaintFixpoint.get_result fixpoint_state callable |> IssueHandle.SerializableMap.data
  in
  externalize
    ~taint_configuration
    ~fixpoint_state
    ~filename_lookup
    ~override_graph
    callable
    result
    model


let emit_externalization
    ~taint_configuration
    ~fixpoint_state
    ~filename_lookup
    ~override_graph
    emitter
    callable
  =
  fetch_and_externalize
    ~taint_configuration
    ~fixpoint_state
    ~filename_lookup
    ~override_graph
    callable
  |> List.iter ~f:emitter


type callable_shard = {
  shard_index: int;
  callables: Target.t list;
}

let save_results_to_directory
    ~scheduler
    ~taint_configuration
    ~result_directory
    ~output_format
    ~local_root
    ~filename_lookup
    ~override_graph
    ~skipped_overrides
    ~callables
    ~model_verification_errors
    ~fixpoint_state
    ~errors
  =
  let timer = Timer.start () in
  let root = local_root |> PyrePath.absolute in
  let open_file ~filename =
    let path = PyrePath.append result_directory ~element:filename in
    open_out (PyrePath.absolute path)
  in
  let save_models () =
    let write_header ~out_channel =
      `Assoc ["file_version", `Int 3; "config", `Assoc ["repo", `String root]]
      |> Json.to_channel out_channel;
      Printf.fprintf out_channel "\n"
    in
    let write_model ~out_channel callable =
      let emitter json =
        Json.to_channel out_channel json;
        Printf.fprintf out_channel "\n"
      in
      let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
      emit_externalization
        ~taint_configuration
        ~fixpoint_state
        ~filename_lookup
        ~override_graph
        emitter
        callable
    in
    match output_format with
    | Configuration.TaintOutputFormat.Json ->
        let out_channel = open_file ~filename:"taint-output.json" in
        write_header ~out_channel;
        Target.Set.iter (write_model ~out_channel) callables;
        close_out out_channel
    | Configuration.TaintOutputFormat.ShardedJson ->
        let shard_size =
          Int.max 1 (Target.Set.cardinal callables / Scheduler.number_workers scheduler)
        in
        let shards =
          callables
          |> Target.Set.elements
          |> List.chunks_of ~length:shard_size
          |> List.mapi ~f:(fun shard_index callables -> { shard_index; callables })
        in
        let number_shards = List.length shards in
        let write_json_shard { shard_index; callables } =
          let filename =
            Format.sprintf "taint-output@%05d-of-%05d.json" shard_index number_shards
          in
          let out_channel = open_file ~filename in
          write_header ~out_channel;
          List.iter ~f:(write_model ~out_channel) callables;
          close_out out_channel
        in
        Scheduler.map_reduce
          scheduler
          ~policy:(Scheduler.Policy.legacy_fixed_chunk_size 1)
          ~initial:()
          ~map:(fun () shards -> List.iter shards ~f:write_json_shard)
          ~reduce:(fun () () -> ())
          ~inputs:shards
          ()
  in
  let remove_existing_models () =
    if PyrePath.is_directory result_directory then
      PyrePath.read_directory_ordered result_directory
      |> List.filter ~f:(fun path ->
             let filename = PyrePath.last path in
             String_utils.string_starts_with filename "taint-output@"
             && String_utils.string_ends_with filename ".json")
      |> List.iter ~f:PyrePath.remove
  in
  let save_errors () =
    let out_channel = open_file ~filename:"errors.json" in
    Json.to_channel out_channel (`List errors);
    close_out out_channel
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
      Json.Util.combine global_statistics (statistics ~model_verification_errors)
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
        ]
    in
    Json.to_channel out_channel metadata_json;
    close_out out_channel
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
    ~filename_lookup
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
          "heap size", SharedMemory.heap_size ();
          "issues", List.length errors;
        ]
      ()
  in
  let error_to_json error =
    error
    |> Error.instantiate ~show_error_traces ~lookup:filename_lookup
    |> Error.Instantiated.to_yojson
  in
  let errors = List.map errors ~f:error_to_json in
  errors
