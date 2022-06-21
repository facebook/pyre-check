(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Pyre

type environment_data = {
  global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
  qualifiers: Ast.Reference.t list;
}

let build_environment_data
    ~configuration:
      ({ Configuration.Analysis.project_root; source_paths; search_paths; _ } as configuration)
    ()
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (PyrePath.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" PyrePath.pp directory))
  in
  source_paths |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  check_directory_exists project_root;
  search_paths |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;

  let global_environment =
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let annotated_global_environment =
      EnvironmentControls.create configuration |> AnnotatedGlobalEnvironment.create
    in
    Statistics.performance ~name:"full environment built" ~timer ();
    AnnotatedGlobalEnvironment.read_only annotated_global_environment
  in

  let qualifiers = AnnotatedGlobalEnvironment.ReadOnly.project_qualifiers global_environment in
  { global_environment; qualifiers }


let should_analyze_file ~paths_to_modify path =
  let matches item = PyrePath.equal item path || PyrePath.directory_contains ~directory:item path in
  List.exists paths_to_modify ~f:matches


let run_infer
    ~configuration
    ~scheduler
    ~filename_lookup
    ~paths_to_modify
    { global_environment; qualifiers }
  =
  Log.info "Running inference...";
  let timer = Timer.start () in
  let global_resolution = GlobalResolution.create global_environment in
  let ast_environment = GlobalResolution.ast_environment global_resolution in
  let should_analyze_qualifier =
    match paths_to_modify with
    | None -> fun _qualifier -> true
    | Some paths_to_modify ->
        fun qualifier ->
          qualifier
          |> filename_lookup
          >>| PyrePath.create_absolute
          >>| should_analyze_file ~paths_to_modify
          |> Option.value ~default:false
  in
  let qualifiers = qualifiers |> List.filter ~f:should_analyze_qualifier in
  let map _ qualifiers =
    let analyze_qualifier qualifier =
      let analyze_source source =
        TypeInference.Local.infer_for_module
          ~configuration
          ~global_resolution
          ~filename_lookup
          source
      in
      qualifier |> AstEnvironment.ReadOnly.get_processed_source ast_environment >>| analyze_source
    in
    qualifiers |> List.filter_map ~f:analyze_qualifier |> List.concat
  in
  let reduce left right = List.append left right in
  let results =
    Scheduler.map_reduce
      scheduler
      ~policy:(Scheduler.Policy.legacy_fixed_chunk_size 75)
      ~initial:[]
      ~map
      ~reduce
      ~inputs:qualifiers
      ()
  in
  Statistics.performance ~name:"inference" ~phase_name:"Type inference" ~timer ();
  TypeInference.Data.GlobalResult.from_local_results ~global_resolution results
