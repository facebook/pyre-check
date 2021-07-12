(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Pyre

type environment_data = {
  module_tracker: ModuleTracker.t;
  ast_environment: AstEnvironment.t;
  global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
  global_resolution: GlobalResolution.t;
  qualifiers: Ast.Reference.t list;
}

type result = {
  module_tracker: ModuleTracker.t;
  ast_environment: AstEnvironment.t;
  global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
  errors: Analysis.InferenceError.t list;
}

let build_environment_data
    ~configuration:
      ({ Configuration.Analysis.project_root; source_path; search_path; _ } as configuration)
    ~scheduler
    ()
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (Path.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
  in
  source_path |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  check_directory_exists project_root;
  search_path |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;

  let module_tracker = ModuleTracker.create configuration in
  let ast_environment = AstEnvironment.create module_tracker in
  let global_environment, qualifiers =
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let update_result =
      let annotated_global_environment = AnnotatedGlobalEnvironment.create ast_environment in
      AnnotatedGlobalEnvironment.update_this_and_all_preceding_environments
        annotated_global_environment
        ~scheduler
        ~configuration
        ColdStart
    in
    let global_environment = AnnotatedGlobalEnvironment.UpdateResult.read_only update_result in
    Statistics.performance ~name:"full environment built" ~timer ();
    let qualifiers =
      let is_not_external qualifier =
        let ast_environment = AstEnvironment.read_only ast_environment in
        AstEnvironment.ReadOnly.get_source_path ast_environment qualifier
        >>| (fun { Ast.SourcePath.is_external; _ } -> not is_external)
        |> Option.value ~default:false
      in
      AnnotatedGlobalEnvironment.UpdateResult.ast_environment_update_result update_result
      |> AstEnvironment.UpdateResult.invalidated_modules
      |> List.filter ~f:is_not_external
    in
    global_environment, qualifiers
  in
  let global_resolution = GlobalResolution.create global_environment in
  { module_tracker; ast_environment; global_environment; global_resolution; qualifiers }


let run_infer ~scheduler ~configuration ~global_resolution qualifiers =
  let number_of_sources = List.length qualifiers in
  Log.info "Running inference...";
  let timer = Timer.start () in
  let ast_environment = GlobalResolution.ast_environment global_resolution in
  let map _ qualifiers =
    let analyze_source (errors, number_files) source =
      let new_errors = Inference.run ~configuration ~global_resolution ~source in
      List.append new_errors errors, number_files + 1
    in
    List.filter_map qualifiers ~f:(AstEnvironment.ReadOnly.get_processed_source ast_environment)
    |> List.fold ~init:([], 0) ~f:analyze_source
  in
  let reduce (left_errors, left_number_files) (right_errors, right_number_files) =
    let number_files = left_number_files + right_number_files in
    Log.log ~section:`Progress "Processed %d of %d sources" number_files number_of_sources;
    List.append left_errors right_errors, number_files
  in
  let errors, _ =
    Scheduler.map_reduce
      scheduler
      ~policy:(Scheduler.Policy.legacy_fixed_chunk_size 75)
      ~initial:([], 0)
      ~map
      ~reduce
      ~inputs:qualifiers
      ()
  in
  Statistics.performance ~name:"inference" ~phase_name:"Type inference" ~timer ();
  errors


let infer ~configuration ~scheduler () =
  let { module_tracker; ast_environment; global_environment; global_resolution; qualifiers } =
    build_environment_data ~configuration ~scheduler ()
  in
  let errors = run_infer ~scheduler ~configuration ~global_resolution qualifiers in
  { module_tracker; ast_environment; global_environment; errors }


let run_infer_v2 ~scheduler ~configuration ~global_resolution qualifiers =
  Log.info "Running inference...";
  let timer = Timer.start () in
  let ast_environment = GlobalResolution.ast_environment global_resolution in
  let map _ qualifiers =
    let analyze_qualifier qualifier =
      let analyze_source source =
        TypeInference.Local.infer_for_module ~configuration ~global_resolution ~source
      in
      AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier >>| analyze_source
    in
    List.filter_map qualifiers ~f:analyze_qualifier |> List.concat
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
  results


let infer_v2 ~configuration ~scheduler () =
  let { global_resolution; qualifiers; _ } = build_environment_data ~configuration ~scheduler () in
  run_infer_v2 ~scheduler ~configuration ~global_resolution qualifiers
  |> TypeInference.Data.GlobalResult.from_local_results ~global_resolution
