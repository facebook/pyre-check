(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Pyre

type result = {
  module_tracker: ModuleTracker.t;
  ast_environment: AstEnvironment.t;
  environment: TypeEnvironment.t;
  errors: Error.t list;
}

let run_infer ~scheduler ~configuration ~environment qualifiers =
  let number_of_sources = List.length qualifiers in
  Log.info "Running inference...";
  let timer = Timer.start () in
  let ast_environment = TypeEnvironment.ast_environment environment in
  let map _ qualifiers =
    AttributeResolution.AttributeCache.clear ();
    let analyze_source number_files source =
      Inference.run ~configuration ~environment ~source;
      number_files + 1
    in
    List.filter_map qualifiers ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
    |> List.fold ~init:0 ~f:analyze_source
  in
  let reduce left right =
    let number_files = left + right in
    Log.log ~section:`Progress "Processed %d of %d sources" number_files number_of_sources;
    number_files
  in
  let _ =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~bucket_size:75
      ~initial:0
      ~map
      ~reduce
      ~inputs:qualifiers
      ()
  in
  Statistics.performance ~name:"inference" ~timer ();
  List.concat_map qualifiers ~f:(TypeEnvironment.get_errors environment)


let infer
    ~configuration:
      ({ Configuration.Analysis.project_root; local_root; search_path; _ } as configuration)
    ()
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (Path.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
  in
  check_directory_exists local_root;
  check_directory_exists project_root;
  search_path |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  let scheduler = Scheduler.create ~configuration ~bucket_multiplier:10 () in

  let module_tracker = ModuleTracker.create configuration in
  let ast_environment = AstEnvironment.create module_tracker in
  let ast_environment_update_result =
    AstEnvironment.update ~scheduler ~configuration ast_environment ColdStart
  in
  let qualifiers = AstEnvironment.UpdateResult.reparsed ast_environment_update_result in
  let environment =
    let ast_environment = AstEnvironment.read_only ast_environment in
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let update_result =
      AnnotatedGlobalEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        (Ast.Reference.Set.of_list qualifiers)
    in
    let global_environment = AnnotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let environment = TypeEnvironment.create global_environment in
    AttributeResolution.AttributeCache.clear ();
    Statistics.performance ~name:"full environment built" ~timer ();
    environment
  in
  let errors =
    let qualifiers =
      let is_not_external qualifier =
        let ast_environment = AstEnvironment.read_only ast_environment in
        AstEnvironment.ReadOnly.get_source_path ast_environment qualifier
        >>| (fun { Ast.SourcePath.is_external; _ } -> not is_external)
        |> Option.value ~default:false
      in
      List.filter qualifiers ~f:is_not_external
    in
    run_infer ~scheduler ~configuration ~environment qualifiers
  in
  { module_tracker; ast_environment; environment; errors }
