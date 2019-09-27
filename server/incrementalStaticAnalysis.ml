(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis

let compute_type_check_resolution ~configuration ~scheduler ~environment ~source_paths =
  (* Only compute type check resolutions for source paths that need it. *)
  let source_paths =
    let has_resolution_shared_memory { SourcePath.qualifier; _ } =
      ResolutionSharedMemory.get_keys ~qualifiers:[qualifier] |> List.is_empty |> not
    in
    List.filter source_paths ~f:(fun source_path -> not (has_resolution_shared_memory source_path))
  in
  List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
  |> Service.Check.analyze_sources
       ~scheduler
       ~configuration:{ configuration with store_type_check_resolution = true }
       ~environment
  |> ignore


let run_additional_check ~configuration ~scheduler ~environment ~source_paths ~check =
  compute_type_check_resolution ~configuration ~scheduler ~environment ~source_paths;
  match Analysis.Check.get_check_to_run ~check_name:check with
  | Some (module Check) ->
      let ast_environment =
        AnnotatedGlobalEnvironment.ReadOnly.class_metadata_environment environment
        |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
        |> ClassHierarchyEnvironment.ReadOnly.alias_environment
        |> AliasEnvironment.ReadOnly.unannotated_global_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment
      in
      let qualifiers = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
      Service.Check.run_check ~configuration ~scheduler ~environment qualifiers (module Check)
      |> List.map
           ~f:
             (Analysis.Error.instantiate
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))
  | None ->
      Log.warning "No check corresponding to `%s` found." check;
      []
