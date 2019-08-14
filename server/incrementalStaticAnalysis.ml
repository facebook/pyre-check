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
  let ast_environment = Environment.ast_environment environment in
  let sources =
    List.filter_map source_paths ~f:(fun { SourcePath.qualifier; _ } ->
        AstEnvironment.ReadOnly.get_source ast_environment qualifier)
  in
  Service.Check.analyze_sources
    ~scheduler
    ~configuration:{ configuration with store_type_check_resolution = true }
    ~environment
    sources
  |> ignore


let run_additional_check ~configuration ~scheduler ~environment ~source_paths ~check =
  compute_type_check_resolution ~configuration ~scheduler ~environment ~source_paths;
  match Analysis.Check.get_check_to_run ~check_name:check with
  | Some (module Check) ->
      let ast_environment = Environment.ast_environment environment in
      let sources =
        let to_source { SourcePath.qualifier; _ } =
          AstEnvironment.ReadOnly.get_source ast_environment qualifier
        in
        List.filter_map source_paths ~f:to_source
      in
      Service.Check.run_check ~configuration ~scheduler ~environment sources (module Check)
      |> List.map
           ~f:
             (Analysis.Error.instantiate
                ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment))
  | None ->
      Log.warning "No check corresponding to `%s` found." check;
      []
