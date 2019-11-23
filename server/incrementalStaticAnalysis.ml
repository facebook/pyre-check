(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis

let compute_type_check_resolution ~configuration ~scheduler ~environment ~source_paths =
  (* Only compute type check resolutions for source paths that need it. *)
  let qualifiers = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
  Analysis.Check.analyze_sources
    qualifiers
    ~scheduler
    ~configuration:{ configuration with store_type_check_resolution = true }
    ~environment;

  (* Discard all type errors as we only want the resolution *)
  let qualifiers = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
  TypeEnvironment.invalidate environment qualifiers


let run_additional_check ~configuration ~scheduler ~environment ~source_paths ~check =
  compute_type_check_resolution ~configuration ~scheduler ~environment ~source_paths;
  match Analysis.Check.get_check_to_run ~check_name:check with
  | Some (module Check) ->
      let ast_environment =
        TypeEnvironment.global_environment environment
        |> AnnotatedGlobalEnvironment.ReadOnly.ast_environment
      in
      let qualifiers = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
      Analysis.Check.run_check ~configuration ~scheduler ~environment qualifiers (module Check);
      List.concat_map qualifiers ~f:(TypeEnvironment.get_errors environment)
      |> List.map
           ~f:
             (Analysis.Error.instantiate
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))
  | None ->
      Log.warning "No check corresponding to `%s` found." check;
      []
