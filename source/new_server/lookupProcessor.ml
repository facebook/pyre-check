(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis

type error_reason =
  | StubShadowing
  | FileNotFound

type types_by_path = {
  path: PyrePath.t;
  types_by_location: ((Location.t * Type.t) list, error_reason) Result.t;
}

type lookup = {
  path: PyrePath.t;
  lookup: (Lookup.t, error_reason) Result.t;
}

let get_lookups ~configuration ~environment paths =
  let generate_lookup_for_existent_path (path, { SourcePath.qualifier; _ }) =
    let lookup = Lookup.create_of_module (TypeEnvironment.read_only environment) qualifier in
    { path; lookup = Result.Ok lookup }
  in
  let generate_lookup_for_nonexistent_path (path, error_reason) =
    { path; lookup = Result.Error error_reason }
  in
  let generate_lookup_for_path path =
    let module_tracker = TypeEnvironment.module_tracker environment in
    match ModuleTracker.lookup_path ~configuration module_tracker path with
    | ModuleTracker.PathLookup.Found source_path ->
        generate_lookup_for_existent_path (path, source_path)
    | ModuleTracker.PathLookup.ShadowedBy _ ->
        generate_lookup_for_nonexistent_path (path, StubShadowing)
    | ModuleTracker.PathLookup.NotFound -> generate_lookup_for_nonexistent_path (path, FileNotFound)
  in
  List.map paths ~f:generate_lookup_for_path


let find_all_annotations_batch ~environment ~configuration ~paths =
  let get_annotations { path; lookup; _ } =
    {
      path;
      types_by_location =
        Result.map lookup ~f:(fun lookup ->
            Lookup.get_all_annotations lookup |> List.sort ~compare:[%compare: Location.t * Type.t]);
    }
  in
  List.map ~f:get_annotations (get_lookups ~configuration ~environment paths)
