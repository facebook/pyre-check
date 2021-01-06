(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open State
open Pyre

type error_reason =
  | StubShadowing
  | FileNotFound

type types_by_path = {
  path: PyrePath.t;
  types_by_location: (Location.t * Type.t) list option;
  error_reason: error_reason option;
}

type qualified_names_by_path = {
  path: PyrePath.t;
  qualified_names_by_location: (Location.t * Reference.t) list option;
  error_reason: error_reason option;
}

type lookup = {
  path: PyrePath.t;
  source_path: SourcePath.t option;
  lookup: Lookup.t option;
  error_reason: error_reason option;
}

let get_lookups ~configuration ~state:{ environment; _ } paths =
  let generate_lookup_for_existent_path (path, ({ SourcePath.qualifier; _ } as source_path)) =
    let lookup = Lookup.create_of_module (TypeEnvironment.read_only environment) qualifier in
    { path; source_path = Some source_path; lookup = Some lookup; error_reason = None }
  in
  let generate_lookup_for_nonexistent_path (path, error_reason) =
    { path; source_path = None; lookup = None; error_reason = Some error_reason }
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


let log_lookup ~handle ~position ~timer ~name ?(integers = []) ?(normals = []) () =
  let normals =
    let base_normals = ["handle", handle; "position", Location.show_position position] in
    base_normals @ normals
  in
  Statistics.performance
    ~section:`Event
    ~category:"perfpipe_pyre_ide_integration"
    ~name
    ~timer
    ~integers
    ~normals
    ()


let find_annotation ~state ~configuration ~path ~position =
  let timer = Timer.start () in
  let { lookup; source_path; _ } = get_lookups ~configuration ~state [path] |> List.hd_exn in
  let annotation = lookup >>= Lookup.get_annotation ~position in
  let _ =
    match source_path with
    | Some { SourcePath.relative = handle; _ } ->
        let normals =
          annotation
          >>| fun (location, annotation) ->
          ["resolved location", Location.show location; "resolved annotation", Type.show annotation]
        in
        log_lookup ~handle ~position ~timer ~name:"find annotation" ?normals ()
    | _ -> ()
  in
  annotation


let find_all_annotations_batch ~state ~configuration ~paths =
  let get_annotations { path; lookup; error_reason; _ } =
    let annotations =
      lookup >>| Lookup.get_all_annotations >>| List.sort ~compare:[%compare: Location.t * Type.t]
    in
    { path; types_by_location = annotations; error_reason }
  in
  List.map ~f:get_annotations (get_lookups ~configuration ~state paths)


let find_definition ~state ~configuration path position =
  let timer = Timer.start () in
  let { lookup; source_path; _ } = get_lookups ~configuration ~state [path] |> List.hd_exn in
  let definition = lookup >>= Lookup.get_definition ~position in
  let _ =
    match source_path with
    | Some { SourcePath.relative = handle; _ } ->
        let normals =
          definition >>| fun location -> ["resolved location", Location.show location]
        in
        log_lookup ~handle ~position ~timer ~name:"find definition" ?normals ()
    | _ -> ()
  in
  definition
