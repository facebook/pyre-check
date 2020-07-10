(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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

type lookup = {
  path: PyrePath.t;
  source_path: SourcePath.t option;
  lookup: Lookup.t option;
  error_reason: error_reason option;
}

let get_lookups ~configuration ~state:{ lookups; environment; _ } paths =
  let paths, nonexistent_paths =
    let get_source_path path =
      let module_tracker = TypeEnvironment.module_tracker environment in
      match Analysis.ModuleTracker.lookup_path ~configuration module_tracker path with
      | Some source_path -> Either.First (path, source_path)
      | None when PyrePath.is_python_stub path -> Either.Second (path, FileNotFound)
      | None ->
          let error_reason =
            match
              Analysis.ModuleTracker.lookup_path
                ~configuration
                module_tracker
                (PyrePath.with_suffix path ~suffix:"i")
            with
            | Some _ -> StubShadowing
            | None -> FileNotFound
          in
          Either.Second (path, error_reason)
    in
    List.partition_map ~f:get_source_path paths
  in
  let cache_hits, cache_misses =
    let retrieve_cached (path, ({ SourcePath.qualifier; _ } as source_path)) =
      let cache_read = String.Table.find lookups (Reference.show qualifier) in
      match cache_read with
      | Some lookup ->
          Either.First
            { path; source_path = Some source_path; lookup = Some lookup; error_reason = None }
      | None -> Either.Second (path, source_path)
    in
    List.partition_map ~f:retrieve_cached paths
  in
  let generate_lookups paths =
    let generate_lookup (path, ({ SourcePath.qualifier; _ } as source_path)) =
      let lookup = Lookup.create_of_module (TypeEnvironment.read_only environment) qualifier in
      String.Table.set lookups ~key:(Reference.show qualifier) ~data:lookup;
      { path; source_path = Some source_path; lookup = Some lookup; error_reason = None }
    in
    List.map ~f:generate_lookup paths
  in
  let nonexistents =
    List.map nonexistent_paths ~f:(fun (path, error_reason) ->
        { path; source_path = None; lookup = None; error_reason = Some error_reason })
  in
  nonexistents @ cache_hits @ generate_lookups cache_misses


let evict ~lookups reference = String.Table.remove lookups (Reference.show reference)

let evict_path ~state:{ State.environment; lookups; _ } ~configuration path =
  let module_tracker = TypeEnvironment.module_tracker environment in
  match Analysis.ModuleTracker.lookup_path ~configuration module_tracker path with
  | None -> ()
  | Some { SourcePath.qualifier; _ } -> evict ~lookups qualifier


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
