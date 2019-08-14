(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open State
open Pyre

type types_by_path = {
  path: PyrePath.t;
  types_by_location: (Location.Instantiated.t * Type.t) list option;
}

type lookup = {
  path: PyrePath.t;
  source_path: SourcePath.t option;
  lookup: Lookup.t option;
}

let instantiate_location ~state:{ State.environment; _ } =
  let ast_environment = Environment.ast_environment environment in
  Location.instantiate ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment)


let get_lookups
    ~configuration:({ Configuration.Analysis.store_type_check_resolution; _ } as configuration)
    ~state:{ lookups; module_tracker; environment; scheduler; _ }
    paths
  =
  let paths, nonexistent_paths =
    let get_source_path path =
      match Analysis.ModuleTracker.lookup_path ~configuration module_tracker path with
      | Some source_path -> `Fst (path, source_path)
      | None -> `Snd path
    in
    List.partition_map ~f:get_source_path paths
  in
  let cache_hits, cache_misses =
    let retrieve_cached (path, ({ SourcePath.qualifier; _ } as source_path)) =
      let cache_read = String.Table.find lookups (Reference.show qualifier) in
      match cache_read with
      | Some lookup -> `Fst { path; source_path = Some source_path; lookup = Some lookup }
      | None -> `Snd (path, source_path)
    in
    List.partition_map ~f:retrieve_cached paths
  in
  let generate_lookups paths =
    let generate_lookup (path, ({ SourcePath.qualifier; _ } as source_path)) =
      let lookup =
        let global_resolution = Environment.resolution environment () in
        let ast_environment = Environment.ast_environment environment in
        AstEnvironment.ReadOnly.get_source ast_environment qualifier
        >>| Lookup.create_of_source global_resolution
      in
      lookup
      >>| (fun lookup -> String.Table.set lookups ~key:(Reference.show qualifier) ~data:lookup)
      |> ignore;
      { path; source_path = Some source_path; lookup }
    in
    let source_paths = List.map paths ~f:(fun (_, source_path) -> source_path) in
    if not store_type_check_resolution then (
      IncrementalStaticAnalysis.compute_type_check_resolution
        ~configuration
        ~scheduler
        ~environment
        ~source_paths;
      let lookups = List.map ~f:generate_lookup paths in
      ResolutionSharedMemory.remove
        (List.map ~f:(fun { SourcePath.qualifier; _ } -> qualifier) source_paths);
      lookups )
    else
      List.map ~f:generate_lookup paths
  in
  let nonexistents =
    List.map nonexistent_paths ~f:(fun path -> { path; source_path = None; lookup = None })
  in
  nonexistents @ cache_hits @ generate_lookups cache_misses


let evict ~state:{ lookups; _ } reference = String.Table.remove lookups (Reference.show reference)

let evict_path ~state:({ State.module_tracker; _ } as state) ~configuration path =
  match Analysis.ModuleTracker.lookup_path ~configuration module_tracker path with
  | None -> ()
  | Some { SourcePath.qualifier; _ } -> evict ~state qualifier


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
  let annotation =
    lookup
    >>= Lookup.get_annotation ~position
    >>| fun (location, annotation) -> instantiate_location ~state location, annotation
  in
  let _ =
    match source_path with
    | Some { SourcePath.relative = handle; _ } ->
        let normals =
          annotation
          >>| fun (location, annotation) ->
          [ "resolved location", Location.Instantiated.show location;
            "resolved annotation", Type.show annotation ]
        in
        log_lookup ~handle ~position ~timer ~name:"find annotation" ?normals ()
    | _ -> ()
  in
  annotation


let find_all_annotations ~state ~configuration ~path =
  let timer = Timer.start () in
  let { lookup; source_path; _ } = get_lookups ~configuration ~state [path] |> List.hd_exn in
  let annotations =
    lookup
    >>| Lookup.get_all_annotations
    >>| List.map ~f:(fun (location, annotation) ->
            instantiate_location ~state location, annotation)
  in
  let _ =
    match source_path, annotations with
    | Some { SourcePath.relative = handle; _ }, Some annotations ->
        let integers = ["annotation list size", List.length annotations] in
        log_lookup
          ~handle
          ~position:Location.any_position
          ~timer
          ~name:"find all annotations"
          ~integers
          ()
    | _ -> ()
  in
  annotations


let find_all_annotations_batch ~state ~configuration ~paths =
  let get_annotations { path; lookup; _ } =
    let annotations =
      lookup
      >>| Lookup.get_all_annotations
      >>| List.map ~f:(fun (location, annotation) ->
              instantiate_location ~state location, annotation)
    in
    { path; types_by_location = annotations }
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
          definition >>| fun location -> ["resolved location", Location.Reference.show location]
        in
        log_lookup ~handle ~position ~timer ~name:"find definition" ?normals ()
    | _ -> ()
  in
  definition
