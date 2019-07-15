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
  types_by_location: (Location.Instantiated.t * Type.t) list option
}

type lookup = {
  path: PyrePath.t;
  qualifier: Reference.t;
  handle: string option;
  lookup: Lookup.t option
}

let get_lookups
    ~configuration:({ Configuration.Analysis.store_type_check_resolution; _ } as configuration)
    ~state:{ lookups; environment; scheduler; _ }
    paths
  =
  let paths, nonexistent_paths =
    let get_source_path path =
      match SourcePath.create ~configuration path with
      | Some { SourcePath.qualifier; relative_path; _ } ->
          `Fst
            { path;
              qualifier;
              handle = Some (Path.RelativePath.relative relative_path);
              lookup = None
            }
      | None -> `Snd { path; qualifier = Reference.empty; handle = None; lookup = None }
    in
    List.partition_map ~f:get_source_path paths
  in
  let cache_hits, cache_misses =
    let retrieve_cached ({ qualifier; _ } as path) =
      let cache_read = String.Table.find lookups (Reference.show qualifier) in
      match cache_read with
      | Some _ -> `Fst { path with lookup = cache_read }
      | None -> `Snd path
    in
    List.partition_map ~f:retrieve_cached paths
  in
  let generate_lookups paths =
    let generate_lookup ({ qualifier; _ } as incomplete_lookup) =
      let lookup =
        Ast.SharedMemory.Sources.get qualifier >>| Lookup.create_of_source environment
      in
      lookup
      >>| (fun lookup -> String.Table.set lookups ~key:(Reference.show qualifier) ~data:lookup)
      |> ignore;
      { incomplete_lookup with lookup }
    in
    if not store_type_check_resolution then (
      Service.Check.analyze_sources
        ~scheduler:(Scheduler.with_parallel scheduler ~is_parallel:(List.length paths > 5))
        ~configuration:{ configuration with store_type_check_resolution = true }
        ~environment
        ~handles:
          (List.map ~f:(fun { path; _ } -> path |> File.create |> File.handle ~configuration) paths)
        ()
      |> ignore;
      let lookups = List.map ~f:generate_lookup paths in
      ResolutionSharedMemory.remove (List.map ~f:(fun { qualifier; _ } -> qualifier) paths);
      lookups )
    else
      List.map ~f:generate_lookup paths
  in
  nonexistent_paths @ cache_hits @ generate_lookups cache_misses


let evict ~state:{ lookups; _ } reference = String.Table.remove lookups (Reference.show reference)

let evict_path ~state ~configuration path =
  match SourcePath.create ~configuration path with
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
  let { lookup; handle; _ } = get_lookups ~configuration ~state [path] |> List.hd_exn in
  let annotation = lookup >>= Lookup.get_annotation ~position in
  let _ =
    match handle with
    | Some handle ->
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
  let { lookup; handle; _ } = get_lookups ~configuration ~state [path] |> List.hd_exn in
  let annotations = lookup >>| Lookup.get_all_annotations in
  let _ =
    match handle, annotations with
    | Some handle, Some annotations ->
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
    let annotations = lookup >>| Lookup.get_all_annotations in
    { path; types_by_location = annotations }
  in
  List.map ~f:get_annotations (get_lookups ~configuration ~state paths)


let find_definition ~state ~configuration path position =
  let timer = Timer.start () in
  let { lookup; handle; _ } = get_lookups ~configuration ~state [path] |> List.hd_exn in
  let definition = lookup >>= Lookup.get_definition ~position in
  let _ =
    match handle with
    | Some handle ->
        let normals =
          definition >>| fun location -> ["resolved location", Location.Instantiated.show location]
        in
        log_lookup ~handle ~position ~timer ~name:"find definition" ?normals ()
    | _ -> ()
  in
  definition
