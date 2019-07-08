(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open State
open Pyre

let get_by_qualifier ~state:{ lookups; environment; _ } qualifier =
  let key = Reference.show qualifier in
  let cache_read = String.Table.find lookups key in
  match cache_read with
  | Some _ -> cache_read
  | None ->
      let lookup =
        Ast.SharedMemory.Sources.get qualifier >>| Lookup.create_of_source environment
      in
      lookup >>| (fun lookup -> String.Table.set lookups ~key ~data:lookup) |> ignore;
      lookup


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
  SourcePath.create ~configuration path
  >>= fun { SourcePath.qualifier; relative_path; _ } ->
  let timer = Timer.start () in
  let annotation = get_by_qualifier ~state qualifier >>= Lookup.get_annotation ~position in
  let normals =
    annotation
    >>| fun (location, annotation) ->
    [ "resolved location", Location.Instantiated.show location;
      "resolved annotation", Type.show annotation ]
  in
  let handle = Path.RelativePath.relative relative_path in
  log_lookup ~handle ~position ~timer ~name:"find annotation" ?normals ();
  annotation


let find_all_annotations ~state ~configuration ~path =
  SourcePath.create ~configuration path
  >>= fun { SourcePath.qualifier; relative_path; _ } ->
  let timer = Timer.start () in
  let annotations =
    get_by_qualifier ~state qualifier >>| Lookup.get_all_annotations |> Option.value ~default:[]
  in
  let integers = ["annotation list size", List.length annotations] in
  let handle = Path.RelativePath.relative relative_path in
  log_lookup
    ~handle
    ~position:Location.any_position
    ~timer
    ~name:"find all annotations"
    ~integers
    ();
  Some annotations


let find_definition ~state ~configuration path position =
  SourcePath.create ~configuration path
  >>= fun { SourcePath.qualifier; relative_path; _ } ->
  let timer = Timer.start () in
  let definition = get_by_qualifier ~state qualifier >>= Lookup.get_definition ~position in
  let normals =
    definition >>| fun location -> ["resolved location", Location.Instantiated.show location]
  in
  let handle = Path.RelativePath.relative relative_path in
  log_lookup ~handle ~position ~timer ~name:"find definition" ?normals ();
  definition
