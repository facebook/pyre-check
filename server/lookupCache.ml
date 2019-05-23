(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis

open State

open Pyre


let handle ~configuration file =
  try
    File.handle ~configuration file
    |> Option.some
  with File.NonexistentHandle error ->
    Log.info "%s" error;
    None


let get_by_handle ~state:{ lookups; environment; _ } ~handle =
  let cache_read = String.Table.find lookups (File.Handle.show handle) in
  match cache_read with
  | Some _ ->
      cache_read
  | None ->
      let lookup =
        Ast.SharedMemory.Sources.get handle
        >>| Lookup.create_of_source environment
      in
      lookup
      >>| (fun lookup -> String.Table.set lookups ~key:(File.Handle.show handle) ~data:lookup)
      |> ignore;
      lookup


let evict ~state:{ lookups; _ } ~configuration file =
  handle ~configuration file
  >>| File.Handle.show
  >>| String.Table.remove lookups
  |> ignore


let log_lookup ~handle ~position ~timer ~name ?(integers = []) ?(normals = []) () =
  let normals =
    let base_normals = [
      "handle", File.Handle.show handle;
      "position", Location.show_position position;
    ]
    in
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


let find_annotation ~state ~configuration ~file ~position =
  let find_annotation_by_handle handle =
    let timer = Timer.start () in
    let annotation =
      get_by_handle ~state ~handle
      >>= Lookup.get_annotation ~position
    in
    let normals =
      annotation
      >>| fun (location, annotation) ->
      [
        "resolved location", Location.Instantiated.show location;
        "resolved annotation", Type.show annotation;
      ]
    in
    log_lookup
      ~handle
      ~position
      ~timer
      ~name:"find annotation"
      ?normals
      ();
    annotation
  in
  handle ~configuration file
  >>= find_annotation_by_handle


let find_all_annotations ~state ~configuration ~file =
  let find_annotation_by_handle handle =
    let timer = Timer.start () in
    let annotations =
      get_by_handle ~state ~handle
      >>| Lookup.get_all_annotations
      |> Option.value ~default:[]
    in
    let integers = ["annotation list size", List.length annotations] in
    log_lookup
      ~handle
      ~position:Location.any_position
      ~timer
      ~name:"find all annotations"
      ~integers
      ();
    annotations
  in
  handle ~configuration file
  >>| find_annotation_by_handle


let find_definition ~state ~configuration file position =
  let find_definition_by_handle handle =
    let timer = Timer.start () in
    let definition =
      get_by_handle ~state ~handle
      >>= Lookup.get_definition ~position
    in
    let normals =
      definition
      >>| fun location -> ["resolved location", Location.Instantiated.show location]
    in
    log_lookup
      ~handle
      ~position
      ~timer
      ~name:"find definition"
      ?normals
      ();
    definition
  in
  handle ~configuration file
  >>= find_definition_by_handle
