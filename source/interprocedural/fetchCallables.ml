(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement
module GlobalResolution = Analysis.GlobalResolution
module TypeEnvironment = Analysis.TypeEnvironment
module AstEnvironment = Analysis.AstEnvironment

(* The boolean indicated whether the callable is internal or not. *)
type callable_with_dependency_information = Target.t * bool

type initial_callables = {
  callables_with_dependency_information: callable_with_dependency_information list;
  stubs: Target.t list;
  filtered_callables: Target.Set.t;
}

let unfiltered_callables ~resolution ~source:{ Source.source_path = { SourcePath.qualifier; _ }; _ }
  =
  let defines =
    GlobalResolution.unannotated_global_environment resolution
    |> (fun environment ->
         Analysis.UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module environment qualifier)
    |> List.filter_map ~f:(GlobalResolution.function_definitions resolution)
    |> List.concat
    |> List.filter ~f:(fun { Node.value = define; _ } -> not (Define.is_overloaded_function define))
  in
  List.map ~f:(fun define -> Target.create define, define) defines


type found_callable = {
  callable: Target.t;
  define: Define.t Node.t;
  is_internal: bool;
}

let regular_and_filtered_callables ~configuration ~resolution ~source =
  let callables = unfiltered_callables ~resolution ~source in
  let included, filtered =
    if GlobalResolution.source_is_unit_test resolution ~source then
      [], List.map callables ~f:fst
    else if Ast.SourcePath.is_stub source.source_path then
      ( List.filter callables ~f:(fun (_, { Node.value = define; _ }) ->
            not (Define.is_toplevel define || Define.is_class_toplevel define)),
        [] )
    else
      callables, []
  in
  let is_internal_source =
    Ast.SourcePath.is_internal_path
      ~configuration
      (Ast.SourcePath.full_path ~configuration source.source_path)
  in
  ( List.map included ~f:(fun (callable, define) ->
        { callable; define; is_internal = is_internal_source }),
    filtered )


let get_source ~environment qualifier =
  let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
  AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier


let fetch_callables_to_analyze ~scheduler ~environment ~configuration ~qualifiers =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let classify_source
      (callables, stubs)
      { callable; define = { Node.value = define; _ }; is_internal }
    =
    if Define.is_stub define then
      callables, callable :: stubs
    else
      (callable, is_internal) :: callables, stubs
  in
  let map result qualifiers =
    let make_callables
        ({
           callables_with_dependency_information = existing_callables;
           stubs = existing_stubs;
           filtered_callables = existing_filtered_callables;
         } as result)
        qualifier
      =
      get_source ~environment qualifier
      >>| (fun source ->
            let callables, new_filtered_callables =
              regular_and_filtered_callables ~configuration ~resolution:global_resolution ~source
            in
            let callables, stubs =
              List.fold callables ~f:classify_source ~init:(existing_callables, existing_stubs)
            in
            let filtered_callables =
              List.fold
                new_filtered_callables
                ~init:existing_filtered_callables
                ~f:(Fn.flip Target.Set.add)
            in
            { callables_with_dependency_information = callables; stubs; filtered_callables })
      |> Option.value ~default:result
    in
    List.fold qualifiers ~f:make_callables ~init:result
  in
  let reduce
      {
        callables_with_dependency_information = new_callables;
        stubs = new_stubs;
        filtered_callables = new_filtered_callables;
      }
      { callables_with_dependency_information = callables; stubs; filtered_callables }
    =
    {
      callables_with_dependency_information = List.rev_append new_callables callables;
      stubs = List.rev_append new_stubs stubs;
      filtered_callables = Target.Set.union new_filtered_callables filtered_callables;
    }
  in
  Scheduler.map_reduce
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_count ~minimum_chunk_size:50 ~preferred_chunks_per_worker:1 ())
    ~map
    ~reduce
    ~initial:
      {
        callables_with_dependency_information = [];
        stubs = [];
        filtered_callables = Target.Set.empty;
      }
    ~inputs:qualifiers
    ()


(* Traverse the AST to find all callables (functions and methods), filtering out callables from test
   files. *)
let fetch_initial_callables ~scheduler ~configuration ~environment ~qualifiers =
  let timer = Timer.start () in
  let initial_callables =
    fetch_callables_to_analyze ~scheduler ~environment ~configuration ~qualifiers
  in
  Statistics.performance
    ~name:"Fetched initial callables to analyze"
    ~phase_name:"Fetching initial callables to analyze"
    ~timer
    ();
  initial_callables
