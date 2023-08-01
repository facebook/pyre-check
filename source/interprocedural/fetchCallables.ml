(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* FetchCallables: implements the logic that discovers all functions and methods
 * to analyze, and categorize them.
 *)

open Core
open Pyre
open Ast
open Statement
module GlobalResolution = Analysis.GlobalResolution
module TypeEnvironment = Analysis.TypeEnvironment
module AstEnvironment = Analysis.AstEnvironment

type t = {
  (* All callables:
   * - With an explicit definition (i.e, existing `def <name>():`)
   * - That are not stubs (i.e, NOT `def <name>(): ...`)
   * - That are in files within the source paths (as opposed to being in the
   * search path).
   *)
  internals: Target.t list;
  (* All non-stub callables with a definition. *)
  definitions: Target.t list;
  (* All stub callables. *)
  stubs: Target.t list;
}

let empty = { internals = []; definitions = []; stubs = [] }

let join left right =
  {
    internals = List.rev_append right.internals left.internals;
    definitions = List.rev_append right.definitions left.definitions;
    stubs = List.rev_append right.stubs left.stubs;
  }


let gather_raw_definitions
    ~resolution
    ~source:{ Source.module_path = { ModulePath.qualifier; _ }; _ }
  =
  (* Ignoring parameters that are also function definitions,
   * i.e def f(g): if not g: def g(): ...; g() *)
  let filter_parameters define_name =
    let define_name = Reference.show define_name in
    if String.is_prefix ~prefix:"$parameter$" define_name then
      let () =
        Log.warning
          "In module `%a`, the parameter name `%s` is used as a function name. This function will \
           NOT be analyzed."
          Reference.pp
          qualifier
          (String.sub define_name ~pos:11 ~len:(String.length define_name - 11))
      in
      false
    else
      true
  in
  let fetch_callables define_name =
    let { Target.qualifier = define_qualifier; callables; has_multiple_definitions } =
      Option.value_exn
        ~message:"Missing definitions for define name"
        (Target.get_definitions ~resolution define_name)
    in
    if not (Reference.equal qualifier define_qualifier) then
      let () =
        Log.warning
          "Function `%a` present in multiple qualifiers: `%a` and `%a`. This function will NOT be \
           analyzed"
          Reference.pp
          define_name
          Reference.pp
          qualifier
          Reference.pp
          define_qualifier
      in
      None
    else
      let () =
        if has_multiple_definitions then
          Log.warning
            "Found multiple definitions for the given symbol: `%a`. We will only consider the last \
             definition."
            Reference.pp
            define_name
      in
      Some callables
  in
  let merge_callables callables_left callables_right =
    Target.Map.merge callables_left callables_right ~f:(fun ~key:target value ->
        match value with
        | `Left define
        | `Right define ->
            Some define
        | `Both (define_left, define_right) ->
            Format.asprintf
              "Unexpected callable `%a` with multiple define names: `%a` and `%a`"
              Target.pp_internal
              target
              Reference.pp
              define_left.Node.value.Define.signature.name
              Reference.pp
              define_right.Node.value.Define.signature.name
            |> failwith)
  in
  GlobalResolution.unannotated_global_environment resolution
  |> (fun environment ->
       Analysis.UnannotatedGlobalEnvironment.ReadOnly.get_define_names environment qualifier)
  |> Reference.Set.of_list
  |> Reference.Set.elements
  |> List.filter ~f:filter_parameters
  |> List.filter_map ~f:fetch_callables
  |> List.fold ~init:Target.Map.empty ~f:merge_callables


(** Traverse the AST to find all callables (functions and methods). *)
let from_source ~configuration ~resolution ~include_unit_tests ~source =
  if (not include_unit_tests) && GlobalResolution.source_is_unit_test resolution ~source then
    empty
  else
    let definitions = gather_raw_definitions ~resolution ~source in
    let definitions =
      if Ast.ModulePath.is_stub source.module_path then
        Target.Map.filter definitions ~f:(fun { Node.value = define; _ } ->
            not (Define.is_toplevel define || Define.is_class_toplevel define))
      else
        definitions
    in
    let is_internal =
      Ast.ModulePath.is_internal_path
        ~configuration
        (Ast.ModulePath.full_path ~configuration source.module_path)
    in
    let add_definition ~key:definition ~data:{ Node.value = define; _ } result =
      if Define.is_stub define then
        { result with stubs = definition :: result.stubs }
      else if is_internal then
        {
          result with
          internals = definition :: result.internals;
          definitions = definition :: result.definitions;
        }
      else
        { result with definitions = definition :: result.definitions }
    in
    Target.Map.fold ~init:empty ~f:add_definition definitions


let get_source ~environment qualifier =
  let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
  AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier


let from_qualifiers ~scheduler ~environment ~configuration ~include_unit_tests ~qualifiers =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let map qualifiers =
    let callables_of_qualifier callables qualifier =
      get_source ~environment qualifier
      >>| (fun source ->
            from_source ~configuration ~resolution:global_resolution ~include_unit_tests ~source)
      |> Option.value ~default:empty
      |> join callables
    in
    List.fold qualifiers ~f:callables_of_qualifier ~init:empty
  in
  Scheduler.map_reduce
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_count
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:50
         ~preferred_chunks_per_worker:1
         ())
    ~map
    ~reduce:join
    ~initial:empty
    ~inputs:qualifiers
    ()


let get_internal_definitions { internals; _ } = internals

let get_definitions { definitions; _ } = definitions

let get_stubs { stubs; _ } = stubs

let get { definitions; stubs; _ } ~definitions:include_definitions ~stubs:include_stubs =
  let targets =
    if include_definitions then
      definitions
    else
      []
  in
  let targets =
    if include_stubs then
      List.rev_append stubs targets
    else
      targets
  in
  targets


let get_stats { internals; definitions; stubs } =
  [
    "definitions", List.length definitions;
    "internals", List.length internals;
    "stubs", List.length stubs;
  ]
