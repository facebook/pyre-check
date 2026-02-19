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
    ~pyre1_api
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
    let { Target.qualifier = define_qualifier; callables } =
      Option.value_exn
        ~message:"Missing definitions for define name"
        (Target.get_definitions ~pyre1_api ~warn_multiple_definitions:true define_name)
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
      Some callables
  in
  let merge_callables callables_left callables_right =
    Target.Map.union
      (fun target define_left define_right ->
        (* TODO(T199841372): Pysa code should not assume that the raw AST has fully qualified
           names. *)
        Format.asprintf
          "Unexpected callable `%a` with multiple define names: `%a` and `%a`"
          Target.pp_internal
          target
          Reference.pp
          define_left.Node.value.Define.signature.name
          Reference.pp
          define_right.Node.value.Define.signature.name
        |> failwith)
      callables_left
      callables_right
  in
  Analysis.PyrePysaEnvironment.ReadOnly.get_define_names_for_qualifier pyre1_api qualifier
  |> Reference.Set.of_list
  |> Set.elements
  |> List.filter ~f:filter_parameters
  |> List.filter_map ~f:fetch_callables
  |> List.fold ~init:Target.Map.empty ~f:merge_callables


(** Traverse the AST to find all callables (functions and methods). *)
let from_source_with_pyre1 ~configuration ~pyre1_api ~source =
  if Analysis.PyrePysaEnvironment.ReadOnly.source_is_unit_test pyre1_api ~source then
    empty
  else
    let definitions = gather_raw_definitions ~pyre1_api ~source in
    let definitions =
      if Ast.ModulePath.is_stub source.module_path then
        Target.Map.filter
          (fun _ { Node.value = define; _ } ->
            not (Define.is_toplevel define || Define.is_class_toplevel define))
          definitions
      else
        definitions
    in
    let is_internal =
      Analysis.ArtifactPaths.(
        is_internal_path
          ~configuration
          (artifact_path_of_module_path ~configuration source.module_path))
    in
    let add_definition definition { Node.value = define; _ } result =
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
    Target.Map.fold add_definition definitions empty


let from_qualifier_with_pyrefly ~pyrefly_api ~qualifier =
  let define_names =
    PyreflyApi.ReadOnly.get_define_names_for_qualifier
      pyrefly_api
      ~exclude_test_modules:true
      qualifier
  in
  let is_stub_module = PyreflyApi.ReadOnly.is_stub_qualifier pyrefly_api qualifier in
  let add_target result define_name =
    let target =
      PyreflyApi.ReadOnly.target_from_define_name pyrefly_api ~override:false define_name
    in
    let { PyreflyApi.CallableMetadata.is_toplevel; is_class_toplevel; _ } =
      PyreflyApi.ReadOnly.get_callable_metadata pyrefly_api define_name
    in
    let is_stub_like = PyreflyApi.ReadOnly.is_stub_like_callable pyrefly_api define_name in
    if is_stub_module && (is_toplevel || is_class_toplevel) then
      (* Ignore top level define for stub modules (i.e, `.pyi`) *)
      result
    else if is_stub_like then
      { result with stubs = target :: result.stubs }
    else
      (* TODO(T225700656): For now, all modules are considered internal. We could potentially use
         the "roots" of pyrefly to determine if a callable is internal or not. *)
      {
        result with
        internals = target :: result.internals;
        definitions = target :: result.definitions;
      }
  in
  List.fold define_names ~init:empty ~f:add_target


let from_qualifier ~configuration ~pyre_api ~qualifier =
  match pyre_api with
  | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api -> from_qualifier_with_pyrefly ~pyrefly_api ~qualifier
  | PyrePysaApi.ReadOnly.Pyre1 pyre1_api ->
      Analysis.PyrePysaEnvironment.ReadOnly.source_of_qualifier pyre1_api qualifier
      >>| (fun source -> from_source_with_pyre1 ~configuration ~pyre1_api ~source)
      |> Option.value ~default:empty


let from_qualifiers ~scheduler ~scheduler_policy ~pyre_api ~configuration ~qualifiers =
  let map qualifiers =
    let callables_of_qualifier callables qualifier =
      from_qualifier ~configuration ~pyre_api ~qualifier |> join callables
    in
    List.fold qualifiers ~f:callables_of_qualifier ~init:empty
  in
  Scheduler.map_reduce
    scheduler
    ~policy:scheduler_policy
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
