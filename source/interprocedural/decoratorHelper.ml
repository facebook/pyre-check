(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Core
open Pyre
open Statement

let all_decorators environment =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let unannotated_global_environment =
    GlobalResolution.unannotated_global_environment global_resolution
  in
  let decorator_set = Reference.Hash_set.create () in
  let add_decorators define_reference =
    let add_decorator_to_set decorator =
      match Decorator.from_expression decorator with
      | None -> ()
      | Some { Decorator.name = { Node.value = decorator; _ }; _ } ->
          Base.Hash_set.add decorator_set decorator
    in
    UnannotatedGlobalEnvironment.ReadOnly.get_define_body
      unannotated_global_environment
      define_reference
    |> Option.iter ~f:(fun { Node.value = { Define.signature = { decorators; _ }; _ }; _ } ->
           List.iter ~f:add_decorator_to_set decorators)
  in
  let _ =
    UnannotatedGlobalEnvironment.ReadOnly.all_defines unannotated_global_environment
    |> List.iter ~f:add_decorators
  in
  Base.Hash_set.to_list decorator_set


let all_decorator_bodies environment =
  all_decorators environment
  |> List.filter_map ~f:(fun decorator ->
         GlobalResolution.define (TypeEnvironment.ReadOnly.global_resolution environment) decorator
         >>| fun decorator_define -> decorator, decorator_define)
  |> Reference.Map.of_alist
  |> function
  | `Ok map -> map
  | _ -> Reference.Map.empty


let inline_decorators ~decorator_bodies source =
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children state _ = state, true

    let expression _ expression = expression

    let statement _ statement =
      let statement =
        match statement with
        | { Node.value = Statement.Define define; location } ->
            {
              statement with
              value =
                Statement.Define
                  (InlineDecorator.inline_decorators_for_define
                     ~get_decorator_body:(Map.find decorator_bodies)
                     ~location
                     define);
            }
        | _ -> statement
      in
      (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let type_environment_with_decorators_inlined
    ~configuration
    ~scheduler
    ~recheck
    ~decorators_to_skip:_
    environment
  =
  let open Analysis in
  let open Ast in
  let decorator_bodies = all_decorator_bodies (TypeEnvironment.read_only environment) in
  let decorator_bodies =
    Map.filter_keys decorator_bodies ~f:(fun decorator ->
        not (InlineDecorator.DecoratorsToSkip.mem decorator))
  in
  let environment =
    AstEnvironment.create
      ~additional_preprocessing:(inline_decorators ~decorator_bodies)
      (AstEnvironment.module_tracker (TypeEnvironment.ast_environment environment))
    |> AnnotatedGlobalEnvironment.create
    |> TypeEnvironment.create
  in
  let all_internal_paths =
    let get_internal_path source_path =
      let path = SourcePath.full_path ~configuration source_path in
      Option.some_if (SourcePath.is_internal_path ~configuration path) path
    in
    ModuleTracker.source_paths
      (AstEnvironment.module_tracker (TypeEnvironment.ast_environment environment))
    |> List.filter_map ~f:get_internal_path
  in
  let _ =
    recheck
      ~configuration
      ~scheduler
      ~environment
      ~errors:(Ast.Reference.Table.create ())
      all_internal_paths
  in
  environment
