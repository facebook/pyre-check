(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Core
open Statement

let all_decorators environment =
  let unannotated_global_environment =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
  in
  let decorator_set = Reference.Hash_set.create () in
  let add_decorators define_reference =
    let add_decorator_to_set { Decorator.name = { Node.value; _ }; _ } =
      Base.Hash_set.add decorator_set value
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
