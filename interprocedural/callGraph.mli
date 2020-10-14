(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis

type callees =
  | ConstructorTargets of {
      new_targets: Callable.t list;
      init_targets: Callable.t list;
    }
  | RegularTargets of {
      implicit_self: bool;
      targets: Callable.t list;
    }
[@@deriving eq, show]

val call_graph_of_define
  :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callees Ast.Location.Map.t

val defining_attribute
  :  resolution:Resolution.t ->
  Type.t ->
  string ->
  Annotated.Attribute.instantiated option

val resolve_ignoring_optional : resolution:Resolution.t -> Ast.Expression.t -> Type.t
