(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Statement

val all_decorators : TypeEnvironment.ReadOnly.t -> Reference.t list

val all_decorator_bodies : TypeEnvironment.ReadOnly.t -> Define.t Reference.Map.t

val inline_decorators : decorator_bodies:Define.t Reference.Map.t -> Source.t -> Source.t

val type_environment_with_decorators_inlined
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  recheck:
    (configuration:Configuration.Analysis.t ->
    scheduler:Scheduler.t ->
    environment:Analysis.TypeEnvironment.t ->
    errors:Analysis.AnalysisError.t list Ast.Reference.Table.t ->
    PyrePath.t list ->
    Ast.Reference.t list * Analysis.AnalysisError.t list) ->
  decorators_to_skip:Reference.Set.t ->
  TypeEnvironment.t ->
  TypeEnvironment.t
