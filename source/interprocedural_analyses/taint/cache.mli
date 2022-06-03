(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Interprocedural

type t

val load : scheduler:Scheduler.t -> configuration:Configuration.Analysis.t -> enabled:bool -> t

val type_environment : t -> (unit -> TypeEnvironment.t) -> TypeEnvironment.t

val class_hierarchy_graph : t -> (unit -> ClassHierarchyGraph.t) -> ClassHierarchyGraph.t

val initial_callables : t -> (unit -> FetchCallables.t) -> FetchCallables.t

val override_graph
  :  t ->
  (unit -> OverrideGraph.whole_program_overrides) ->
  OverrideGraph.whole_program_overrides
