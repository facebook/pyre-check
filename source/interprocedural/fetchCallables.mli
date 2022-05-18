(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

type found_callable = {
  callable: Target.t;
  define: Define.t Node.t;
  is_internal: bool;
}

(* Exposed for testing purposes. *)
val regular_and_filtered_callables
  :  configuration:Configuration.Analysis.t ->
  resolution:Analysis.GlobalResolution.t ->
  source:Source.t ->
  found_callable list * Target.t list

(* The boolean indicated whether the callable is internal or not. *)
type callable_with_dependency_information = Target.t * bool

type initial_callables = {
  callables_with_dependency_information: callable_with_dependency_information list;
  stubs: Target.t list;
  filtered_callables: Target.Set.t;
}

val fetch_initial_callables
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  initial_callables
