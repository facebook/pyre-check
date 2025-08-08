(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type t

(** Traverse the AST to find all callables (functions and methods). *)
val from_qualifier
  :  configuration:Configuration.Analysis.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  qualifier:Reference.t ->
  t

val from_qualifiers
  :  scheduler:Scheduler.t ->
  scheduler_policy:Scheduler.Policy.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  configuration:Configuration.Analysis.t ->
  qualifiers:Reference.t list ->
  t

(* Return all callables:
 * - With an explicit definition (i.e, existing `def <name>():`)
 * - That are not stubs (i.e, NOT `def <name>(): ...`)
 * - That are in files within the source paths (as opposed to being in the
 * search path).
 *)
val get_internal_definitions : t -> Target.t list

val get_definitions : t -> Target.t list

val get_stubs : t -> Target.t list

val get : t -> definitions:bool -> stubs:bool -> Target.t list

val get_stats : t -> (string * int) list
