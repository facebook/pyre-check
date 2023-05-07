(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type t

(** Traverse the AST to find all callables (functions and methods). *)
val from_source
  :  configuration:Configuration.Analysis.t ->
  resolution:Analysis.GlobalResolution.t ->
  include_unit_tests:bool ->
  source:Source.t ->
  t

val from_qualifiers
  :  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  configuration:Configuration.Analysis.t ->
  include_unit_tests:bool ->
  qualifiers:Reference.t list ->
  t

(* Return non-stub callables that are in files within the source paths (as opposed to being in the
   search path). *)
val get_internal_callables : t -> Target.t list

val get_non_stub_callables : t -> Target.t list

val get_stubs : t -> Target.t list

val get_callables_and_stubs : t -> Target.t list

val get_stats : t -> (string * int) list
