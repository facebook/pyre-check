(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

module PyrePysaEnvironment = Analysis.PyrePysaEnvironment

(* Add the files that contain any of the given callables. *)
val from_callables
  :  scheduler:Scheduler.t ->
  scheduler_policies:Configuration.SchedulerPolicies.t ->
  callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
  resolve_module_path:(Ast.Reference.t -> Interprocedural.RepositoryPath.t option) ->
  callables:Interprocedural.Target.t list ->
  t

val empty : t

val is_empty : t -> bool

val write_to_file : path:PyrePath.t -> t -> unit
