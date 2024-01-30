(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type t

val module_tracker : t -> ModuleTracker.t

val controls : t -> EnvironmentControls.t

val create : EnvironmentControls.t -> t

val as_source_code_incremental : t -> SourceCodeIncrementalApi.Base.t

(* Load and store the environment to and from saved-state *)

val load : EnvironmentControls.t -> t

val store : t -> unit

val clear_memory_for_tests : scheduler:Scheduler.t -> t -> unit

val remove_sources : t -> Reference.t list -> unit
