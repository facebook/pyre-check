(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ErrorsEnvironmentReadOnly : sig
  include Environment.ReadOnly

  val type_environment : t -> TypeEnvironment.ReadOnly.t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val module_tracker : t -> ModuleTracker.ReadOnly.t

  val get_errors_for_qualifier : t -> Ast.Reference.t -> AnalysisError.t list

  (* Get all errors for in-project modules; use this to grab errors that are already computed *)
  val get_all_errors : t -> AnalysisError.t list
end

include
  Environment.S
    with module ReadOnly = ErrorsEnvironmentReadOnly
     and module PreviousEnvironment = TypeEnvironment

val type_environment : t -> TypeEnvironment.t

val module_tracker : t -> ModuleTracker.t

val populate_all_errors : scheduler:Scheduler.t -> t -> unit

module UpdateStatistics : sig
  type t = {
    module_updates_count: int;
    invalidated_modules_count: int;
    rechecked_modules_count: int;
    rechecked_functions_count: int;
  }

  val count_updates : UpdateResult.t -> t
end
