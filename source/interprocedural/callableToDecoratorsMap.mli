(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

module DecoratedDefineBody : sig
  type t = {
    decorated_callable: Target.t;
    module_qualifier: Reference.t;
    define_name: Reference.t;
    return_expression: Expression.t;
    original_function_name: Name.t;
    original_function_name_location: Location.t;
  }
end

(* A map from each callable to its decorators. *)
module SharedMemory : sig
  type t

  module ReadOnly : sig
    type t

    val get_decorators : t -> Target.t -> Expression.t list option
  end

  val read_only : t -> ReadOnly.t

  val create_empty : is_pyrefly:bool -> unit -> t

  (* We assume `DecoratorPreprocessing.setup_preprocessing` is called before since we use its shared
     memory here. *)
  val create
    :  scheduler:Scheduler.t ->
    scheduler_policy:Scheduler.Policy.t ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
    skip_analysis_targets:Target.HashSet.t ->
    Target.t list ->
    t

  val cleanup : t -> unit

  val save_decorator_counts_to_directory
    :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
    scheduler:Scheduler.t ->
    t ->
    unit

  val redirect_to_decorated : ReadOnly.t -> Target.t -> Target.t

  val redirect_to_decorated_opt : ReadOnly.t -> Target.t -> Target.t option

  val decorated_callable_body : ReadOnly.t -> Target.t -> DecoratedDefineBody.t option

  val register_decorator_defines
    :  t ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    CallablesSharedMemory.ReadWrite.t ->
    CallablesSharedMemory.ReadWrite.t

  val decorated_targets : t -> Target.t list
end
