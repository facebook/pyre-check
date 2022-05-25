(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type ReadOnly = sig
  type t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
end

module UpdateResult : sig
  module type S = sig
    type t

    type read_only

    val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

    val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t list

    val read_only : t -> read_only

    val unannotated_global_environment_update_result
      :  t ->
      UnannotatedGlobalEnvironment.UpdateResult.t

    val invalidated_modules : t -> AstEnvironment.InvalidatedModules.t
  end
end

module type PreviousEnvironment = sig
  module ReadOnly : ReadOnly

  module UpdateResult : UpdateResult.S with type read_only := ReadOnly.t

  type t

  val create : AstEnvironment.t -> t

  val ast_environment : t -> AstEnvironment.t

  val configuration : t -> Configuration.Analysis.t

  val read_only : t -> ReadOnly.t

  val cold_start : t -> ReadOnly.t

  val update_this_and_all_preceding_environments
    :  t ->
    scheduler:Scheduler.t ->
    AstEnvironment.trigger ->
    UpdateResult.t
end

module type S = sig
  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult : UpdateResult.S with type read_only = ReadOnly.t

  type t

  val create : AstEnvironment.t -> t

  val ast_environment : t -> AstEnvironment.t

  val configuration : t -> Configuration.Analysis.t

  val read_only : t -> ReadOnly.t

  val cold_start : t -> ReadOnly.t

  val update_this_and_all_preceding_environments
    :  t ->
    scheduler:Scheduler.t ->
    AstEnvironment.trigger ->
    UpdateResult.t
end

(* The following is a special form of a shared memory table optimized for incremental type checking.
   This update function executes a parallelized update of the shared memory tables contained in the
   environment, based off of the triggers from the previous environment updates. This then generates
   triggers for further downstream environments. *)

module EnvironmentTable : sig
  module type In = sig
    (* This refers to the immediately preceding environment *)
    module PreviousEnvironment : PreviousEnvironment

    module Key : Memory.KeyType

    module Value : Memory.ComparableValueType

    (* This is the data type of the key that we are being told to compute. This sometimes
       unfortunately has to differ from the actual key of the table, but the difference should be
       one with a one-to-one conversion, done by convert_trigger *)
    type trigger [@@deriving sexp, compare]

    val convert_trigger : trigger -> Key.t

    val key_to_trigger : Key.t -> trigger

    module TriggerSet : Set.S with type Elt.t = trigger

    val lazy_incremental : bool

    (* This function should extract the relevant updates from upstream triggers. Usually this
       selecting the relevant variant from SharedMemoryKeys.dependency *)
    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val trigger_to_dependency : trigger -> SharedMemoryKeys.dependency

    (* For compatibility with the old dependency mode, we also need a different kind of key
       discovery that just returns all of the keys that possibly could have been affected by the
       update *)
    val legacy_invalidated_keys : UnannotatedGlobalEnvironment.UpdateResult.t -> TriggerSet.t

    (* This is the actual main function of the update. *)
    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      dependency:SharedMemoryKeys.DependencyKey.registered option ->
      Value.t

    val serialize_value : Value.t -> string

    val show_key : Key.t -> string

    val equal_value : Value.t -> Value.t -> bool
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
    end

    module UpdateResult : UpdateResult.S with type read_only = ReadOnly.t

    type t

    val create : AstEnvironment.t -> t

    val ast_environment : t -> AstEnvironment.t

    val configuration : t -> Configuration.Analysis.t

    val read_only : t -> ReadOnly.t

    val cold_start : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      AstEnvironment.trigger ->
      UpdateResult.t
  end

  module WithCache (In : In) : S with module In = In

  module NoCache (In : In) : S with module In = In
end
