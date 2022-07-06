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

  val controls : t -> EnvironmentControls.t
end

module UpdateResult : sig
  module type S = sig
    type t

    val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

    val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t list

    val unannotated_global_environment_update_result
      :  t ->
      UnannotatedGlobalEnvironment.UpdateResult.t

    val invalidated_modules : t -> Ast.Reference.t list
  end
end

module PreviousEnvironment : sig
  module type S = sig
    module ReadOnly : ReadOnly

    module UpdateResult : UpdateResult.S

    type t

    val create : EnvironmentControls.t -> t

    val create_for_testing : EnvironmentControls.t -> (Ast.ModulePath.t * string) list -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      ArtifactPath.t list ->
      UpdateResult.t

    val store : t -> unit

    val load : EnvironmentControls.t -> t

    module Overlay : sig
      type t

      val create : ReadOnly.t -> t

      val module_tracker : t -> ModuleTracker.Overlay.t

      val update_overlaid_code
        :  t ->
        code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
        UpdateResult.t

      val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

      val read_only : t -> ReadOnly.t
    end
  end
end

module type S = sig
  include PreviousEnvironment.S

  module PreviousEnvironment : PreviousEnvironment.S

  module Testing : sig
    module ReadOnly : sig
      val upstream : ReadOnly.t -> PreviousEnvironment.ReadOnly.t
    end

    module UpdateResult : sig
      val upstream : UpdateResult.t -> PreviousEnvironment.UpdateResult.t
    end
  end
end

(* The following is a special form of a shared memory table optimized for incremental type checking.
   This update function executes a parallelized update of the shared memory tables contained in the
   environment, based off of the triggers from the previous environment updates. This then generates
   triggers for further downstream environments. *)

module EnvironmentTable : sig
  module type In = sig
    (* This refers to the immediately preceding environment *)
    module PreviousEnvironment : PreviousEnvironment.S

    module Key : Memory.KeyType

    module Value : Memory.ValueTypeWithEquivalence

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

    (* This is the actual main function of the update. *)
    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      dependency:SharedMemoryKeys.DependencyKey.registered option ->
      Value.t

    val show_key : Key.t -> string

    val overlay_owns_key : ModuleTracker.Overlay.t -> Key.t -> bool

    val equal_value : Value.t -> Value.t -> bool
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

      val controls : t -> EnvironmentControls.t
    end

    module UpdateResult : UpdateResult.S

    module Overlay : sig
      type t

      val create : ReadOnly.t -> t

      val module_tracker : t -> ModuleTracker.Overlay.t

      val update_overlaid_code
        :  t ->
        code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
        UpdateResult.t

      val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

      val read_only : t -> ReadOnly.t
    end

    type t

    val create : EnvironmentControls.t -> t

    val create_for_testing : EnvironmentControls.t -> (Ast.ModulePath.t * string) list -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      ArtifactPath.t list ->
      UpdateResult.t

    val store : t -> unit

    val load : EnvironmentControls.t -> t

    module Unsafe : sig
      val upstream : t -> In.PreviousEnvironment.t
    end

    module Testing : sig
      module ReadOnly : sig
        val upstream : ReadOnly.t -> In.PreviousEnvironment.ReadOnly.t
      end

      module UpdateResult : sig
        val upstream : UpdateResult.t -> In.PreviousEnvironment.UpdateResult.t
      end
    end
  end

  module WithCache (In : In) : S with module In = In

  module NoCache (In : In) : S with module In = In
end
