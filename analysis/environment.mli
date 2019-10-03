(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module type ReadOnly = sig
  type t
end

module type PreviousUpdateResult = sig
  type t

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t list
end

module type PreviousEnvironment = sig
  type t

  module ReadOnly : ReadOnly

  module UpdateResult : PreviousUpdateResult
end

module UpdateResult : sig
  module type S = sig
    include PreviousUpdateResult

    type upstream

    val upstream : t -> upstream
  end

  module type Private = sig
    type t

    type upstream

    val create
      :  triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.t ->
      upstream:upstream ->
      t
  end

  module Make (PreviousEnvironment : PreviousEnvironment) : sig
    include S with type upstream = PreviousEnvironment.UpdateResult.t

    include Private with type upstream := upstream and type t := t
  end
end

module type S = sig
  type t

  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult : UpdateResult.S with type upstream = PreviousEnvironment.UpdateResult.t

  val create : PreviousEnvironment.ReadOnly.t -> t

  val update
    :  t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    PreviousEnvironment.UpdateResult.t ->
    UpdateResult.t

  val read_only : t -> ReadOnly.t
end

(* Updaters: The following functors are aids to producing the update function expected by the above
   module signature. This update function executes a parallelized update of the shared memory
   tables contained in the environment, based off of the triggers from the previous environment
   updates. This then generates triggers for further downstream environments. *)

module Updater : sig
  module type In = sig
    (* This is the table that the updater is actually updating *)
    module Table : sig
      include Memory.NoCache.S

      val add_to_transaction
        :  SharedMemoryKeys.DependencyKey.Transaction.t ->
        keys:KeySet.t ->
        SharedMemoryKeys.DependencyKey.Transaction.t

      val get : ?dependency:SharedMemoryKeys.DependencyKey.t -> key -> t option
    end

    (* This refers to the immediately preceding environment *)
    module PreviousEnvironment : PreviousEnvironment

    (* This should have been most likely produced by UpdateResult.Make(PreviousEnvironment) *)
    module UpdateResult : sig
      include UpdateResult.S with type upstream = PreviousEnvironment.UpdateResult.t

      include UpdateResult.Private with type upstream := upstream and type t := t
    end

    (* This is the environment's internal data type, only used for the actual update function *)
    type t

    (* This is the data type of the key that we are being told to compute. This sometimes
       unfortunately has to differ from the actual key of the table, but the difference should be
       one with a one-to-one conversion, done by convert_trigger *)
    type trigger

    val convert_trigger : trigger -> Table.key

    module TriggerSet : Set.S with type Elt.t = trigger

    (* This function should extract the relevant updates from upstream triggers. Usually this
       selecting the relevant variant from SharedMemoryKeys.dependency *)
    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    (* Environment updates both have to update invalidated entries and calculate values for added
       ones. This function should return those new keys *)
    val added_keys : UpdateResult.upstream -> TriggerSet.t

    (* For compatibility with the old dependency mode, we also need a different kind of key
       discovery that just returns all of the keys under current consideration. For now this
       usually is just going back to the UnannotatedGlobalEnvironment.UpdateResult.t and returning
       either current_and_previous_classes or current_and_previous_unannotated_globals *)
    val current_and_previous_keys : UpdateResult.upstream -> TriggerSet.t

    (* This is the actual main function of the update. This should write the correct data into the
       table for the given keys *)
    val register : t -> trigger list -> track_dependencies:bool -> unit
  end

  module Make (In : In) : sig
    val update
      :  In.t ->
      scheduler:Scheduler.t ->
      configuration:Configuration.Analysis.t ->
      In.PreviousEnvironment.UpdateResult.t ->
      In.UpdateResult.t
  end
end
