(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module type ReadOnly = sig
  type t

  val hash_to_key_map : t -> string String.Map.t

  val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

  val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option
end

module type PreviousUpdateResult = sig
  type t

  type read_only

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t list

  val read_only : t -> read_only
end

module type PreviousEnvironment = sig
  module ReadOnly : ReadOnly

  module UpdateResult : PreviousUpdateResult with type read_only := ReadOnly.t
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

    type read_only

    val create
      :  triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.t ->
      upstream:upstream ->
      read_only:read_only ->
      t
  end

  module Make (PreviousEnvironment : PreviousEnvironment) (ReadOnly : ReadOnly) : sig
    include
      S with type upstream = PreviousEnvironment.UpdateResult.t and type read_only = ReadOnly.t

    include Private with type upstream := upstream and type t := t and type read_only := read_only
  end
end

module type S = sig
  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult :
    UpdateResult.S
      with type upstream = PreviousEnvironment.UpdateResult.t
       and type read_only = ReadOnly.t

  val update
    :  scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    PreviousEnvironment.UpdateResult.t ->
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
    type trigger

    val convert_trigger : trigger -> Key.t

    val key_to_trigger : Key.t -> trigger

    module TriggerSet : Set.S with type Elt.t = trigger

    (* This function should extract the relevant updates from upstream triggers. Usually this
       selecting the relevant variant from SharedMemoryKeys.dependency *)
    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    (* For compatibility with the old dependency mode, we also need a different kind of key
       discovery that just returns all of the keys that possibly could have been affected by the
       update *)
    val legacy_invalidated_keys : PreviousEnvironment.UpdateResult.t -> TriggerSet.t

    (* This is the actual main function of the update. *)
    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      track_dependencies:bool ->
      Value.t

    val all_keys : PreviousEnvironment.ReadOnly.t -> Key.t list

    val serialize_value : Value.t -> string

    val show_key : Key.out -> string

    val equal_value : Value.t -> Value.t -> bool
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.dependency -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val hash_to_key_map : t -> string String.Map.t

      val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

      val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option
    end

    module UpdateResult : sig
      include
        UpdateResult.S
          with type upstream = In.PreviousEnvironment.UpdateResult.t
           and type read_only = ReadOnly.t

      include
        UpdateResult.Private
          with type upstream := upstream
           and type t := t
           and type read_only := read_only
    end

    val update
      :  scheduler:Scheduler.t ->
      configuration:Configuration.Analysis.t ->
      In.PreviousEnvironment.UpdateResult.t ->
      UpdateResult.t
  end

  module WithCache (In : In) : S with module In = In

  module NoCache (In : In) : S with module In = In
end
