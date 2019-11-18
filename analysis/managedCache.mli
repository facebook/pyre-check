(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module type In = sig
  module PreviousEnvironment : Environment.PreviousEnvironment

  module Key : Memory.KeyType

  module Value : Memory.ComparableValueType

  module KeySet : Set.S with type Elt.t = Key.t

  module HashableKey : Hashable with type t := Key.t

  val produce_value : PreviousEnvironment.ReadOnly.t -> Key.t -> track_dependencies:bool -> Value.t

  val filter_upstream_dependency : SharedMemoryKeys.dependency -> Key.t option
end

module Make (In : In) :
  Environment.EnvironmentTable.S
    with module In.Key = In.Key
     and module In.PreviousEnvironment = In.PreviousEnvironment
     and module In.Value = In.Value
