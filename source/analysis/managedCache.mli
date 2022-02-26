(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type SexpableKeyType = sig
  type t [@@deriving sexp, compare]

  val to_string : t -> string

  type out

  val from_string : string -> out
end

module type In = sig
  module PreviousEnvironment : Environment.PreviousEnvironment

  module Key : SexpableKeyType

  module Value : Memory.ComparableValueType

  module KeySet : Set.S with type Elt.t = Key.t

  module HashableKey : Hashable with type t := Key.t

  val lazy_incremental : bool

  val produce_value
    :  PreviousEnvironment.ReadOnly.t ->
    Key.t ->
    dependency:SharedMemoryKeys.DependencyKey.registered option ->
    Value.t

  val filter_upstream_dependency : SharedMemoryKeys.dependency -> Key.t option

  val trigger_to_dependency : Key.t -> SharedMemoryKeys.dependency
end

module Make (In : In) :
  Environment.EnvironmentTable.S
    with type In.Key.t = In.Key.t
     and module In.PreviousEnvironment = In.PreviousEnvironment
     and module In.Value = In.Value
