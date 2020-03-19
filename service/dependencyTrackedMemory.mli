(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Set = Caml.Set
open Memory

(* Deliberately left abstract to prevent calls to DependencyKey.Transaction.add from outside this
   module *)
type 'keyset transaction_element

module DependencyKey : sig
  module type S = sig
    include KeyType

    module KeySet : Set.S with type elt = t

    val encode : t -> int

    val decode : int -> t option

    module Transaction : sig
      type t

      val empty : scheduler:Scheduler.t -> configuration:Configuration.Analysis.t -> t

      (* Cannot be called from outside this module *)
      val add : t -> KeySet.t transaction_element -> t

      val execute : t -> update:(unit -> 'a) -> 'a * KeySet.t

      val scheduler : t -> Scheduler.t

      val configuration : t -> Configuration.Analysis.t
    end
  end

  module Make (Key : KeyType) : S with type t = Key.t
end

module DependencyKind : sig
  type t =
    | Get
    | Mem
end

module DependencyTrackedTableWithCache
    (Key : KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : ComparableValueType) : sig
  include
    WithCache.S
      with type t = Value.t
       and type key = Key.t
       and type key_out = Key.out
       and module KeySet = Set.Make(Key)
       and module KeyMap = MyMap.Make(Key)

  val get : ?dependency:DependencyKey.t -> key -> t option

  val mem : ?dependency:DependencyKey.t -> key -> bool

  val get_dependents : kind:DependencyKind.t -> key -> DependencyKey.KeySet.t

  val get_all_dependents : KeySet.t -> DependencyKey.KeySet.t

  val add_to_transaction
    :  DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t

  val add_pessimistic_transaction
    :  DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t
end

module DependencyTrackedTableNoCache
    (Key : KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : ComparableValueType) : sig
  include
    NoCache.S
      with type t = Value.t
       and type key = Key.t
       and type key_out = Key.out
       and module KeySet = Set.Make(Key)
       and module KeyMap = MyMap.Make(Key)

  val get : ?dependency:DependencyKey.t -> key -> t option

  val mem : ?dependency:DependencyKey.t -> key -> bool

  val get_dependents : kind:DependencyKind.t -> key -> DependencyKey.KeySet.t

  val get_all_dependents : KeySet.t -> DependencyKey.KeySet.t

  val add_to_transaction
    :  DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t

  val add_pessimistic_transaction
    :  DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t
end
