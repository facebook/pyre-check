(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Set = Caml.Set

(* Deliberately left abstract to prevent calls to DependencyKey.Transaction.add from outside this
   module *)
type 'keyset transaction_element

module EncodedDependency : sig
  type t [@@deriving compare, sexp]

  val to_string : t -> string

  val of_string : string -> t

  val make : 'a -> hash:('a -> int) -> t

  val increment : t -> t

  module Table : Hashtbl.S with type key = t
end

module DependencyKey : sig
  module type S = sig
    type key

    type registered

    module RegisteredSet : Set.S with type elt = registered

    module KeySet : Set.S with type elt = key

    val mark : registered -> depends_on:EncodedDependency.t -> unit

    val query : EncodedDependency.t -> RegisteredSet.t

    module Transaction : sig
      type t

      val empty : scheduler:Scheduler.t -> t

      val add : t -> RegisteredSet.t transaction_element -> t

      val execute : t -> update:(unit -> 'a) -> 'a * RegisteredSet.t

      val scheduler : t -> Scheduler.t
    end
  end

  module type In = sig
    type key [@@deriving compare, sexp]

    type registered [@@deriving compare, sexp]

    module RegisteredSet : Set.S with type elt = registered

    module KeySet : Set.S with type elt = key

    module Registry : sig
      val encode : registered -> EncodedDependency.t

      val decode : EncodedDependency.t -> registered list option
    end
  end

  module Make (In : In) :
    S
      with type key = In.key
       and type registered = In.registered
       and module RegisteredSet = In.RegisteredSet
       and module KeySet = In.KeySet
end

module DependencyKind : sig
  type t =
    | Get
    | Mem
end

module DependencyTrackedTableWithCache
    (Key : Memory.KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : Memory.ValueTypeWithEquivalence) : sig
  include
    Memory.FirstClass.WithCache.S
      with type value = Value.t
       and type key = Key.t
       and module KeySet = Set.Make(Key)
       and module KeyMap = MyMap.Make(Key)

  val get : t -> ?dependency:DependencyKey.registered -> key -> value option

  val mem : t -> ?dependency:DependencyKey.registered -> key -> bool

  val get_dependents : kind:DependencyKind.t -> key -> DependencyKey.RegisteredSet.t

  val get_all_dependents : KeySet.t -> DependencyKey.RegisteredSet.t

  val add_to_transaction
    :  t ->
    DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t

  val add_pessimistic_transaction
    :  t ->
    DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t
end

module DependencyTrackedTableNoCache
    (Key : Memory.KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : Memory.ValueTypeWithEquivalence) : sig
  include
    Memory.FirstClass.NoCache.S
      with type value = Value.t
       and type key = Key.t
       and module KeySet = Set.Make(Key)
       and module KeyMap = MyMap.Make(Key)

  val get : t -> ?dependency:DependencyKey.registered -> key -> value option

  val mem : t -> ?dependency:DependencyKey.registered -> key -> bool

  val get_dependents : kind:DependencyKind.t -> key -> DependencyKey.RegisteredSet.t

  val get_all_dependents : KeySet.t -> DependencyKey.RegisteredSet.t

  val add_to_transaction
    :  t ->
    DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t

  val add_pessimistic_transaction
    :  t ->
    DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t
end
