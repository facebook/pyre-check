(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Set = Caml.Set

module SharedMemory = Hack_parallel.Std.SharedMem
(** Infrastructure for decoding values from SharedHeap Heap tables registered with this module
    stores enough information such that an arbitrary (key, value) pair can be decoded back to an
    OCaml type. The [decode] function takes a pair and returns a value of type decodable if a
    decoder was found for that key. [NoCache] and [WithCache] are augmented with a [Decoded]
    constructor that can be used to check if the decodable value come from this table. *)

type decodable = ..

type decoding_error =
  [ `Malformed_key
  | `Unknown_type
  | `Decoder_failure of exn
  ]

val decode : key:string -> value:string -> (decodable, decoding_error) result

module type KeyType = sig
  include SharedMem.UserKeyType

  type out

  val from_string : string -> out
end

module type ValueType = sig
  include Value.Type

  val unmarshall : string -> t
end

module NoCache : sig
  module type S = sig
    include SharedMemory.NoCache

    type key_out

    type decodable += Decoded of key_out * t option

    val serialize_key : key -> string

    val hash_of_key : key -> string

    val compute_hashes_to_keys : keys:key list -> string String.Map.t
  end

  module Make (Key : KeyType) (Value : ValueType) : sig
    include
      S
        with type t = Value.t
         and type key = Key.t
         and type key_out = Key.out
         and module KeySet = Set.Make(Key)
         and module KeyMap = MyMap.Make(Key)
  end
end

module WithCache : sig
  module type S = sig
    include SharedMemory.WithCache

    type key_out

    type decodable += Decoded of key_out * t option

    val serialize_key : key -> string

    val hash_of_key : key -> string

    val compute_hashes_to_keys : keys:key list -> string String.Map.t
  end

  module Make (Key : KeyType) (Value : ValueType) : sig
    include
      S
        with type t = Value.t
         and type key = Key.t
         and type key_out = Key.out
         and module KeySet = Set.Make(Key)
         and module KeyMap = MyMap.Make(Key)
  end
end

val get_heap_handle : Configuration.Analysis.t -> SharedMemory.handle

val heap_size : unit -> int

val worker_garbage_control : Caml.Gc.control

val report_statistics : unit -> unit

val save_shared_memory : path:string -> unit

val load_shared_memory : path:string -> unit

val reset_shared_memory : unit -> unit

val unsafe_little_endian_representation : key:Caml.Digest.t -> Int64.t

module SingletonKey : sig
  include KeyType with type out = int

  val key : t
end

module type ComparableValueType = sig
  include ValueType

  val compare : t -> t -> int
end

module type SerializableValueType = sig
  type t

  module Serialized : ValueType

  val serialize : t -> Serialized.t

  val deserialize : Serialized.t -> t
end

module Serializer (Value : SerializableValueType) : sig
  val load : unit -> Value.t

  val store : Value.t -> unit
end

(* Deliberately left abstract to prevent calls to DependencyKey.Transaction.add from outside this
   module *)
type 'keyset transaction_element

module DependencyKey : sig
  module type S = sig
    include KeyType

    module KeySet : Set.S with type elt = t

    val encode : t -> int

    val decode : int -> t

    module Transaction : sig
      type t

      val empty : t

      (* Cannot be called from outside this module *)
      val add : t -> KeySet.t transaction_element -> t

      val execute : t -> update:(unit -> 'a) -> 'a * KeySet.t
    end
  end

  module Make (Key : KeyType) : S with type t = Key.t
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

  val add_dependency : key -> DependencyKey.t -> unit

  val get_dependents : key -> DependencyKey.KeySet.t

  val get_all_dependents : KeySet.t -> DependencyKey.KeySet.t

  val add_to_transaction
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

  val add_dependency : key -> DependencyKey.t -> unit

  val get : ?dependency:DependencyKey.t -> key -> t option

  val get_dependents : key -> DependencyKey.KeySet.t

  val get_all_dependents : KeySet.t -> DependencyKey.KeySet.t

  val add_to_transaction
    :  DependencyKey.Transaction.t ->
    keys:KeySet.t ->
    DependencyKey.Transaction.t
end
