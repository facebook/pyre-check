(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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

module NoCache (Key : KeyType) (Value : ValueType) : sig
  type decodable += Decoded of Key.out * Value.t option

  val serialize_key : Key.t -> string

  val hash_of_key : Key.t -> string

  val compute_hashes_to_keys : keys:Key.t list -> string Core.String.Map.t

  include
    SharedMemory.NoCache
      with type t = Value.t
       and type key = Key.t
       and module KeySet = Set.Make(Key)
       and module KeyMap = MyMap.Make(Key)
end

module WithCache (Key : KeyType) (Value : ValueType) : sig
  type decodable += Decoded of Key.out * Value.t option

  val serialize_key : Key.t -> string

  val hash_of_key : Key.t -> string

  val compute_hashes_to_keys : keys:Key.t list -> string Core.String.Map.t

  include
    SharedMemory.WithCache
      with type t = Value.t
       and type key = Key.t
       and module KeySet = Set.Make(Key)
       and module KeyMap = MyMap.Make(Key)
end

val get_heap_handle : Configuration.Analysis.t -> SharedMemory.handle

val worker_garbage_control : Gc.control

val report_statistics : unit -> unit

val save_shared_memory : path:string -> unit

val load_shared_memory : path:string -> unit

val unsafe_little_endian_representation : key:Digest.t -> Int64.t

module SingletonKey : sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int

  type out = int

  val from_string : string -> int

  val key : t
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
