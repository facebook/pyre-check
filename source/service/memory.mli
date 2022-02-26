(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Set = Caml.Set
module SharedMemory = Hack_parallel.Std.SharedMem

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

val initialize_for_tests : unit -> unit

val get_heap_handle : Configuration.Analysis.t -> SharedMemory.handle

val heap_size : unit -> int

val worker_garbage_control : Caml.Gc.control

val report_statistics : unit -> unit

exception SavedStateLoadingFailure of string

val save_shared_memory : path:string -> configuration:Configuration.Analysis.t -> unit

val load_shared_memory : path:string -> configuration:Configuration.Analysis.t -> unit

val reset_shared_memory : unit -> unit

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

module type InternerValueType = sig
  include ValueType

  val to_string : t -> string
end

(* Provide a unique integer for a given value. *)
module Interner (Value : InternerValueType) : sig
  type t = int

  val intern : Value.t -> t

  val unintern : t -> Value.t

  val compare : t -> t -> int
end
