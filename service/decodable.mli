(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

(** Infrastructure for decoding values from SharedHeap
    Heap tables registered with this module stores enough information
    such that an arbitrary (key, value) pair can be decoded back to an OCaml
    type.
    The [decode] function takes a pair and returns a value of type decodable if
    a decoder was found for that key.
    [NoCache] and [WithCache] are augmented with a [Decoded] constructor that
    can be used to check if the decodable value come from this table.
*)

type decodable = ..


type decoding_error = [
  | `Malformed_key
  | `Unknown_type
  | `Decoder_failure of exn
]


val decode: key: string -> value: string -> (decodable, decoding_error) result


module type KeyType = sig
  include SharedMem.UserKeyType
  type out
  val from_string: string -> out
end


module NoCache (Key: KeyType) (Value: Value.Type): sig
  type decodable += Decoded of Key.out * Value.t
  include Memory.NoCache with
    type t = Value.t
    and type key = Key.t
    and module KeySet = Set.Make (Key)
    and module KeyMap = MyMap.Make (Key)
end


module WithCache (Key: KeyType) (Value: Value.Type): sig
  type decodable += Decoded of Key.out * Value.t
  include Memory.WithCache with
    type t = Value.t
    and type key = Key.t
    and module KeySet = Set.Make (Key)
    and module KeyMap = MyMap.Make (Key)
end
