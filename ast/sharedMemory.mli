(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module IntKey : Memory.KeyType with type t = int and type out = int

module WildcardExports : sig
  val get : qualifier:Reference.t -> Reference.t list option

  val add : Source.t -> unit

  val remove : qualifiers:Reference.t list -> unit

  (* Exposed for testing. *)
  val hash_of_key : Reference.t -> string

  val serialize_key : Reference.t -> string

  val compute_hashes_to_keys : keys:Reference.t list -> string Core.String.Map.t
end

val heap_size : unit -> int
