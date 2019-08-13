(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module SharedMemory = Memory

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Int.of_string
end

module WildcardExports = struct
  module WildcardExportsValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Wildcard exports"

    let unmarshall value = Marshal.from_string value 0
  end

  module Exports = SharedMemory.WithCache (Reference.Key) (WildcardExportsValue)

  let add ({ Source.qualifier; _ } as source) =
    let wildcard_exports = Module.wildcard_exports_from_source source in
    Exports.write_through qualifier wildcard_exports


  let remove ~qualifiers =
    let references = List.filter ~f:Exports.mem qualifiers in
    Exports.remove_batch (Exports.KeySet.of_list references)


  let get ~qualifier = Exports.get qualifier

  let hash_of_key = Exports.hash_of_key

  let serialize_key = Exports.serialize_key

  let compute_hashes_to_keys = Exports.compute_hashes_to_keys
end

let heap_size () =
  Memory.SharedMemory.heap_size () |> Float.of_int |> (fun size -> size /. 1.0e6) |> Int.of_float
