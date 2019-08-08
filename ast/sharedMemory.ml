(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
module SharedMemory = Memory

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Int.of_string
end

module Modules = struct
  module ModuleValue = struct
    type t = Module.t

    let prefix = Prefix.make ()

    let description = "Module"

    let unmarshall value = Marshal.from_string value 0
  end

  module Modules = SharedMemory.WithCache (Reference.Key) (ModuleValue)

  let add ~qualifier ~ast_module = Modules.write_through qualifier ast_module

  let remove ~qualifiers =
    let references = List.filter ~f:Modules.mem qualifiers in
    Modules.remove_batch (Modules.KeySet.of_list references)


  let get ~qualifier = Modules.get qualifier

  let get_exports ~qualifier = get ~qualifier >>| Module.wildcard_exports

  let exists ~qualifier = Modules.mem qualifier

  let hash_of_key = Modules.hash_of_key

  let serialize_key = Modules.serialize_key

  let compute_hashes_to_keys = Modules.compute_hashes_to_keys
end

let heap_size () =
  Memory.SharedMemory.heap_size () |> Float.of_int |> (fun size -> size /. 1.0e6) |> Int.of_float
