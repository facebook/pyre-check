(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module SharedMemory = Memory

module ConfigurationSharedMemory =
  SharedMemory.WithCache.Make
    (struct
      include String

      type out = string

      let from_string = ident
    end)
    (struct
      type nonrec t = Configuration.Analysis.t

      let prefix = Prefix.make ()

      let description = "Analysis configuration"

      let unmarshall value = Marshal.from_string value 0
    end)

let key = "analysis_configuration"

let register_configuration configuration =
  let () =
    if ConfigurationSharedMemory.mem key then
      ConfigurationSharedMemory.remove_batch (ConfigurationSharedMemory.KeySet.singleton key)
  in
  ConfigurationSharedMemory.add key configuration


let get_configuration () =
  match ConfigurationSharedMemory.get key with
  | None -> failwith "Attempted to get unregistered configuration"
  | Some configuration -> configuration
