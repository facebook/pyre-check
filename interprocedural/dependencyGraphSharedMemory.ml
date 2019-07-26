(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
module SharedMemory = Memory

module OverrideTypes = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "overriding types"

  let unmarshall value = Marshal.from_string value 0
end

(* Maps the method access to the next subtypes that override that method *)
module Overrides = SharedMemory.WithCache (Reference.Key) (OverrideTypes)

let add_overriding_types ~member ~subtypes = Overrides.add member subtypes

let get_overriding_types ~member = Overrides.get member

let overrides_exist member = Overrides.mem member
