(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module SharedMemory = Memory

module OverrideTypes = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "overriding types"

  let unmarshall value = Marshal.from_string value 0
end

(* Maps the method access to the next subtypes that override that method *)
module Overrides =
  SharedMemory.WithCache.Make (Analysis.SharedMemoryKeys.ReferenceKey) (OverrideTypes)

let add_overriding_types ~member ~subtypes = Overrides.add member subtypes

let get_overriding_types ~member = Overrides.get member

let overrides_exist member = Overrides.mem member

let record_overrides ?maximum_overrides_to_analyze overrides =
  let record_override_edge ~key:member ~data:subtypes =
    let number_of_overrides = List.length subtypes in
    match maximum_overrides_to_analyze with
    | Some cap ->
        if number_of_overrides < cap then
          add_overriding_types ~member ~subtypes
        else
          Log.info
            "Omitting overrides for `%s`, as it has %d overrides."
            (Reference.show member)
            number_of_overrides
    | None ->
        if number_of_overrides > 50 then
          Log.warning
            "`%s` has %d overrides, this might slow down the analysis considerably."
            (Reference.show member)
            number_of_overrides;
        add_overriding_types ~member ~subtypes
  in
  Reference.Map.iteri overrides ~f:record_override_edge
