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

type record_overrides_result = { skipped_overrides: Reference.t list }

let record_overrides ?maximum_overrides_to_analyze overrides =
  let record_override_edge ~key:member ~data:subtypes skipped_overrides =
    let number_of_overrides = List.length subtypes in
    match maximum_overrides_to_analyze with
    | Some cap ->
        if number_of_overrides < cap then begin
          add_overriding_types ~member ~subtypes;
          skipped_overrides
        end
        else begin
          Log.info
            "Omitting overrides for `%s`, as it has %d overrides."
            (Reference.show member)
            number_of_overrides;
          member :: skipped_overrides
        end
    | None ->
        if number_of_overrides > 50 then
          Log.warning
            "`%s` has %d overrides, this might slow down the analysis considerably."
            (Reference.show member)
            number_of_overrides;
        add_overriding_types ~member ~subtypes;
        skipped_overrides
  in
  Reference.Map.fold overrides ~f:record_override_edge ~init:[]
  |> fun skipped_overrides -> { skipped_overrides }


let remove_overriding_types keys = Overrides.remove_batch (Overrides.KeySet.of_list keys)
