(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module OverrideTypes = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "overriding types"
end

(* Maps the method access to the next subtypes that override that method *)
module Overrides = Memory.WithCache.Make (Analysis.SharedMemoryKeys.ReferenceKey) (OverrideTypes)

let add_overriding_types ~member ~subtypes = Overrides.add member subtypes

let get_overriding_types ~member = Overrides.get member

let overrides_exist member = Overrides.mem member

type cap_overrides_result = {
  overrides: Reference.t list Reference.Map.t;
  skipped_overrides: Reference.t list;
}

let cap_overrides ?maximum_overrides_to_analyze overrides =
  (* Keep the information of whether we're skipping overrides in a ref that we accumulate while we
     filter the map. *)
  let skipped_overrides = ref [] in
  let keep_override_edge ~key:member ~data:subtypes =
    let number_of_overrides = List.length subtypes in
    match maximum_overrides_to_analyze with
    | Some cap ->
        if number_of_overrides < cap then
          true
        else begin
          Log.info
            "Omitting overrides for `%s`, as it has %d overrides."
            (Reference.show member)
            number_of_overrides;
          skipped_overrides := member :: !skipped_overrides;
          false
        end
    | None ->
        if number_of_overrides > 50 then
          Log.warning
            "`%s` has %d overrides, this might slow down the analysis considerably."
            (Reference.show member)
            number_of_overrides;
        true
  in
  let overrides = Reference.Map.filteri overrides ~f:keep_override_edge in
  { overrides; skipped_overrides = !skipped_overrides }


let record_overrides overrides =
  let record_override_edge ~key:member ~data:subtypes = add_overriding_types ~member ~subtypes in
  Reference.Map.iteri overrides ~f:record_override_edge


let remove_overriding_types keys = Overrides.remove_batch (Overrides.KeySet.of_list keys)
