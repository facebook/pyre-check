(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
module SharedMemory = Memory

type annotation_map = {
  precondition: Annotation.t Reference.Map.Tree.t;
  postcondition: Annotation.t Reference.Map.Tree.t;
}
[@@deriving eq]

type annotations = annotation_map Int.Map.Tree.t [@@deriving eq]

let pp_annotations formatter map =
  Format.fprintf formatter "{";
  let pp_annotation_map ~key ~data:{ precondition; postcondition } =
    let pp_state formatter map =
      let pp_element ~key ~data =
        Format.fprintf formatter "\"%a\": \"%a\", " Reference.pp key Annotation.pp data
      in
      Format.fprintf formatter "{";
      Reference.Map.Tree.iteri map ~f:pp_element;
      Format.fprintf formatter "}"
    in
    Format.fprintf
      formatter
      "%d: { \"Precondition\": %a, \"Postcondition\": %a}"
      key
      pp_state
      precondition
      pp_state
      postcondition
  in
  Int.Map.Tree.iteri map ~f:pp_annotation_map


let show_annotations map = Format.asprintf "%a" pp_annotations map

module TypeAnnotationsValue = struct
  type t = annotations

  let prefix = Prefix.make ()

  let description = "Node type resolution"

  let unmarshall value = Marshal.from_string value 0
end

module AnnotationsKeyValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Node type resolution keys"

  let unmarshall value = Marshal.from_string value 0
end

include SharedMemory.WithCache (Reference.Key) (TypeAnnotationsValue)
(** A map of function definitions (indexed by Reference.t key) to to annotations for each statement *)

module Keys = SharedMemory.NoCache (Reference.Key) (AnnotationsKeyValue)

let remove qualifiers =
  let accesses =
    List.filter_map ~f:Keys.get qualifiers |> List.concat |> List.filter ~f:mem |> KeySet.of_list
  in
  remove_batch accesses;
  Keys.remove_batch (Keys.KeySet.of_list qualifiers)


let add ~qualifier name value =
  ( match Keys.get qualifier with
  | None -> Keys.add qualifier [name]
  | Some names -> Keys.add qualifier (name :: names) );
  add name value


let get_keys ~qualifiers = List.filter_map qualifiers ~f:Keys.get |> List.concat
