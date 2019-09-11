(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
module SharedMemory = Memory

module TypeAnnotationsValue = struct
  type t = LocalAnnotationMap.t

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

include SharedMemory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (TypeAnnotationsValue)
(** A map of function definitions (indexed by Reference.t key) to to annotations for each statement *)

module Keys = SharedMemory.NoCache.Make (SharedMemoryKeys.ReferenceKey) (AnnotationsKeyValue)

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
