(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

module LocalAnnotationsValue = struct
  type t = (Reference.t * LocalAnnotationMap.t) list

  let prefix = Prefix.make ()

  let description = "Node type resolution"

  let unmarshall value = Marshal.from_string value 0
end

module LocalAnnotations =
  Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (LocalAnnotationsValue)

let add = LocalAnnotations.add

let get = LocalAnnotations.get

let get_local_annotation_map ~qualifier name =
  get qualifier
  >>= fun local_annotations -> List.Assoc.find local_annotations name ~equal:Reference.equal


let remove qualifiers = LocalAnnotations.KeySet.of_list qualifiers |> LocalAnnotations.remove_batch
