(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open EnvironmentSharedMemory

let compute_hashes_to_keys ~indices ~annotations =
  let add_index_mappings map key =
    Map.set map ~key:(OrderAnnotations.hash_of_key key) ~data:(OrderAnnotations.serialize_key key)
    |> Map.set ~key:(OrderEdges.hash_of_key key) ~data:(OrderEdges.serialize_key key)
    |> Map.set ~key:(OrderBackedges.hash_of_key key) ~data:(OrderBackedges.serialize_key key)
  in
  let add_annotation_mappings map annotation =
    Map.set
      map
      ~key:(OrderIndices.hash_of_key annotation)
      ~data:(OrderIndices.serialize_key annotation)
  in
  List.fold indices ~init:String.Map.empty ~f:add_index_mappings
  |> fun map -> List.fold annotations ~init:map ~f:add_annotation_mappings
