(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open EnvironmentSharedMemory

let compute_hashes_to_keys ~indices ~annotations =
  let add_index_mappings map key =
    Map.add_exn
      map
      ~key:(OrderAnnotations.hash_of_key key)
      ~data:(Base64.encode_exn (OrderAnnotations.serialize_key key))
    |> Map.add_exn
      ~key:(OrderEdges.hash_of_key key)
      ~data:(Base64.encode_exn (OrderEdges.serialize_key key))
    |> Map.add_exn
      ~key:(OrderBackedges.hash_of_key key)
      ~data:(Base64.encode_exn (OrderBackedges.serialize_key key))
  in
  let add_annotation_mappings map annotation =
    Map.add_exn
      map
      ~key:(OrderIndices.hash_of_key annotation)
      ~data:(Base64.encode_exn (OrderIndices.serialize_key annotation))
  in
  List.fold indices ~init:String.Map.empty ~f:add_index_mappings
  |> fun map -> List.fold annotations ~init:map ~f:add_annotation_mappings
