(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Analysis

open OUnit2

let test_compute_hashes_to_keys _ =
  let open Service.EnvironmentSharedMemory in
  assert_equal
    ~cmp:(String.Map.equal String.equal)
    (String.Map.of_alist_exn [
        OrderEdges.hash_of_key 15, Base64.encode_exn (OrderEdges.serialize_key 15 );
        OrderBackedges.hash_of_key 15, Base64.encode_exn (OrderBackedges.serialize_key 15);
        OrderAnnotations.hash_of_key 15, Base64.encode_exn (OrderAnnotations.serialize_key 15);
      ])
    (Service.TypeOrder.compute_hashes_to_keys ~indices:[15] ~annotations:[]);
  assert_equal
    ~cmp:(String.Map.equal String.equal)
    (String.Map.of_alist_exn [
        OrderIndices.hash_of_key (Type.Primitive "fifteen"),
        Base64.encode_exn (OrderIndices.serialize_key (Type.Primitive "fifteen"));
      ])
    (Service.TypeOrder.compute_hashes_to_keys ~indices:[] ~annotations:[Type.Primitive "fifteen"])


let () =
  "typeOrder">:::[
    "compute_hashes_to_keys">::test_compute_hashes_to_keys;
  ]
  |> Test.run
