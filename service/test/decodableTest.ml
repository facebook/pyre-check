(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open Service
open EnvironmentSharedMemory


let test_decodable _ =
  let assert_decode prefix key value expected =
    let key = Prefix.make_key prefix key in
    assert_equal
      (Decodable.decode ~key ~value)
      (Ok expected)
  in
  assert_decode
    EdgeValue.prefix
    (IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures]) (OrderEdges.Decoded (1234, []));
  assert_decode
    BackedgeValue.prefix
    (IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures]) (OrderBackedges.Decoded (1234, []));
  assert_equal
    (Error `Malformed_key)
    (Decodable.decode ~key:"" ~value:(Marshal.to_string [] [Marshal.Closures]));
  let unregistered_prefix = Prefix.make () in
  assert_equal
    (Error `Unknown_type)
    (Decodable.decode
       ~key:(Prefix.make_key unregistered_prefix "")
       ~value:(Marshal.to_string [] [Marshal.Closures]))


let test_serialize_key _ =
  assert_equal (OrderEdges.serialize_key 1234) (Prefix.make_key EdgeValue.prefix "1234");
  assert_equal (OrderBackedges.serialize_key 1234) (Prefix.make_key BackedgeValue.prefix "1234")


let test_hash_of_key _ =
  assert_equal
    (OrderEdges.hash_of_key 1234)
    (OrderEdges.string_of_key 1234
     |> (fun key -> Memory.unsafe_little_endian_representation ~key)
     |> Int64.to_string);
  assert_equal
    (OrderBackedges.hash_of_key 1234)
    (OrderBackedges.string_of_key 1234
     |> (fun key -> Memory.unsafe_little_endian_representation ~key)
     |> Int64.to_string);
  (* Make sure different modules have different caches. *)
  assert_equal
    false
    (String.equal
       (OrderBackedges.hash_of_key 1234)
       (OrderEdges.string_of_key 1234
        |> (fun key -> Memory.unsafe_little_endian_representation ~key)
        |> Int64.to_string))


let () =
  "decodable">:::[
    "decodable">::test_decodable;
    "serialize_key">::test_serialize_key;
    "hash_of_key">::test_hash_of_key;
  ]
  |> Test.run
