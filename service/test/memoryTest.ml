(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Service.EnvironmentSharedMemory


let test_little_endian_representation _ =
  let assert_correctly_represented ~expected ~key =
    let digest = Digest.from_hex key in
    let little_endian_representation = Memory.unsafe_little_endian_representation ~key:digest in
    assert_equal
      ~printer:Int64.to_string
      (Int64.of_string expected)
      little_endian_representation
  in
  assert_correctly_represented
    ~expected:"0x0706050403020100"
    ~key:"000102030405060708090a0b0c0d0e0f";
  assert_correctly_represented
    ~expected:"0x0102030405060708"
    ~key:"0807060504030201090a0b0c0d0e0fff";
  assert_raises
    (Invalid_argument "Digest.to_hex")
    (fun () -> Memory.unsafe_little_endian_representation ~key:"short");
  assert_raises
    (Invalid_argument "Digest.to_hex")
    (fun () -> Memory.unsafe_little_endian_representation ~key:"Too_long_of_a_message")


let test_decodable _ =
  let assert_decode prefix key value expected =
    let key = Prefix.make_key prefix key in
    assert_equal
      (Memory.decode ~key ~value)
      (Ok expected)
  in
  assert_decode
    EdgeValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures]) (OrderEdges.Decoded (1234, Some []));
  assert_decode
    BackedgeValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures]) (OrderBackedges.Decoded (1234, Some []));
  assert_decode
    EdgeValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    "can't decode this" (OrderEdges.Decoded (1234, None));
  assert_equal
    (Error `Malformed_key)
    (Memory.decode ~key:"" ~value:(Marshal.to_string [] [Marshal.Closures]));
  let unregistered_prefix = Prefix.make () in
  assert_equal
    (Error `Unknown_type)
    (Memory.decode
       ~key:(Prefix.make_key unregistered_prefix "")
       ~value:(Marshal.to_string [] [Marshal.Closures]))


let test_serialize_key _ =
  assert_equal
    (OrderEdges.serialize_key 1234)
    (Prefix.make_key EdgeValue.prefix "1234"
     |> Base64.encode_exn);
  assert_equal
    (OrderBackedges.serialize_key 1234)
    (Prefix.make_key BackedgeValue.prefix "1234"
     |> Base64.encode_exn)


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
  "memory">:::[
    "little_endian_representation">::test_little_endian_representation;
    "decodable">::test_decodable;
    "serialize_key">::test_serialize_key;
    "hash_of_key">::test_hash_of_key;
  ]
  |> Test.run
