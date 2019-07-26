(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2

let test_little_endian_representation _ =
  let assert_correctly_represented ~expected ~key =
    let digest = Digest.from_hex key in
    let little_endian_representation = Memory.unsafe_little_endian_representation ~key:digest in
    assert_equal ~printer:Int64.to_string (Int64.of_string expected) little_endian_representation
  in
  assert_correctly_represented
    ~expected:"0x0706050403020100"
    ~key:"000102030405060708090a0b0c0d0e0f";
  assert_correctly_represented
    ~expected:"0x0102030405060708"
    ~key:"0807060504030201090a0b0c0d0e0fff";
  assert_raises (Invalid_argument "Digest.to_hex") (fun () ->
      Memory.unsafe_little_endian_representation ~key:"short");
  assert_raises (Invalid_argument "Digest.to_hex") (fun () ->
      Memory.unsafe_little_endian_representation ~key:"Too_long_of_a_message")


module MockEdgeValue = struct
  type t = Analysis.ClassHierarchy.Target.t list

  let prefix = Prefix.make ()

  let description = "Edges"

  let unmarshall value = Marshal.from_string value 0
end

module MockEdges = Memory.WithCache (Ast.SharedMemory.IntKey) (MockEdgeValue)

module MockBackedgeValue = struct
  type t = Analysis.ClassHierarchy.Target.Set.Tree.t

  let prefix = Prefix.make ()

  let description = "Backedges"

  let unmarshall value = Marshal.from_string value 0
end

module MockBackedges = Memory.WithCache (Ast.SharedMemory.IntKey) (MockBackedgeValue)

module MockAnnotationValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Annotations"

  (* Shared memory does not marshall strings *)
  let unmarshall value = value
end

module MockAnnotations = Memory.WithCache (Ast.SharedMemory.IntKey) (MockAnnotationValue)

let test_decodable _ =
  let assert_decode prefix key value expected =
    let key = Prefix.make_key prefix key in
    assert_equal (Memory.decode ~key ~value) (Ok expected)
  in
  assert_decode
    MockEdgeValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures])
    (MockEdges.Decoded (1234, Some []));
  assert_decode
    MockBackedgeValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    (Marshal.to_string [] [Marshal.Closures])
    (MockBackedges.Decoded (1234, Some Analysis.ClassHierarchy.Target.Set.Tree.empty));
  assert_decode
    MockEdgeValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    "can't decode this"
    (MockEdges.Decoded (1234, None));
  assert_decode
    MockAnnotationValue.prefix
    (Ast.SharedMemory.IntKey.to_string 1234)
    "can decode this"
    (MockAnnotations.Decoded (1234, Some "can decode this"));
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
    (MockEdges.serialize_key 1234)
    (Prefix.make_key MockEdgeValue.prefix "1234" |> Base64.encode_exn);
  assert_equal
    (MockBackedges.serialize_key 1234)
    (Prefix.make_key MockBackedgeValue.prefix "1234" |> Base64.encode_exn)


let test_hash_of_key _ =
  assert_equal
    (MockEdges.hash_of_key 1234)
    ( MockEdges.string_of_key 1234
    |> (fun key -> Memory.unsafe_little_endian_representation ~key)
    |> Int64.to_string );
  assert_equal
    (MockBackedges.hash_of_key 1234)
    ( MockBackedges.string_of_key 1234
    |> (fun key -> Memory.unsafe_little_endian_representation ~key)
    |> Int64.to_string );

  (* Make sure different modules have different caches. *)
  assert_equal
    false
    (String.equal
       (MockBackedges.hash_of_key 1234)
       ( MockEdges.string_of_key 1234
       |> (fun key -> Memory.unsafe_little_endian_representation ~key)
       |> Int64.to_string ))


let () =
  "memory"
  >::: [ "little_endian_representation" >:: test_little_endian_representation;
         "decodable" >:: test_decodable;
         "serialize_key" >:: test_serialize_key;
         "hash_of_key" >:: test_hash_of_key ]
  |> Test.run
