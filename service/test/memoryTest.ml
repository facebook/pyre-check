(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2


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


let () =
  "memory">:::[
    "little_endian_representation">::test_little_endian_representation;
  ]
  |> Test.run
