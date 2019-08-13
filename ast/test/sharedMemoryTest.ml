(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Core
open Test
open OUnit2

let test_compute_hashes_to_keys _ =
  let open Ast.SharedMemory in
  let assert_mapping_equal expected actual =
    assert_equal
      ~printer:(fun map -> Sexp.to_string (String.Map.sexp_of_t String.sexp_of_t map))
      ~cmp:(String.Map.equal String.equal)
      (String.Map.of_alist_exn expected)
      actual
  in
  assert_mapping_equal
    [ WildcardExports.hash_of_key !&"foo", WildcardExports.serialize_key !&"foo";
      WildcardExports.hash_of_key !&"bar", WildcardExports.serialize_key !&"bar";
      WildcardExports.hash_of_key !&"foo.b", WildcardExports.serialize_key !&"foo.b" ]
    (WildcardExports.compute_hashes_to_keys ~keys:[!&"foo"; !&"bar"; !&"foo.b"])


let () =
  "ast_shared_memory" >::: ["compute_hashes_to_keys" >:: test_compute_hashes_to_keys] |> Test.run
