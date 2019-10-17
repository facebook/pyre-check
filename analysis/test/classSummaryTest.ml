(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Test

let test_is_unit_test _ =
  let assert_is_unit_test source expected =
    parse_single_class source
    |> ClassSummary.create
    |> ClassSummary.is_unit_test
    |> assert_equal expected
  in
  assert_is_unit_test {|
    class unittest.TestCase(object):
      pass
  |} true;
  assert_is_unit_test {|
    class unittest.case.TestCase(object):
      pass
  |} true;
  assert_is_unit_test {|
    class a.TestCase(unittest.TestCase):
      pass
  |} false;
  ()


let () = "summary" >::: ["is_unit_test" >:: test_is_unit_test] |> Test.run
