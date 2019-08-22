(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Ast
open Statement
open Test

let test_nested_defines_deep_to_shallow _ =
  let assert_nested_defines_deep_to_shallow source expected =
    let nested =
      parse_single_define source
      |> Node.create_with_default_location
      |> CustomAnalysis.nested_defines_deep_to_shallow
      |> List.map ~f:Node.value
      |> List.map ~f:(fun { Define.signature = { Define.name; _ }; _ } -> name)
    in
    let expected = List.map ~f:Reference.create expected in
    assert_equal
      ~printer:(List.to_string ~f:Reference.show)
      ~cmp:(List.equal Reference.equal)
      expected
      nested
  in
  assert_nested_defines_deep_to_shallow
    {|
      def a():
        def b():
          def d():
            pass
        def c():
          pass
    |}
    ["d"; "b"; "c"];
  assert_nested_defines_deep_to_shallow
    {|
      def a():
        def b():
          def d():
            pass
          def c():
            pass
    |}
    ["d"; "c"; "b"]


let () =
  "customAnalysis"
  >::: ["nested_defines_deep_to_shallow" >:: test_nested_defines_deep_to_shallow]
  |> Test.run
