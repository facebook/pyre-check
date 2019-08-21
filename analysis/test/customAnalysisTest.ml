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

let test_ordered_nested_defines _ =
  let assert_ordered_nested_defines source expected =
    let nested =
      parse_single_define source
      |> Node.create_with_default_location
      |> CustomAnalysis.ordered_nested_defines
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
  assert_ordered_nested_defines
    {|
      def a():
        def b():
          def d():
            pass
        def c():
          pass
    |}
    ["d"; "b"; "c"];
  assert_ordered_nested_defines
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
  "customAnalysis" >::: ["ordered_nested_defines" >:: test_ordered_nested_defines] |> Test.run
