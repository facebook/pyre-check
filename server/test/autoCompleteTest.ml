(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open OUnit2
open Server
open Test

let test_remove_dot _ =
  let assert_remove_dot ~position ~original ~expected =
    let actual = AutoComplete.remove_dot ~cursor_position:position original in
    assert_equal ~printer:ident expected actual
  in
  assert_remove_dot
    ~position:{ line = 3; column = 30 }
    ~original:{|
      a = 1 + 1
      function_call(variable1., variable2)
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      function_call(variable1, variable2)
      b = 2 + 2
    |};

  (* Edge case: DOT is the first character in a line *)
  assert_remove_dot
    ~position:{ line = 1; column = 1 }
    ~original:"., variable2)"
    ~expected:", variable2)";

  (* Edge case: DOT is the last character in a line *)
  assert_remove_dot
    ~position:{ line = 3; column = 16 }
    ~original:{|
      a = 1 + 1
      variable1.
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      variable1
      b = 2 + 2
    |}


let test_get_completion_items _ =
  let cursor_position = { Location.line = 8; column = 4 } in
  let source =
    {|
      class A:
        foo: bool
        def bar() -> int:
          return 3
      def main() -> None:
        a = A()
        a.
    |}
    |> trim_extra_indentation
  in
  let configuration, state = RequestTest.initialize [] in
  let path = mock_path "a.py" in
  let state = { state with open_documents = PyrePath.Map.singleton path source } in
  let completion_item_list =
    AutoComplete.get_completion_items ~state ~configuration ~path ~cursor_position
  in
  let open LanguageServer.Types in
  let position = { Position.line = 7; character = 4 } in
  let range = { Range.start = position; end_ = position } in
  assert_equal
    ~printer:CompletionItems.show
    [ { label = "bar() -> int"; detail = "() -> int"; textEdit = { range; newText = "bar()" } };
      { label = "foo"; detail = "bool"; textEdit = { range; newText = "foo" } } ]
    completion_item_list


let () =
  "autoComplete"
  >::: ["remove_dot" >:: test_remove_dot; "get_completion_items" >:: test_get_completion_items]
  |> Test.run
