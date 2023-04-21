(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_format_string context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors {|
      def foo() -> None:
        f'foo{1}'
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        f'foo{1 + "x"}'
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + "x"}'
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + 2}'
    |}
    [];
  assert_type_errors
    {|
      def boo() -> int:
        return 1

      def foo() -> None:
        f'{boo() + "x"}'
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors {|
      def foo() -> None:
        f'{{x}}'
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        f'{{{x}}}'
    |}
    ["Unbound name [10]: Name `x` is used but not defined in the current scope."]


let test_multiline_format_string context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> None:
        # pyre-fixme[58]
        # Some comment.
        f"""
        foo
        {1 + "hello"}
        bar
        {1 + "world"}
        baz
        """

        # pyre-fixme[58]
        # Some comment.
        f"""
        {1 + "hello2"}
        """ f"""
        {1 + "hello3"}
        """ f"""
        {1 + "hello4"}
        """

        # pyre-fixme[58]
        f"{1 + 'hello5'}"
    |}
    [];
  (* Only one error ignored. *)
  assert_type_errors
    {|
      def foo() -> None:
        # pyre-fixme[6]
        f"""

        {1 + "world"}

        {" ".join(1)}

        """
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors
    {|
      def foo() -> None:
        # Unignored format string
        f"""
        {1 + "world"}
        """
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `int` and `str`."];
  (* When the error is fixed, there is only one "unused ignore" error. *)
  assert_type_errors
    {|
      def foo() -> None:
        # pyre-fixme[58]
        # Some comment.
        f"""
        foo
        {1 + 2}
        bar
        {1 + 3}
        baz
        """
    |}
    [
      "Unused ignore [0]: The `pyre-ignore[58]` or `pyre-fixme[58]` comment is not suppressing \
       type errors, please remove it.";
    ];
  (* Even if there is a subexpression with no error, we won't get a spurious "unused ignore" because
     the fixme comment is being "used" by the subexpression that does have an error. *)
  assert_type_errors
    {|
      def foo() -> None:
        # pyre-fixme[58]
        # Some comment.
        f"""
        foo
        {1 + "hello"}
        bar
        {1 + 3}
        baz
        """
    |}
    [];
  (* Note: We end up suppressing any error that happens to be on the last line of the format string.
     There's no good way to get around this since fixmes work on entire lines. *)
  assert_type_errors
    {|
      def foo() -> None:
        # pyre-fixme[58]
        f"""
        foo
        {1 + "hello"}
        bar
        """ + (1 + "hello")
    |}
    [];
  ()


let () =
  "format_string"
  >::: [
         "format_string" >:: test_format_string;
         "multiline_format_string" >:: test_multiline_format_string;
       ]
  |> Test.run
