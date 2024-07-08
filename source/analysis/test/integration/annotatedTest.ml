(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_annotated context =
  assert_type_errors
    {|
      from builtins import expect_int
      import typing_extensions
      def foo(annotated: typing_extensions.Annotated[int, True]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `int`."]
    context;
  assert_type_errors
    {|
      import typing
      def foo(annotated: typing.Annotated[int, True]) -> None:
        pass
    |}
    []
    context;
  assert_type_errors
    {|
      import typing
      def foo(annotated: typing.Annotated) -> None:
        pass
    |}
    [
      "Invalid type parameters [24]: Generic type `typing.Annotated` expects at least 2 type \
       parameters.";
    ]
    context;
  assert_type_errors
    {|
      import typing
      typing.Annotated()
    |}
    ["Invalid class instantiation [45]: `typing.Annotated` cannot be instantiated."]
    context;
  assert_type_errors
    {|
      import typing
      def foo(annotated: typing.Annotated[int]) -> None:
        pass
    |}
    [
      "Invalid type parameters [24]: Generic type `typing.Annotated` expects at least 2 type \
       parameters, received 1.";
    ]
    context;
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(annotated: typing.Annotated[int, True]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `int`."]
    context;
  assert_type_errors
    {|
      from builtins import expect_int
      from typing import Annotated
      def foo(annotated: Annotated[Annotated[Annotated[int, True], True], True]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `int`."]
    context;
  assert_type_errors
    {|
      from builtins import expect_int
      from typing import Annotated
      def foo(annotated: Annotated[Annotated[Annotated[int, "A"], "B"], "C"]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `int`."]
    context;
  ()


let () = "annotated" >::: ["annotated" >:: test_annotated] |> Test.run
