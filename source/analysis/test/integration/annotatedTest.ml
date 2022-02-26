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
    ~context
    {|
      from builtins import expect_int
      import typing_extensions
      def foo(annotated: typing_extensions.Annotated[int]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `typing.Annotated[int]`."];

  assert_type_errors
    ~context
    {|
      from builtins import expect_int
      import typing
      def foo(annotated: typing.Annotated[int]) -> int:
        expect_int(annotated)
        reveal_type(annotated)
        return annotated
    |}
    ["Revealed type [-1]: Revealed type for `annotated` is `typing.Annotated[int]`."]


let () = "annotated" >::: ["annotated" >:: test_annotated] |> Test.run
