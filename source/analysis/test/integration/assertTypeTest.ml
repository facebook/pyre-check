(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

(* These test cases are adapted from the conformance test at
   https://github.com/python/typing/blob/62ec871cad97b241a4c5e5cbc483eaaf87f274f6/conformance/tests/directives_assert_type.py#L6C1-L7C1 *)
let test_assert_type_from_conformance =
  let assert_type_errors =
    assert_type_errors ~python_version:(Configuration.PythonVersion.create ~major:3 ~minor:11 ())
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Annotated, Any, Literal, assert_type


              def f(
                  a: int | str,
                  b: list[int],
                  c: Any,  # type: ignore
                  e: Annotated[Literal[4], ""],
              ) -> None:
                  assert_type(a, int | str)  # OK
                  assert_type(b, list[int])  # OK
                  assert_type(c, Any)  # OK
                  assert_type(e, Literal[4])  # OK
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Annotated, Any, Literal, assert_type


              def f(
                  d: "ForwardReference",
              ) -> None:
                  assert_type(d, "ForwardReference")  # OK

              class ForwardReference:
                  pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Annotated, Any, Literal, assert_type


              def f(
                  a: int | str,
                  c: Any,  # type: ignore
                  e: Annotated[Literal[4], ""],
              ) -> None:
                  assert_type(a, int)  # Error: Type mismatch
                  assert_type(c, int)  # Error: Type mismatch
                  assert_type(e, int)  # Error: Type mismatch
            |}
           [
             "Incompatible parameter type [6]: In call `assert_type`, for 1st positional argument, \
              expected `int` but got `Union[int, str]`.";
             "Incompatible parameter type [6]: In call `assert_type`, for 1st positional argument, \
              expected `int` but got `typing.Any`.";
             "Incompatible parameter type [6]: In call `assert_type`, for 1st positional argument, \
              expected `int` but got `typing_extensions.Literal[4]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Annotated, Any, Literal, assert_type


              def f(
                  a: int | str,
                  c: Any,  # type: ignore
                  e: Annotated[Literal[4], ""],
              ) -> None:
                  assert_type()  # Error: not enough arguments
                  assert_type("", int)  # Error: wrong argument type
                  assert_type(a, int | str, a)  # Error: too many arguments
            |}
           [
             "Missing argument [20]: Call `assert_type` expects argument in position 0.";
             "Incompatible parameter type [6]: In call `assert_type`, for 1st positional argument, \
              expected `int` but got `str`.";
             "Too many arguments [19]: Call `assert_type` expects 2 positional arguments, 3 were \
              provided.";
           ];
    ]


let () = "assert" >::: [test_assert_type_from_conformance] |> Test.run
