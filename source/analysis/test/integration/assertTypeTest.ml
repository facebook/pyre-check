(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

(* `typing.assert_type` is only defined as of 3.11 *)
let assert_type_errors =
  assert_type_errors ~python_version:(Configuration.PythonVersion.create ~major:3 ~minor:11 ())


(* These test cases are adapted from the conformance test at
   https://github.com/python/typing/blob/62ec871cad97b241a4c5e5cbc483eaaf87f274f6/conformance/tests/directives_assert_type.py#L6C1-L7C1 *)
let test_assert_type_from_conformance =
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
             "Assert type [70]: Expected `int` but got `Union[int, str]`.";
             "Assert type [70]: Expected `int` but got `typing.Any`.";
             "Assert type [70]: Expected `int` but got `typing_extensions.Literal[4]`.";
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
             "Assert type [70]: Expected `int` but got `typing_extensions.Literal['']`.";
             "Too many arguments [19]: Call `assert_type` expects 2 positional arguments, 3 were \
              provided.";
           ];
    ]


(* Verifying behavior on a variety of examples that show up elsewhere in the conformance tests. *)
let test_assert_type_edge_cases =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import assert_type

              x: int | str = ...

              assert_type(x, int | str)
              assert_type(x, str | int)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import assert_type, Dict, Tuple

              def f(
                  *args: int,
                  **kwargs: int,
              ) -> None:
                  assert_type(args, tuple[int, ...])
                  assert_type(args, Tuple[int, ...])
                  assert_type(kwargs, dict[str, int])
                  assert_type(kwargs, Dict[str, int])
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Any, Callable, Coroutine, assert_type

              async def func1(ignored: int, /) -> str:
                  return "str"

              assert_type(func1, Callable[[int], Coroutine[Any, Any, str]])
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import assert_type, Callable

              class Foo:
                  @classmethod
                  def class_method(cls) -> None: ...

                  def normal_method(self) -> None: ...

              assert_type(Foo.class_method, Callable[[], None])
              assert_type(Foo().normal_method, Callable[[], None])
            |}
           [];
    ]


let () = "assert" >::: [test_assert_type_from_conformance; test_assert_type_edge_cases] |> Test.run
