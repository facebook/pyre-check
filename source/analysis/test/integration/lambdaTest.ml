(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_map_lambda =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[int]) -> typing.Iterator[int]:
        return map(lambda x: str(x), l)
    |}
           ["Incompatible return type [7]: Expected `Iterator[int]` but got `map[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[int]) -> typing.Iterator[str]:
        return map(lambda x: str(x), l)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.List[int]:
        return map(lambda x: str(x), l)
    |}
           ["Incompatible return type [7]: Expected `List[int]` but got `map[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.List[str]:
        return map(lambda x: str(x), l)
    |}
           ["Incompatible return type [7]: Expected `List[str]` but got `map[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[int]) -> typing.Iterator[int]:
        return list(map(lambda x: str(x), l))
    |}
           ["Incompatible return type [7]: Expected `Iterator[int]` but got `List[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[int]) -> typing.Iterator[str]:
        return list(map(lambda x: str(x), l))
    |}
           ["Incompatible return type [7]: Expected `Iterator[str]` but got `List[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.List[int]:
        return list(map(lambda x: x, l))
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.List[str]:
        return list(map(lambda x: x, l))
    |}
           ["Incompatible return type [7]: Expected `List[str]` but got `List[int]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.List[str]:
        return list(map(lambda x: str(x) if isinstance(x, int) else None, l))
    |}
           ["Incompatible return type [7]: Expected `List[str]` but got `List[Optional[str]]`."];
    ]


let test_check_filter_lambda =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[int]) -> typing.Iterator[int]:
        return filter(lambda x: x * x == 10, l)
    |}
           [];
      (* False positive *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[str]) -> typing.Iterator[str]:
        return filter(lambda x: x * x == 10, l)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.Iterator[int]:
        return filter(lambda x: x * x == 10, l)
    |}
           [];
      (* False positive *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[str]) -> typing.Iterator[str]:
        return filter(lambda x: x * x == 10, l)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[int]) -> typing.List[int]:
        return filter(lambda x: x * x == 10, l)
    |}
           ["Incompatible return type [7]: Expected `List[int]` but got `filter[typing.Any]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.List[str]) -> typing.List[str]:
        return filter(lambda x: x * x == 10, l)
    |}
           ["Incompatible return type [7]: Expected `List[str]` but got `filter[typing.Any]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[str]) -> typing.Iterator[str]:
        return filter(lambda x: x[:4] == "http", l)
    |}
           [];
      (* False positive *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(l: typing.Iterator[int]) -> typing.Iterator[int]:
        return filter(lambda x: x[:4] == "http", l)
    |}
           [];
    ]


let test_check_reduce_lambda =
  test_list
    [
      (* False negative *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[int]) -> int:
        return functools.reduce(lambda x, y: x + y, l)
    |}
           [];
      (* False negative *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[int]) -> str:
        return functools.reduce(lambda x, y: x + y, l)
    |}
           [];
      (* False positive and false negative *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[str]) -> str:
        return functools.reduce(lambda x, y: x * y, l)
    |}
           [];
      (* False positive and false negative *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[str]) -> int:
        return functools.reduce(lambda x, y: x * y, l)
    |}
           [];
      (* False negative *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[int]) -> int:
        return functools.reduce(lambda x, y: x + y, l, 1)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[int]) -> str:
        return functools.reduce(lambda x, y: x + y, l, 1)
    |}
           ["Incompatible return type [7]: Expected `str` but got `int`."];
      (* False positive and false negative *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      import functools
      def foo(l: typing.Iterator[str]) -> str:
        return functools.reduce(lambda x, y: x * y, l, "a")
    |}
           [];
    ]


let test_check_apply_lambda =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      _T = typing.TypeVar("_T")
      _S = typing.TypeVar("_S")

      def apply(__fun : typing.Callable[[_T], _S], __x : _T) -> _S:
        return __fun(__x)

      def foo(y: int) -> int:
        return apply(lambda x: x, y)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      _T = typing.TypeVar("_T")
      _S = typing.TypeVar("_S")

      def apply(__fun : typing.Callable[[_T], _S], __x : _T) -> _S:
        return __fun(__x)

      def foo(y: int) -> str:
        return apply(lambda x: x, y)
    |}
           ["Incompatible return type [7]: Expected `str` but got `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      _T = typing.TypeVar("_T")
      _S = typing.TypeVar("_S")

      def apply(__fun : typing.Callable[[_T], _S], __x : _T) -> _S:
        return __fun(__x)

      def foo(y: int) -> str:
        return apply(lambda x: str(x), y)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      _T = typing.TypeVar("_T")
      _S = typing.TypeVar("_S")

      def apply(__fun : typing.Callable[[_T], _S], __x : _T) -> _S:
        return __fun(__x)

      def foo(y: int) -> int:
        return apply(lambda x: str(x), y)
    |}
           ["Incompatible return type [7]: Expected `int` but got `str`."];
    ]


let () =
  "lambda"
  >::: [
         test_check_map_lambda;
         test_check_filter_lambda;
         test_check_reduce_lambda;
         test_check_apply_lambda;
       ]
  |> Test.run
