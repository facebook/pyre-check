(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_reveal_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              reveal_type(12345678901234567890123)
            |}
           ["Revealed type [-1]: Revealed type for `12345678901234567890123` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: str) -> None:
                reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `str`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(x: typing.Union[int, str]) -> None:
                x = 1
                reveal_type(x)
            |}
           [
             "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]` (inferred: \
              `typing_extensions.Literal[1]`).";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x) -> None:
                reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int, y: int) -> None:
                reveal_type(x + y)
            |}
           ["Revealed type [-1]: Revealed type for `x.__add__(y)` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from builtins import int_to_str
              def foo(x: int) -> None:

                reveal_type(int_to_str(x))
            |}
           ["Revealed type [-1]: Revealed type for `int_to_str(x)` is `str`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> int:
                bar, baz = list(range(2))
                reveal_type(bar)
                return bar
            |}
           ["Revealed type [-1]: Revealed type for `bar` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(s: typing.Sequence[float]) -> list[float]:
                l = list(s)
                bar, baz = l
                reveal_type(bar)
                return l
            |}
           ["Revealed type [-1]: Revealed type for `bar` is `float`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> dict[str, int]:
                d = dict(a = 1, b = 2)
                reveal_type(d)
                bar = d['a']
                reveal_type(bar)
                return d
            |}
           [
             "Revealed type [-1]: Revealed type for `d` is `typing.Dict[str, int]`.";
             "Revealed type [-1]: Revealed type for `bar` is `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(map: typing.Mapping[str, int]) -> dict[str, int]:
                d = dict(map)
                bar = d['a']
                reveal_type(bar)
                return d
            |}
           ["Revealed type [-1]: Revealed type for `bar` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(t: typing.Iterable[typing.Tuple[str, int]]) -> dict[str, int]:
                d = dict(t)
                bar = d['a']
                reveal_type(bar)
                return d
            |}
           ["Revealed type [-1]: Revealed type for `bar` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(bar: typing.Union[int, str]) -> None:
                if type(bar) is int:
                  reveal_type(bar)
                else:
                  reveal_type(bar)
            |}
           [
             "Revealed type [-1]: Revealed type for `bar` is `int`.";
             "Revealed type [-1]: Revealed type for `bar` is `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              reveal_type(typing.List[int])
            |}
           [
             "Revealed type [-1]: Revealed type for `typing.List[int]` is \
              `typing.Type[typing.List[int]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
               x = 1.0
               def foo() -> None:
                 global x
                 x = 1
                 reveal_type(x)
             |}
           [
             "Revealed type [-1]: Revealed type for `x` is `float` (inferred: \
              `typing_extensions.Literal[1]`).";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
               import typing
               class Foo:
                 attribute: typing.Optional[int] = 1
               def foo() -> None:
                 reveal_type(Foo.attribute)
             |}
           ["Revealed type [-1]: Revealed type for `test.Foo.attribute` is `typing.Optional[int]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
               import typing
               class Foo:
                 attribute: typing.Optional[int] = 1
               def foo() -> None:
                 Foo.attribute = 1
                 reveal_type(Foo.attribute)
             |}
           [
             "Revealed type [-1]: Revealed type for `test.Foo.attribute` is `typing.Optional[int]` \
              (inferred: `typing_extensions.Literal[1]`).";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class A:
                def foo(self) -> None:
                  reveal_type(self)
            |}
           ["Revealed type [-1]: Revealed type for `self` is `A`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class A:
                def foo(self) -> None:
                  def bar() -> None:
                    reveal_type(self)
            |}
           ["Revealed type [-1]: Revealed type for `self` is `A`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeVar, Generic
              T = TypeVar("T")
              class A(Generic[T]):
                def foo(self) -> None:
                  def bar() -> None:
                    reveal_type(self)
            |}
           ["Revealed type [-1]: Revealed type for `self` is `A[Variable[T]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class A:
                @classmethod
                def foo(cls) -> None:
                  reveal_type(cls)
            |}
           ["Revealed type [-1]: Revealed type for `cls` is `typing.Type[A]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class A:
                @classmethod
                def foo(cls) -> None:
                  def bar() -> None:
                    reveal_type(cls)
            |}
           ["Revealed type [-1]: Revealed type for `cls` is `typing.Type[A]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                def baz() -> None:
                  reveal_type(bar)
                def bar(x: int) -> int:
                  return x
            |}
           [
             "Revealed type [-1]: Revealed type for `bar` is \
              `typing.Callable($local_test?foo$bar)[[Named(x, int)], int]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo( *args: str, **kwargs: int) -> None:
                def f() -> None:
                  reveal_type(args[0])
                  reveal_type(kwargs['key'])
            |}
           [
             "Revealed type [-1]: Revealed type for `args[0]` is `str`.";
             "Revealed type [-1]: Revealed type for `kwargs[\"key\"]` is `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import builtins
              def foo(x: builtins.int) -> int:
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import builtins
              def foo(x: builtins.int) -> str:
                return builtins.str(x)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import builtins
              class MyInt:
                pass
              int = MyInt
              def f(x:int, y:builtins.int) -> None:
                reveal_type(x)
                reveal_type(y)
            |}
           [
             "Revealed type [-1]: Revealed type for `x` is `MyInt`.";
             "Revealed type [-1]: Revealed type for `y` is `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
             from typing import Never

             x: Never

             reveal_type(x)
           |}
           ["Revealed type [-1]: Revealed type for `x` is `Never`."];
    ]


let test_typing_reveal_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~python_version:(Configuration.PythonVersion.create ~major:3 ~minor:11 ~micro:0 ())
           {|
             from typing import reveal_type

             reveal_type("s")
           |}
           ["Revealed type [-1]: Revealed type for `\"s\"` is `typing_extensions.Literal['s']`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~python_version:(Configuration.PythonVersion.create ~major:3 ~minor:11 ~micro:0 ())
           {|
             from typing import reveal_type

             reveal_type(reveal_type("s"))
           |}
           [
             "Revealed type [-1]: Revealed type for `typing.reveal_type(\"s\")` is \
              `typing_extensions.Literal['s']`.";
             "Revealed type [-1]: Revealed type for `\"s\"` is `typing_extensions.Literal['s']`.";
           ];
    ]


let () = "revealType" >::: [test_reveal_type; test_typing_reveal_type] |> Test.run
