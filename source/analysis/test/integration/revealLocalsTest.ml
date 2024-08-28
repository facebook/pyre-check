(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_reveal_locals =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def f(a: int, b: int) -> int:
        c = a + b
        reveal_locals()
        return c
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    c: `int`
    a: `int`
    b: `int`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Optional
      c: Optional[int] = None
      def f(a: int, b: int) -> int:
        global c
        c = a + b
        reveal_locals()
        return c
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    c: `Optional[int]` (inferred: `int`)
    a: `int`
    b: `int`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo(x: str) -> None:
        reveal_locals()
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    x: `str`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(x: typing.Union[int, str]) -> None:
        x = 1
        reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    x: `typing.Union[int, str]` (inferred: `typing_extensions.Literal[1]`)|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
      def foo(x) -> None:
        reveal_locals()
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    x: `typing.Any`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from builtins import int_to_str
      def foo(x: int) -> None:
        y = int_to_str(x)
        reveal_locals()
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    y: `str`
    x: `int`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        bar, baz = list(range(2))
        reveal_locals()
        return bar
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    bar: `int`
    baz: `int`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(s: typing.Sequence[float]) -> list[float]:
        l = list(s)
        bar, baz = l
        reveal_locals()
        return l
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `float`
    baz: `float`
    l: `typing.List[float]`
    s: `typing.Sequence[float]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> dict[str, int]:
        d = dict(a = 1, b = 2)
        bar = d['a']
        reveal_locals()
        return d
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `int`
    d: `typing.Dict[str, int]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(map: typing.Mapping[str, int]) -> dict[str, int]:
        d = dict(map)
        bar = d['a']
        reveal_locals()
        return d
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `int`
    d: `typing.Dict[str, int]`
    map: `typing.Mapping[str, int]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(t: typing.Iterable[typing.Tuple[str, int]]) -> dict[str, int]:
        d = dict(t)
        bar = d['a']
        reveal_locals()
        return d
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `int`
    d: `typing.Dict[str, int]`
    t: `typing.Iterable[typing.Tuple[str, int]]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(bar: typing.Union[int, str]) -> None:
        if type(bar) is int:
          reveal_locals()
        else:
          reveal_locals()
        reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `int`|};
             {|Revealed locals [-2]: Revealed local types are:
    bar: `str`|};
             {|Revealed locals [-2]: Revealed local types are:
    bar: `typing.Union[int, str]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
       x = 1.0
       def foo() -> None:
         global x
         x = 1
         reveal_locals()
     |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    x: `float` (inferred: `typing_extensions.Literal[1]`)|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
       import typing
       class Foo:
         attribute: typing.Optional[int] = 1
       def foo() -> None:
         y = Foo.attribute
         reveal_locals()
     |}
           [{|Revealed locals [-2]: Revealed local types are:
    y: `typing.Optional[int]`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
       import typing
       class Foo:
         attribute: typing.Optional[int] = 1
       def foo() -> None:
         Foo.attribute = 1
         reveal_locals()
     |}
           [{|Revealed locals [-2]: Revealed local types are:
    test.Foo: `typing.Type[Foo]`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      class A:
        def foo(self) -> None:
          reveal_locals()
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    self: `A`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      class A:
        def foo(self) -> None:
          def bar() -> None:
            pass
          reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `typing.Callable(A.foo.bar)[[], None]`
    self: `A`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypeVar, Generic
      T = TypeVar("T")
      class A(Generic[T]):
        def foo(self) -> None:
          def bar() -> None:
            pass
          reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `typing.Callable(A.foo.bar)[[], None]`
    self: `A[Variable[T]]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      class A:
        @classmethod
        def foo(cls) -> None:
          reveal_locals()
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    cls: `typing.Type[A]`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> None:
        def baz() -> None:
          pass
        def bar(x: int) -> int:
          return x
        reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    bar: `typing.Callable(foo.bar)[[Named(x, int)], int]`
    baz: `typing.Callable(foo.baz)[[], None]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo( *args: str, **kwargs: int) -> None:
        def f() -> None:
          x = args[0]
          y = kwargs['key']
          reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    f: `typing.Callable(foo.f)[[], None]`
    x: `str`
    y: `int`
    args: `typing.Tuple[str, ...]`
    kwargs: `typing.Dict[str, int]`|};
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import builtins
      class MyInt:
        pass
      int = MyInt
      def f(x:int, y:builtins.int) -> None:
        reveal_locals()
    |}
           [{|Revealed locals [-2]: Revealed local types are:
    x: `MyInt`
    y: `int`|}];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Any
      class type:
        def __init__(self, __name: str, __bases: tuple[type, ...], __dict: dict[str, Any], **kwds: Any) -> None:
          reveal_locals()
    |}
           [
             {|Revealed locals [-2]: Revealed local types are:
    __bases: `typing.Tuple[type, ...]`
    __dict: `typing.Dict[str, typing.Any]`
    __name: `str`
    kwds: `typing.Dict[str, typing.Any]`
    self: `type`|};
           ];
    ]


let () = "revealLocals" >::: [test_reveal_locals] |> Test.run
