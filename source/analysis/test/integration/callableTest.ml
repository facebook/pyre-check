(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_callable_parameters =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(42, 42)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(a=42, b=42)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(b=42, a=42)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(42, b=42)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(42, *(42,))
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(*(42,), 42)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(*(42, 42), 42)
            |}
           ["Too many arguments [19]: Call `foo` expects 2 positional arguments, 3 were provided."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(42, **{"b": 42})
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(a=42, 42)
            |}
           ["Parsing failure [404]: positional argument follows keyword argument"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(**{"a": 42}, 42)
            |}
           ["Parsing failure [404]: positional argument follows keyword argument unpacking"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(**{"a": 42}, *(42,))
            |}
           ["Parsing failure [404]: iterable argument unpacking follows keyword argument unpacking"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, b:int) -> None:
                pass
              foo(a=42, a=42)
            |}
           ["Unexpected keyword [28]: Unexpected keyword argument `a` to call `foo`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, a:int) -> None:
                pass
            |}
           ["Duplicate parameter [65]: Duplicate parameter name `a`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a:int, a:int, b: int, b: int) -> None:
                pass
            |}
           [
             "Duplicate parameter [65]: Duplicate parameter name `a`.";
             "Duplicate parameter [65]: Duplicate parameter name `b`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Foo:
                  default_fn: Callable[[str], str] = lambda val: val
              Foo.default_fn("X")
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Foo:
                  default_fn: Callable[[str], str] = lambda val: val
              foo = Foo()
              foo.default_fn("X")
            |}
           [
             "Too many arguments [19]: PositionalOnly call expects 0 positional arguments, 1 was \
              provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass
              from typing import Callable
              @dataclass
              class Foo:
                  default_fn: Callable[[str], str] = lambda val: val
              Foo.default_fn("X")
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass
              from typing import Callable
              @dataclass
              class Foo:
                  default_fn: Callable[[str], str] = lambda val: val
              foo = Foo()
              foo.default_fn("X")
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass
              from typing import Callable
              @dataclass
              class Foo:
                  def method(cls) -> None:
                    pass
              foo = Foo()
              foo.method()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass
              from typing import Callable
              @dataclass
              class Foo:
                  def method(cls) -> None:
                    pass
              Foo.method()
            |}
           ["Missing argument [20]: Call `Foo.method` expects argument `cls`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Foo:
                  def method(cls) -> None:
                    pass
              foo = Foo()
              foo.method()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Foo:
                  def method(cls) -> None:
                    pass
              Foo.method()
            |}
           ["Missing argument [20]: Call `Foo.method` expects argument `cls`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass
              from typing import Callable
              @dataclass
              class Foo:
                  @classmethod
                  def method(cls) -> None:
                    pass
              foo = Foo()
              foo.method()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from dataclasses import dataclass
              from typing import Callable
              @dataclass
              class Foo:
                  @classmethod
                  def method(cls) -> None:
                    pass
              Foo.method()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Foo:
                  @classmethod
                  def method(cls) -> None:
                    pass
              foo = Foo()
              foo.method()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Foo:
                  @classmethod
                  def method(cls) -> None:
                    pass
              Foo.method()
            |}
           [];
    ]


let test_higher_order_callables =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(f: typing.Callable[[int], str]) -> str:
                return f(1)

              def callme(x: int) -> str:
                return ""

              foo(callme)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(f: typing.Callable[..., str]) -> str:
                return f(1)

              def callme(x: int) -> str:
                return ""

              foo(callme)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              def foo(f: typing.Callable[[int], T]) -> typing.Callable[[str], T]:
                def takes_str(s: str) -> T:
                  return f(int(s))
                return takes_str

              def callme(x: int) -> str:
                return ""

              reveal_type(foo(callme))
            |}
           [
             "Revealed type [-1]: Revealed type for `test.foo(test.callme)` is \
              `typing.Callable[[str], str]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              def foo(f: typing.Callable[..., T]) -> typing.Callable[..., T]:
                def takes_str(s: str) -> T:
                  return f(int(s))
                return takes_str

              def callme(x: int) -> str:
                return ""

              reveal_type(foo(callme))
            |}
           [
             "Revealed type [-1]: Revealed type for `test.foo(test.callme)` is \
              `typing.Callable[..., str]`.";
           ];
    ]


let test_union_of_callables =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def baz(x: typing.Union[typing.Callable[[int], typing.Any], typing.Callable[..., typing.Any]]) -> None:
                  reveal_type(x)
                  x(1)
            |}
           [
             "Missing parameter annotation [2]: Parameter `x` must have a type that does not \
              contain `Any`.";
             "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing.Callable[[int], \
              typing.Any], typing.Callable[..., typing.Any]]`.";
           ];
    ]


let test_callable_attribute_access =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> int:
                return 0
              def bar() -> None:
                foo.attr
            |}
           ["Undefined attribute [16]: Callable `foo` has no attribute `attr`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                anon = lambda x: 0
                anon.attr
            |}
           ["Undefined attribute [16]: Anonymous callable has no attribute `attr`."];
      (* We filter errors related to patching. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from unittest.mock import Mock
              from unittest import TestCase

              class C:
                def foo(self) -> int: ...
              class Test(TestCase):
                def test_foo(self) -> None:
                  c = C()
                  c.foo = Mock()
                  c.foo.assert_not_called()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from unittest import TestCase
              def patch(item): ...
              class C:
                def foo(self) -> int: ...
              class Test(TestCase):
                def test_foo(self) -> None:
                  x = patch("os.path.abspath")
                  x.assert_not_called()
                  x.assert_called_once()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                pass
              def bar() -> None:
                a = foo.__ne__
                b = foo.__module__
                c = foo.__str__
                reveal_type(a)
                reveal_type(b)
                reveal_type(c)
            |}
           [
             "Revealed type [-1]: Revealed type for `a` is \
              `BoundMethod[typing.Callable(object.__ne__)[[Named(self, object), Named(o, object)], \
              bool], typing.Callable(foo)[[], None]]`.";
             "Revealed type [-1]: Revealed type for `b` is `str`.";
             "Revealed type [-1]: Revealed type for `c` is \
              `BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], \
              typing.Callable(foo)[[], None]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                pass
              def bar() -> None:
                foo.nonsense = 1
            |}
           ["Undefined attribute [16]: Callable `foo` has no attribute `nonsense`."];
      (* `__qualname__` is supported for all functions, including lambdas, as per PEP 3155. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
             def foo() -> None:
                 pass
             def bar() -> str:
                 return foo.__qualname__
             |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
             def foo() -> str:
                 f = lambda x: 0
                 return f.__qualname__
             |}
           [];
      (* Pyre unsoundly assumes that all callables have qualnames, even though objects with `__call__`
       * don't necessarily have one. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
             from typing import Callable
             class C:
                 def __call__(self) -> int:
                     return 0
             def foo(f: Callable[[], int]) -> str:
                 return f.__qualname__

             # This fails in the runtime, but type checks.
             foo(C())
             |}
           [];
    ]


let test_position_only_parameters =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(a: int, b: int, /) -> None:
                pass
              foo(1, 2)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            def foo(a: int, b: int, /, c: str) -> None:
                pass

            foo(1, 2, c="a")
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            def foo(a: int, b: int, /, c: str, *, d: int) -> None:
                pass

            foo(1, 2, "a", 1)
            |}
           ["Too many arguments [19]: Call `foo` expects 3 positional arguments, 4 were provided."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            def foo(a: int, b: int, /, c: str, *, d: int) -> None:
                pass

            foo(1, 2, "a", d=1)
            |}
           [];
    ]


let test_bound_method =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              def foo(bm: BoundMethod[Callable[[int, str], bool], int]) -> None:
                  c = bm.__call__
                  reveal_type(c)
            |}
           ["Revealed type [-1]: Revealed type for `c` is `typing.Callable[[str], bool]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              # This type is invalid, but we strip the first argument anyway
              def foo(bm: BoundMethod[Callable[[int, str], bool], str]) -> None:
                  c = bm.__call__
                  reveal_type(c)
            |}
           ["Revealed type [-1]: Revealed type for `c` is `typing.Callable[[str], bool]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Bar:
                # pyre-ignore[15]: inconsistent with type.__call__
                # pyre-ignore[13]: __call__ not initialized
                __call__: BoundMethod[Callable[[Bar, str], bool], Bar]
              def foo(bar: Bar) -> None:
                  c = bar("A")
                  reveal_type(c)
                  bar(1)
            |}
           [
             "Revealed type [-1]: Revealed type for `c` is `bool`.";
             "Incompatible parameter type [6]: In anonymous call, for 1st positional argument, \
              expected `str` but got `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class Bar:
                # pyre-ignore[15]: inconsistent with object.__init__
                __init__: BoundMethod[Callable[[Bar, str], None], Bar]
              def foo() -> None:
                  c = Bar("A")
                  reveal_type(c)
                  Bar(1)
            |}
           [
             "Revealed type [-1]: Revealed type for `c` is `Bar`.";
             "Incompatible parameter type [6]: In anonymous call, for 1st positional argument, \
              expected `str` but got `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import *
              def foo(x: int, y:str) -> None:
                pass

              def bar(b: bool, s: str) -> None:
                # pyre-ignore[10]: ensure mismatch errors appear after undefined arguments
                foo(unknown, b)
            |}
           [
             "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, \
              expected `int` but got `unknown`.";
             "Incompatible parameter type [6]: In call `foo`, for 2nd positional argument, \
              expected `str` but got `bool`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import *
              from pyre_extensions import Unpack, TypeVarTuple

              Ts = TypeVarTuple("Ts")

              def foo(x: Tuple[Unpack[Ts]], y: Tuple[Unpack[Ts]], z: Tuple[Unpack[Ts]]) -> None:
                pass

              def bar() -> None:
                x = (1,2,3)
                y = (4, 5)
                z = ("hello")
                foo(x, y, z)
            |}
           [
             "Incompatible parameter type [6]: In call `foo`, for 2nd positional argument, \
              expected `typing.Tuple[*test.Ts]` but got `Tuple[int, int]`.";
             "Incompatible parameter type [6]: In call `foo`, for 3rd positional argument, \
              expected `typing.Tuple[*test.Ts]` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import *
              from typing_extensions import Unpack, TypeVarTuple

              Ts = TypeVarTuple("Ts")

              def foo(x: Tuple[Unpack[Ts]], y: Tuple[Unpack[Ts]], z: Tuple[Unpack[Ts]]) -> None:
                pass

              def bar() -> None:
                x = (1,2,3)
                y = (4, 5)
                z = ("hello")
                foo(x, y, z)
            |}
           [
             "Incompatible parameter type [6]: In call `foo`, for 2nd positional argument, \
              expected `typing.Tuple[*test.Ts]` but got `Tuple[int, int]`.";
             "Incompatible parameter type [6]: In call `foo`, for 3rd positional argument, \
              expected `typing.Tuple[*test.Ts]` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import *

              Ts = TypeVarTuple("Ts")

              def foo(x: Tuple[Unpack[Ts]], y: Tuple[Unpack[Ts]], z: Tuple[Unpack[Ts]]) -> None:
                pass

              def bar() -> None:
                x = (1,2,3)
                y = (4, 5)
                z = ("hello")
                foo(x, y, z)
            |}
           [
             "Incompatible parameter type [6]: In call `foo`, for 2nd positional argument, \
              expected `typing.Tuple[*test.Ts]` but got `Tuple[int, int]`.";
             "Incompatible parameter type [6]: In call `foo`, for 3rd positional argument, \
              expected `typing.Tuple[*test.Ts]` but got `str`.";
           ];
    ]


let test_reexported_callable =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~other_sources:
             [
               {
                 handle = "reexports_callable.pyi";
                 source =
                   {|
                from typing import Callable as Callable

              |};
               };
             ]
           {|
              from typing import Any, TypeVar
              from reexports_callable import Callable

              _FuncT = TypeVar("_FuncT", bound=Callable[..., Any])

              def identity_for_function(funcobj: _FuncT) -> _FuncT: ...

              class Foo:
                @identity_for_function
                def my_method(self) -> int: ...

              def main() -> None:
                f1: Callable[[int], str]
                reveal_type(f1)
                reveal_type(Foo.my_method)
            |}
           [
             "Prohibited any [33]: Type variable `_FuncT` cannot have a bound containing `Any`.";
             "Revealed type [-1]: Revealed type for `f1` is `typing.Callable[[int], str]`.";
             "Revealed type [-1]: Revealed type for `test.Foo.my_method` is \
              `typing.Callable(Foo.my_method)[[Named(self, Foo)], int]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~other_sources:
             [
               {
                 handle = "reexports_callable.pyi";
                 source =
                   {|
                from typing import Callable as Callable, List as List

              |};
               };
             ]
           {|
              from typing import Any, TypeVar
              import typing
              from reexports_callable import Callable, List

              MyCallable = typing.Callable
              F = MyCallable[..., Any]
              G = MyCallable[[int], Any]

              f1: F
              f2: G
            |}
           [
             "Invalid type parameters [24]: Generic type `typing.Callable` expects 2 type \
              parameters.";
             "Prohibited any [33]: `F` cannot alias to a type containing `Any`.";
             "Prohibited any [33]: `G` cannot alias to a type containing `Any`.";
           ];
    ]


let test_callable_subtyping =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import Protocol

class Dest(Protocol):
  def __call__(self, x: int, y: int) -> None: ...

class Src(Protocol):
  def __call__(self, **kwargs: int) -> None: ...

src: Src = ...
dest: Dest = ...

dest = src
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import Protocol

class Dest(Protocol):
  def __call__(self, x: int, y: int, **kwargs: int) -> None: ...

class Src(Protocol):
  def __call__(self, **kwargs: int) -> None: ...

src: Src = ...
dest: Dest = ...

dest = src
            |}
           [];
    ]


let () =
  "callable"
  >::: [
         test_higher_order_callables;
         test_union_of_callables;
         test_callable_attribute_access;
         test_position_only_parameters;
         test_bound_method;
         test_reexported_callable;
         test_callable_parameters;
         test_callable_subtyping;
       ]
  |> Test.run
