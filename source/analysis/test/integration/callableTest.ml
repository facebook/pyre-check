(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_higher_order_callables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def foo(f: typing.Callable[[int], str]) -> str:
        return f(1)

      def callme(x: int) -> str:
        return ""

      foo(callme)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(f: typing.Callable[..., str]) -> str:
        return f(1)

      def callme(x: int) -> str:
        return ""

      foo(callme)
    |}
    [];
  assert_type_errors
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
      "Revealed type [-1]: Revealed type for `test.foo(test.callme)` is `typing.Callable[[str], \
       str]`.";
    ];
  assert_type_errors
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
      "Revealed type [-1]: Revealed type for `test.foo(test.callme)` is `typing.Callable[..., str]`.";
    ]


let test_union_of_callables context =
  assert_type_errors
    ~context
    {|
      import typing
      def baz(x: typing.Union[typing.Callable[[int], typing.Any], typing.Callable[..., typing.Any]]) -> None:
          reveal_type(x)
          x(1)
    |}
    [
      "Missing parameter annotation [2]: Parameter `x` must have a type that does not contain `Any`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[typing.Callable[[int], \
       typing.Any], typing.Callable[..., typing.Any]]`.";
    ]


let test_callable_attribute_access context =
  assert_type_errors
    ~context
    {|
      def foo() -> int:
        return 0
      def bar() -> None:
        foo.attr
    |}
    ["Undefined attribute [16]: Callable `foo` has no attribute `attr`."];
  assert_type_errors
    ~context
    {|
      def foo() -> None:
        anon = lambda x: 0
        anon.attr
    |}
    ["Undefined attribute [16]: Anonymous callable has no attribute `attr`."];

  (* We filter errors related to patching. *)
  assert_default_type_errors
    ~context
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
  assert_default_type_errors
    ~context
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
  assert_type_errors
    ~context
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
       `BoundMethod[typing.Callable(object.__ne__)[[Named(self, object), Named(o, object)], bool], \
       typing.Callable(foo)[[], None]]`.";
      "Revealed type [-1]: Revealed type for `b` is `str`.";
      "Revealed type [-1]: Revealed type for `c` is \
       `BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], \
       typing.Callable(foo)[[], None]]`.";
    ];
  assert_type_errors
    ~context
    {|
      def foo() -> None:
        pass
      def bar() -> None:
        foo.nonsense = 1
    |}
    ["Undefined attribute [16]: Callable `foo` has no attribute `nonsense`."];
  ()


let test_position_only_parameters context =
  let assert_type_errors = assert_type_errors ~context in

  assert_type_errors
    {|
      def foo(a: int, b: int, /) -> None:
        pass
      foo(1, 2)
    |}
    [];
  assert_type_errors
    {|
    def foo(a: int, b: int, /, c: str) -> None:
        pass

    foo(1, 2, c="a")
    |}
    [];
  assert_type_errors
    {|
    def foo(a: int, b: int, /, c: str, *, d: int) -> None:
        pass

    foo(1, 2, "a", 1)
    |}
    ["Too many arguments [19]: Call `foo` expects 3 positional arguments, 4 were provided."];
  assert_type_errors
    {|
    def foo(a: int, b: int, /, c: str, *, d: int) -> None:
        pass

    foo(1, 2, "a", d=1)
    |}
    []


let test_bound_method context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Callable
      def foo(bm: BoundMethod[Callable[[int, str], bool], int]) -> None:
          c = bm.__call__
          reveal_type(c)
    |}
    ["Revealed type [-1]: Revealed type for `c` is `typing.Callable[[str], bool]`."];
  assert_type_errors
    {|
      from typing import Callable
      # This type is invalid, but we strip the first argument anyway
      def foo(bm: BoundMethod[Callable[[int, str], bool], str]) -> None:
          c = bm.__call__
          reveal_type(c)
    |}
    ["Revealed type [-1]: Revealed type for `c` is `typing.Callable[[str], bool]`."];
  assert_type_errors
    {|
      from typing import Callable
      # pyre-ignore[13]: __call__ not initialized
      class Bar:
        # pyre-ignore[15]: inconsistent with type.__call__
        __call__: BoundMethod[Callable[[Bar, str], bool], Bar]
      def foo(bar: Bar) -> None:
          c = bar("A")
          reveal_type(c)
          bar(1)
    |}
    [
      "Revealed type [-1]: Revealed type for `c` is `bool`.";
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `str` but got `int`.";
    ];
  assert_type_errors
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
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
      from typing import *
      def foo(x: int, y:str) -> None:
        pass

      def bar(b: bool, s: str) -> None:
        # pyre-ignore[10]: ensure mismatch errors appear after undefined arguments
        foo(unknown, b)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `unknown`.";
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `str` but got `bool`.";
    ];
  assert_type_errors
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
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `typing.Tuple[*test.Ts]` but got `Tuple[int, int]`.";
      "Incompatible parameter type [6]: In call `foo`, for 3rd positional only parameter expected \
       `typing.Tuple[*test.Ts]` but got `str`.";
    ];
  ()


let test_reexported_callable context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "reexports_callable.pyi";
          source = {|
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
      "Prohibited any [33]: `_FuncT` cannot alias to a type containing `Any`.";
      "Revealed type [-1]: Revealed type for `f1` is `typing.Callable[[int], str]`.";
      "Revealed type [-1]: Revealed type for `test.Foo.my_method` is \
       `typing.Callable(Foo.my_method)[[Named(self, Foo)], int]`.";
    ];
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "reexports_callable.pyi";
          source = {|
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
      "Invalid type parameters [24]: Generic type `typing.Callable` expects 2 type parameters.";
      "Prohibited any [33]: `F` cannot alias to a type containing `Any`.";
      "Prohibited any [33]: `G` cannot alias to a type containing `Any`.";
    ];
  ()


let () =
  "callable"
  >::: [
         "higher_order_callables" >:: test_higher_order_callables;
         "union_of_callables" >:: test_union_of_callables;
         "callable_attribute_access" >:: test_callable_attribute_access;
         "position_only_parameters" >:: test_position_only_parameters;
         "bound_method" >:: test_bound_method;
         "reexported_callable" >:: test_reexported_callable;
       ]
  |> Test.run
