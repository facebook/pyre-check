(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_higher_order_callables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(f: typing.Callable[[int], str]) -> str:
        return f(1)

      def callme(x: int) -> str:
        return ""

      foo(callme)
    |}
    [];
  assert_type_errors
    {|
      def foo(f: typing.Callable[..., str]) -> str:
        return f(1)

      def callme(x: int) -> str:
        return ""

      foo(callme)
    |}
    [];
  assert_type_errors
    {|
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
      "Revealed type [-1]: Revealed type for `a` is `typing.Callable(object.__ne__)[[Named(o, \
       object)], bool]`.";
      "Revealed type [-1]: Revealed type for `b` is `str`.";
      "Revealed type [-1]: Revealed type for `c` is `typing.Callable(object.__str__)[[], str]`.";
    ];
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
    def foo(a: int, b: int, /) -> None:
        pass

    foo(a=1, 2)
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `a` to call `foo`."];
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


let () =
  "callable"
  >::: [
         "higher_order_callables" >:: test_higher_order_callables;
         "union_of_callables" >:: test_union_of_callables;
         "callable_attribute_access" >:: test_callable_attribute_access;
         "position_only_parameters" >:: test_position_only_parameters;
       ]
  |> Test.run
