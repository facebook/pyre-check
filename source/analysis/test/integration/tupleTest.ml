(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_tuple =
  let assert_type_errors source errors context = assert_type_errors source errors context in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from builtins import return_tuple
      def derp()->int:
          a, b = return_tuple()
          return a+b
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def f(l: typing.List[int]) -> int:
        [a, b] = l
        return a + b
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(a: typing.Tuple[int, int]) -> None:
        a.tuple_method(1.0)
    |}
           [
             "Incompatible parameter type [6]: In call `tuple.tuple_method`, for 1st positional \
              argument, expected `int` but got `float`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return (1, 2, 3)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, str]:
        return (1, "string", 3)
    |}
           [
             "Incompatible return type [7]: Expected `Tuple[int, str]` but got `Tuple[int, str, \
              int]`. Expected has length 2, but actual has length 3.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
           [
             "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got `Tuple[int, \
              str, int]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo()-> typing.Tuple[int, ...]:
        return tuple([1,2,3])
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return tuple([""])
    |}
           [
             "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got "
             ^ "`typing.Tuple[str, ...]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return tuple([1])
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return (1, 2)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2.0)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(x: typing.Tuple[int, int, str]) -> typing.Tuple[str, int]:
        a, *b = x
        return b
    |}
           [
             "Incompatible return type [7]: Expected `Tuple[str, int]` but got `List[Union[int, \
              str]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo(x: typing.Tuple[int, str]) -> None:
        pass

      foo((1, 2))
    |}
           [
             "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, \
              expected `Tuple[int, str]` but got `Tuple[int, int]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def derp() -> int:
        a, b = [1,2,3]
        return a + b
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        (x, y), z = 0
        return x + y + z
    |}
           ["Unable to unpack [23]: Unable to unpack `int` into 2 values."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      class Foo:
        def __init__(self, coord: typing.Tuple[int, int]) -> None:
            self.xxx, self.yyy = coord
    |}
           [
             "Missing attribute annotation [4]: Attribute `xxx` of class `Foo` "
             ^ "has type `int` but no type is specified.";
             "Missing attribute annotation [4]: Attribute `yyy` of class `Foo` "
             ^ "has type `int` but no type is specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Sized:
        return (1,)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Sized:
        return (1, "")
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple:
        return ()
    |}
           ["Invalid type parameters [24]: Generic type `tuple` expects at least 1 type parameter."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[()]:
        return ()
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, str]:
        return ()
    |}
           [
             "Incompatible return type [7]: Expected `Tuple[int, str]` but got `Tuple[]`. Expected \
              has length 2, but actual has length 0.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return ()
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Iterable[int]:
        return ()
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from builtins import int_to_int
      import typing
      def bar(z: typing.Optional[int]) -> typing.Tuple[int, typing.Optional[int]]:
          return 1, int_to_int(z) if z is not None else None
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo( *args: int) -> typing.Iterable[str]:
        return args
    |}
           [
             "Incompatible return type [7]: Expected `Iterable[str]` but got `typing.Tuple[int, \
              ...]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import collections
      T = collections.namedtuple('T', 'a b c')
      def b(d: T) -> None:
        a = d.a + d.d
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `b` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `c` of class `T` must have a type other \
              than `Any`.";
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `c` must have a type other than `Any`.";
             "Undefined attribute [16]: `T` has no attribute `d`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import collections
      import typing
      class C(collections.namedtuple('T', 'a b')):
        def __new__(cls, a: int) -> typing.Type[C]:
          ...
      C(1,2)
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `C` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `b` of class `C` must have a type other \
              than `Any`.";
             "Too many arguments [19]: Call `C.__new__` expects 1 positional argument, 2 were \
              provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import collections
      T = collections.namedtuple('T', 'a b c')
      def foo(t: T) -> None:
        x, y = t
        x, y, z = t
        x, y, z, other = t
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `b` of class `T` must have a type other \
              than `Any`.";
             "Missing attribute annotation [4]: Attribute `c` of class `T` must have a type other \
              than `Any`.";
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `c` must have a type other than `Any`.";
             "Unable to unpack [23]: Unable to unpack 3 values, 2 were expected.";
             "Unable to unpack [23]: Unable to unpack 3 values, 4 were expected.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import collections
      T = collections.namedtuple('T', 'a')
      T(a=1)
      def foo() -> None:
        T(a=2)
    |}
           [
             "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other \
              than `Any`.";
             "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      T = typing.NamedTuple('T', [('a', str), ('b', int)])
      def takes_int(x: int) -> None: pass
      def foo(x: T) -> None:
          takes_int(x.b)
          a, b = x
          reveal_type(a)
          reveal_type(b)
    |}
           [
             "Revealed type [-1]: Revealed type for `a` is `str`.";
             "Revealed type [-1]: Revealed type for `b` is `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      class TestNamedTupleUnpackingFieldsNotInAlphabeticalOrder(typing.NamedTuple):
        foo: str
        bar: int
        baz: typing.List[str]
        hello: typing.List[int]

      def foo() -> None:
        (a, b, c, d) = TestNamedTupleUnpackingFieldsNotInAlphabeticalOrder("foo", 1, ["bar"], [7])
        reveal_type(a)
        reveal_type(b)
        reveal_type(c)
        reveal_type(d)
    |}
           [
             "Revealed type [-1]: Revealed type for `a` is `str`.";
             "Revealed type [-1]: Revealed type for `b` is `int`.";
             "Revealed type [-1]: Revealed type for `c` is `typing.List[str]`.";
             "Revealed type [-1]: Revealed type for `d` is `typing.List[int]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo(input: int) -> None:
        x, y = input
    |}
           ["Unable to unpack [23]: Unable to unpack `int` into 2 values."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors {|
      def foo() -> bool:
        return (52,) < (1, 2, 3)
    |} [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      class FooNotNamedTuple:
        bar: typing.Optional[str] = None
        baz: typing.Dict[int, typing.Any] = {}
        hello: typing.Dict[str, typing.Any] = {}
    |}
           [
             "Missing attribute annotation [4]: Attribute `baz` of class `FooNotNamedTuple` must \
              have a type that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           (* The parameter `baz` in the __new__ method for NamedTuple will not throw a duplicate
              error for Any. *)
           {|
      import typing
      class Foo(typing.NamedTuple):
        bar: typing.Optional[str] = None
        baz: typing.Dict[int, typing.Any] = {}
        hello: typing.Dict[str, typing.Any] = {}
    |}
           [
             "Missing parameter annotation [2]: Parameter `baz` must have a type that does not \
              contain `Any`.";
             "Missing attribute annotation [4]: Attribute `baz` of class `Foo` must have a type \
              that does not contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           (* A __new__ method for a non-NamedTuple will throw error on Any. *)
           {|
      import typing
      class Foo:
        def __new__(cls, foo: typing.Dict[int, typing.Any] = {}) -> Foo:
            return super(Foo, cls).__new__(cls)
    |}
           [
             "Missing parameter annotation [2]: Parameter `foo` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           (* If __new__ is not a method of some class, it will throw the Any error. *)
           {|
      import typing
      def __new__(foo: typing.Dict[int, typing.Any] = {}) -> None:
        pass
    |}
           [
             "Missing parameter annotation [2]: Parameter `foo` must have a type that does not \
              contain `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      X = typing.NamedTuple(
        "x",
        dates=str
      )
      X(dates="foo")
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple, Union
      def foo() -> None:
        union_of_bounded_tuples: Union[Tuple[int, str], Tuple[bool, List[int]]]
        a, b = union_of_bounded_tuples
        reveal_type(a)
        reveal_type(b)
    |}
           [
             "Revealed type [-1]: Revealed type for `a` is `Union[bool, int]`.";
             "Revealed type [-1]: Revealed type for `b` is `Union[List[int], str]`.";
           ];
      (* T54851036 *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypeVar, NamedTuple, Generic

      State = TypeVar("State")

      class LoopState(Generic[State], NamedTuple):
        blah: int
        state: State
    |}
           [
             "Inconsistent method resolution order [64]: Class `LoopState` does not have a \
              consistent method resolution order";
             "Undefined attribute [16]: `typing.Type` has no attribute `_fields`.";
             "Undefined attribute [16]: `LoopState` has no attribute `blah`.";
             "Undefined attribute [16]: `LoopState` has no attribute `state`.";
             "Undefined attribute [16]: `typing.Type` has no attribute `blah`.";
             "Undefined attribute [16]: `typing.Type` has no attribute `state`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypeVar, NamedTuple, Generic

      State = TypeVar("State")

      class LoopState(NamedTuple, Generic[State]):
        blah: int
        state: State
    |}
           [];
    ]


let test_tuple_literal_access =
  let assert_type_errors source errors context = assert_type_errors source errors context in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[0]
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[1]
    |}
           ["Incompatible return type [7]: Expected `int` but got `str`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo(p: int) -> int:
        x = (0, "one", 2)
        return x[p]
    |}
           ["Incompatible return type [7]: Expected `int` but got `Union[int, str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[-1]
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[-2]
    |}
           ["Incompatible return type [7]: Expected `int` but got `str`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        i = 0
        x = (0, "one", 2)
        return x[i]
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        i = -2
        x = (0, "one", 2.)
        return x[-3]
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[3]
    |}
           [
             "Incompatible return type [7]: Expected `int` but got `Union[int, str]`.";
             "Invalid tuple index [73]: Index 3 is out of bounds for concrete tuple with 3 members.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      import typing
      def foo() -> typing.Tuple[int, int]:
        x = (0, 1, "two")
        return x[0:2]
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def func(a: int, b: str, c: bool) -> None:
        pass
      x = (42, "bla", False)
      func( *x)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def func(a: int, b: str, c: bool) -> None:
        pass
      c = ("bla", False)
      func(1, *c)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def func(a: int, b: bool, c: str) -> None:
        pass
      c = ("bla", False)
      func(1, *c)
    |}
           [
             "Incompatible parameter type [6]: In call `func`, for 2nd positional argument, \
              expected `bool` but got `str`.";
             "Incompatible parameter type [6]: In call `func`, for 3rd positional argument, \
              expected `str` but got `bool`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def func(a: int, b: str, c: bool) -> None:
        pass
      c = ("bla", )
      func(1, *c)
    |}
           ["Missing argument [20]: Call `func` expects argument `c`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def func(a: int, b: bool, c: str, d:int, e:str) -> None:
        pass
      c = (False, "ble")
      d = (1, "abc")
      func(1, *c, *d)
    |}
           [];
    ]


let test_custom_tuple =
  let assert_type_errors source errors context = assert_type_errors source errors context in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      class C:
        def __getitem__(self, index: int) -> int:
          self.counter += 1
          return self.counter

        def __init__(self) -> None:
          self.counter = 0

      def foo() -> None:
        x, y, z = C()
        reveal_type(x)
        reveal_type(y)
        reveal_type(z)
    |}
           [
             "Revealed type [-1]: Revealed type for `x` is `int`.";
             "Revealed type [-1]: Revealed type for `y` is `int`.";
             "Revealed type [-1]: Revealed type for `z` is `int`.";
           ];
      (* We allow specifying Any for custom getitems. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Any
      class C:
        def __getitem__(self, index: int) -> Any:
          self.counter += 1
          return self.counter

        def __init__(self) -> None:
          self.counter = 0

      def foo() -> None:
        x, y, z = C()
        reveal_type(x)
        reveal_type(y)
        reveal_type(z)
    |}
           [
             "Missing return annotation [3]: Returning `int` but type `Any` is specified.";
             "Unable to unpack [23]: Unable to unpack `test.C` into 3 values.";
             "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
             "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
             "Revealed type [-1]: Revealed type for `z` is `typing.Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo() -> None:
        x, y, z = object()
        reveal_type(x)
        reveal_type(y)
        reveal_type(z)
    |}
           [
             "Unable to unpack [23]: Unable to unpack `object` into 3 values.";
             "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
             "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
             "Revealed type [-1]: Revealed type for `z` is `typing.Any`.";
           ];
    ]


let test_length =
  let assert_type_errors source errors context = assert_type_errors source errors context in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple

      def foo() -> None:
        xs: Tuple[()]
        y = len(xs)
        reveal_type(y)
    |}
           ["Revealed type [-1]: Revealed type for `y` is `int`."];
    ]


let test_unpacking =
  let assert_type_errors source errors context = assert_type_errors source errors context in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple

      def foo(x: int, xs: Tuple[str, bool]) -> None:

        y = (x, *xs)
        reveal_type(y)
    |}
           ["Revealed type [-1]: Revealed type for `y` is `Tuple[int, str, bool]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple

      def foo(x: int, xs: str) -> None:

        y = (x, *xs)
        reveal_type(y)
    |}
           ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[str, ...]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple

      def foo() -> None:

        y = (1, *(2, 3), *(4, 5, 6), 7)
        reveal_type(y)
    |}
           [
             "Revealed type [-1]: Revealed type for `y` is `Tuple[typing_extensions.Literal[1], \
              typing_extensions.Literal[2], typing_extensions.Literal[3], \
              typing_extensions.Literal[4], typing_extensions.Literal[5], \
              typing_extensions.Literal[6], typing_extensions.Literal[7]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple, TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(x: int, xs: Tuple[str, Unpack[Ts], bool]) -> Tuple[int, str, Unpack[Ts], bool]:
        y = (x, *xs)
        reveal_type(y)
        return y
    |}
           [
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, str, *test.Ts, bool]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple
      from typing_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(x: int, xs: Tuple[str, Unpack[Ts], bool]) -> Tuple[int, str, Unpack[Ts], bool]:
        y = (x, *xs)
        reveal_type(y)
        return y
    |}
           [
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, str, *test.Ts, bool]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(x: int, xs: Tuple[str, Unpack[Ts], bool]) -> Tuple[int, str, Unpack[Ts], bool]:
        y = (x, *xs)
        reveal_type(y)
        return y
    |}
           [
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, str, *test.Ts, bool]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple, TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(xs: Tuple[str, Unpack[Ts], bool]) -> None:
        y = ( *xs, *(2, 3), *xs)
        reveal_type(y)
    |}
           [
             "Unable to concatenate tuple [60]: Concatenation not yet support for multiple \
              variadic tuples: `*xs, *xs`.";
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[typing.Any, ...]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple

      def foo(x: int, some_list: List[int]) -> Tuple[int, ...]:
        y = (x, *some_list)
        reveal_type(y)
        return y
    |}
           ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[int, ...]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List

      def foo(x: int, some_list: List[str]) -> None:
        y = (x, *some_list)
        reveal_type(y)
    |}
           ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[str, ...]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple, Union

      def foo(x: int, union: Union[Tuple[int, str], Tuple[bool]]) -> None:
        y = (x, *union)
        reveal_type(y)
    |}
           [
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[Union[int, \
              str], ...]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Any

      # pyre-ignore[2]: Explicit Any.
      def foo(x: int, any: Any) -> None:
        y = (x, *any)
        reveal_type(y)
    |}
           [
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[typing.Any, \
              ...]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      def foo(x: int, not_iterable: int) -> None:
        y = (x, *not_iterable)
        reveal_type(y)
    |}
           [
             "Unable to concatenate tuple [60]: Expected to unpack an iterable, but got `int`.";
             "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[typing.Any, \
              ...]]`.";
           ];
    ]


let test_star_args =
  let assert_type_errors source errors context = assert_type_errors source errors context in
  test_list
    [
      (* We should be able to pass an unpacked list to `*args`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple, Unpack

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(xs: List[int]) -> None:
        expect_int( *xs)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple, Unpack

      def expect_int( *args: Unpack[Tuple[int, ...]]) -> None: ...

      def main(list_of_string: List[str]) -> None:
        expect_int( *list_of_string)
    |}
           [
             "Incompatible parameter type [6]: In call `expect_int`, for 1st positional argument, \
              expected `int` but got `str`.";
           ];
      (* ParamSpec `P.args` should be treated as having type `Tuple[object, ...]`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from pyre_extensions import ParameterSpecification
      from typing import Callable, Tuple, Unpack

      P = ParameterSpecification("P")

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def expect_object( *args: Unpack[Tuple[object, Unpack[Tuple[object, ...]]]]) -> None: ...

      def outer(f: Callable[P, int]) -> None:
        def inner( *args: P.args, **kwargs: P.kwargs) -> None:
          expect_object( *args)
          expect_int( *args)
    |}
           [
             "Invalid argument [32]: Argument types `*Tuple[object, ...]` are not compatible with \
              expected variadic elements `int, *Tuple[int, ...]`.";
           ];
      (* We should be able to pass multiple unpacked lists to `*args`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple, Unpack

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(xs: List[int]) -> None:
        expect_int( *xs, 42, *xs)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple, Unpack

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(xs: List[int], ys: List[str]) -> None:
        expect_int( *xs, *ys)
    |}
           [
             "Invalid argument [32]: Argument types `*Tuple[typing.Union[int, str], ...]` are not \
              compatible with expected variadic elements `int, *Tuple[int, ...]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple, Unpack

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(x: int) -> None:
        expect_int( *x)
    |}
           [
             "Invalid argument [32]: Unpacked argument `x` must have an unpackable type but has \
              type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple
      from typing_extensions import Unpack

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(x: int) -> None:
        expect_int( *x)
    |}
           [
             "Invalid argument [32]: Unpacked argument `x` must have an unpackable type but has \
              type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import List, Tuple
      from pyre_extensions import Unpack

      def expect_int( *args: int) -> None: ...

      def main(x: int) -> None:
        expect_int( *x)
    |}
           [
             "Invalid argument [32]: Unpacked argument `x` must have an unpackable type but has \
              type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple, Unpack

      def foo( *args: Unpack[Tuple[str, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main( *args: Unpack[Tuple[str, Unpack[Tuple[int, ...]]]]) -> None:
        foo( *args)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Tuple, Unpack

      def foo(x: str, y: int, *args: Unpack[Tuple[int, ...]]) -> None: ...

      def main( *args: Unpack[Tuple[str, int, Unpack[Tuple[int, ...]]]]) -> None:
        foo( *args)
        foo("hello", 1, 2, 3)

        wrong: Tuple[int, ...]
        foo( *wrong)
    |}
           [
             "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, \
              expected `str` but got `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import Optional

      def foo(
          x: int,
          y: Optional[int],
          z: str,
          a: Optional[int] = None,
      ) -> None: ...

      def main(xs: tuple[int, Optional[int], str]) -> None:
        foo( *xs)
    |}
           [];
    ]


let test_delete =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
p: tuple[int, str] = (1, "abc")
del p[0]
            |}
           [
             "Unable to delete tuple member [72]: Tuples are immutable, so their members may not \
              be deleted.";
           ];
    ]


let () =
  "tuple"
  >::: [
         test_check_tuple;
         test_tuple_literal_access;
         test_custom_tuple;
         test_length;
         test_unpacking;
         test_star_args;
         test_delete;
       ]
  |> Test.run
