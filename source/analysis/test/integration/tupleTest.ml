(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_tuple context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from builtins import return_tuple
      def derp()->int:
          a, b = return_tuple()
          return a+b
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(l: typing.List[int]) -> int:
        [a, b] = l
        return a + b
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Tuple[int, int]) -> None:
        a.tuple_method(1.0)
    |}
    [
      "Incompatible parameter type [6]: In call `tuple.tuple_method`, for 1st positional only \
       parameter expected `int` but got `float`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return (1, 2, 3)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, str]:
        return (1, "string", 3)
    |}
    ["Incompatible return type [7]: Expected `Tuple[int, str]` but got `Tuple[int, str, int]`."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got `Tuple[int, str, \
       int]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo()-> typing.Tuple[int, ...]:
        return tuple([1,2,3])
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return tuple([""])
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got "
      ^ "`typing.Tuple[str, ...]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return tuple([1])
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return (1, 2)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2.0)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Tuple[int, int, str]) -> typing.Tuple[str, int]:
        a, *b = x
        return b
    |}
    ["Incompatible return type [7]: Expected `Tuple[str, int]` but got `List[Union[int, str]]`."];
  assert_type_errors
    {|
      def derp() -> int:
        a, b = [1,2,3]
        return a + b
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        (x, y), z = 0
        return x + y + z
    |}
    ["Unable to unpack [23]: Unable to unpack `int` into 2 values."];
  assert_type_errors
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
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Sized:
        return (1,)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Sized:
        return (1, "")
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple:
        return ()
    |}
    ["Invalid type parameters [24]: Generic type `tuple` expects at least 1 type parameter."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[()]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, str]:
        return ()
    |}
    ["Incompatible return type [7]: Expected `Tuple[int, str]` but got `Tuple[]`."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Iterable[int]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      import typing
      def bar(z: typing.Optional[int]) -> typing.Tuple[int, typing.Optional[int]]:
          return 1, int_to_int(z) if z is not None else None
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo( *args: int) -> typing.Iterable[str]:
        return args
    |}
    ["Incompatible return type [7]: Expected `Iterable[str]` but got `typing.Tuple[int, ...]`."];
  assert_type_errors
    {|
      import collections
      T = collections.namedtuple('T', 'a b c')
      def b(d: T) -> None:
        a = d.a + d.d
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other than \
       `Any`.";
      "Missing attribute annotation [4]: Attribute `b` of class `T` must have a type other than \
       `Any`.";
      "Missing attribute annotation [4]: Attribute `c` of class `T` must have a type other than \
       `Any`.";
      "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `c` must have a type other than `Any`.";
      "Undefined attribute [16]: `T` has no attribute `d`.";
    ];
  assert_type_errors
    {|
      import collections
      import typing
      class C(collections.namedtuple('T', 'a b')):
        def __new__(cls, a: int) -> typing.Type[C]:
          ...
      C(1,2)
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `C` must have a type other than \
       `Any`.";
      "Missing attribute annotation [4]: Attribute `b` of class `C` must have a type other than \
       `Any`.";
      "Uninitialized attribute [13]: Attribute `a` is declared in class `C` to have type \
       `typing.Any` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `b` is declared in class `C` to have type \
       `typing.Any` but is never initialized.";
      "Too many arguments [19]: Call `C.__new__` expects 1 positional argument, 2 were provided.";
    ];
  assert_type_errors
    {|
      import collections
      T = collections.namedtuple('T', 'a b c')
      def foo(t: T) -> None:
        x, y = t
        x, y, z = t
        x, y, z, other = t
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other than \
       `Any`.";
      "Missing attribute annotation [4]: Attribute `b` of class `T` must have a type other than \
       `Any`.";
      "Missing attribute annotation [4]: Attribute `c` of class `T` must have a type other than \
       `Any`.";
      "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `c` must have a type other than `Any`.";
      "Unable to unpack [23]: Unable to unpack 3 values, 2 were expected.";
      "Unable to unpack [23]: Unable to unpack 3 values, 4 were expected.";
    ];
  assert_type_errors
    {|
      import collections
      T = collections.namedtuple('T', 'a')
      T(a=1)
      def foo() -> None:
        T(a=2)
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `T` must have a type other than \
       `Any`.";
      "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
    ];
  assert_type_errors
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
  assert_type_errors
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
  assert_type_errors
    {|
      def foo(input: int) -> None:
        x, y = input
    |}
    ["Unable to unpack [23]: Unable to unpack `int` into 2 values."];
  assert_type_errors {|
      def foo() -> bool:
        return (52,) < (1, 2, 3)
    |} [];
  assert_type_errors
    {|
      import typing
      class FooNotNamedTuple:
        bar: typing.Optional[str] = None
        baz: typing.Dict[int, typing.Any] = {}
        hello: typing.Dict[str, typing.Any] = {}
    |}
    [
      "Missing attribute annotation [4]: Attribute `baz` of class `FooNotNamedTuple` must have a \
       type that does not contain `Any`.";
    ];
  assert_type_errors
    (* The parameter `baz` in the __new__ method for NamedTuple will not throw a duplicate error for
       Any. *)
    {|
      import typing
      class Foo(typing.NamedTuple):
        bar: typing.Optional[str] = None
        baz: typing.Dict[int, typing.Any] = {}
        hello: typing.Dict[str, typing.Any] = {}
    |}
    [
      "Missing parameter annotation [2]: Parameter `baz` must have a type that does not contain \
       `Any`.";
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` must have a type that does \
       not contain `Any`.";
    ];
  assert_type_errors
    (* A __new__ method for a non-NamedTuple will throw error on Any. *)
    {|
      import typing
      class Foo:
        def __new__(cls, foo: typing.Dict[int, typing.Any] = {}) -> Foo:
            return super(Foo, cls).__new__(cls)
    |}
    [
      "Missing parameter annotation [2]: Parameter `foo` must have a type that does not contain \
       `Any`.";
    ];
  assert_type_errors
    (* If __new__ is not a method of some class, it will throw the Any error. *)
    {|
      import typing
      def __new__(foo: typing.Dict[int, typing.Any] = {}) -> None:
        pass
    |}
    [
      "Missing parameter annotation [2]: Parameter `foo` must have a type that does not contain \
       `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      X = typing.NamedTuple(
        "x",
        dates=str
      )
      X(dates="foo")
    |}
    [];
  assert_type_errors
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
  ()


let test_tuple_literal_access context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[0]
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[1]
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      def foo(p: int) -> int:
        x = (0, "one", 2)
        return x[p]
    |}
    ["Incompatible return type [7]: Expected `int` but got `Union[int, str]`."];
  assert_type_errors
    {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[-1]
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[-2]
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      def foo() -> int:
        i = 0
        x = (0, "one", 2)
        return x[i]
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        i = -2
        x = (0, "one", 2.)
        return x[-3]
    |}
    [];

  (* TODO(T41500114): This should trigger a separate error *)
  assert_type_errors
    {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[3]
    |}
    ["Incompatible return type [7]: Expected `int` but got `Union[int, str]`."];

  (* TODO(T41500251): This would ideally work as well *)
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, int]:
        x = (0, 1, "two")
        return x[0:2]
    |}
    [
      "Incompatible return type [7]: Expected `Tuple[int, int]` but got "
      ^ "`typing.Tuple[Union[int, str], ...]`.";
    ];
  assert_type_errors
    {|
      def func(a: int, b: str, c: bool) -> None:
        pass
      x = (42, "bla", False)
      func( *x)
    |}
    [];
  assert_type_errors
    {|
      def func(a: int, b: str, c: bool) -> None:
        pass
      c = ("bla", False)
      func(1, *c)
    |}
    [];
  assert_type_errors
    {|
      def func(a: int, b: str, c: bool) -> None:
        pass
      c = ("bla", )
      func(1, *c)
    |}
    ["Missing argument [20]: Call `func` expects argument `c`."];
  assert_type_errors
    {|
      def func(a: int, b: bool, c: str) -> None:
        pass
      c = ("bla", False)
      func(1, *c)
    |}
    [
      "Incompatible parameter type [6]: In call `func`, for 2nd positional only parameter expected \
       `bool` but got `str`.";
      "Incompatible parameter type [6]: In call `func`, for 3rd positional only parameter expected \
       `str` but got `bool`.";
    ];
  assert_type_errors
    {|
      def func(a: int, b: bool, c: str, d:int, e:str) -> None:
        pass
      c = (False, "ble")
      d = (1, "abc")
      func(1, *c, *d)
    |}
    [];
  ()


let test_custom_tuple context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
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
  assert_type_errors
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

  assert_type_errors
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
    ]


let test_length context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Tuple

      def foo() -> None:
        xs: Tuple[()]
        y = len(xs)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `int`."];
  ()


let test_unpacking context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Tuple

      def foo(x: int, xs: Tuple[str, bool]) -> None:

        y = (x, *xs)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `Tuple[int, str, bool]`."];
  assert_type_errors
    {|
      from typing import Tuple

      def foo(x: int, xs: str) -> None:

        y = (x, *xs)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[str, ...]]`."];
  assert_type_errors
    {|
      from typing import Tuple

      def foo() -> None:

        y = (1, *(2, 3), *(4, 5, 6), 7)
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `Tuple[typing_extensions.Literal[1], \
       typing_extensions.Literal[2], typing_extensions.Literal[3], typing_extensions.Literal[4], \
       typing_extensions.Literal[5], typing_extensions.Literal[6], typing_extensions.Literal[7]]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(x: int, xs: Tuple[str, Unpack[Ts], bool]) -> Tuple[int, str, Unpack[Ts], bool]:
        y = (x, *xs)
        reveal_type(y)
        return y
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, str, *test.Ts, bool]`."];
  assert_type_errors
    {|
      from typing import Tuple
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")

      def foo(xs: Tuple[str, Unpack[Ts], bool]) -> None:
        y = ( *xs, *(2, 3), *xs)
        reveal_type(y)
    |}
    [
      "Unable to concatenate tuple [60]: Concatenation not yet support for multiple variadic \
       tuples: `*xs, *xs`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[typing.Any, ...]`.";
    ];
  assert_type_errors
    {|
      from typing import List, Tuple

      def foo(x: int, some_list: List[int]) -> Tuple[int, ...]:
        y = (x, *some_list)
        reveal_type(y)
        return y
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[int, ...]]`."];
  assert_type_errors
    {|
      from typing import List

      def foo(x: int, some_list: List[str]) -> None:
        y = (x, *some_list)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[str, ...]]`."];
  assert_type_errors
    {|
      from typing import Tuple, Union

      def foo(x: int, union: Union[Tuple[int, str], Tuple[bool]]) -> None:
        y = (x, *union)
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[Union[bool, int, \
       str], ...]]`.";
    ];
  assert_type_errors
    {|
      from typing import Any

      # pyre-ignore[2]: Explicit Any.
      def foo(x: int, any: Any) -> None:
        y = (x, *any)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[typing.Any, ...]]`."];
  assert_type_errors
    {|
      def foo(x: int, not_iterable: int) -> None:
        y = (x, *not_iterable)
        reveal_type(y)
    |}
    [
      "Unable to concatenate tuple [60]: Expected to unpack an iterable, but got `int`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Tuple[int, *Tuple[typing.Any, ...]]`.";
    ];
  ()


let test_star_args context =
  let assert_type_errors = assert_type_errors ~context in
  (* We should be able to pass an unpacked list to `*args`. *)
  assert_type_errors
    {|
      from pyre_extensions import Unpack
      from typing import List, Tuple

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(xs: List[int]) -> None:
        expect_int( *xs)
    |}
    [];
  assert_type_errors
    {|
      from pyre_extensions import Unpack
      from typing import List, Tuple

      def expect_int( *args: Unpack[Tuple[int, ...]]) -> None: ...

      def main(list_of_string: List[str]) -> None:
        expect_int( *list_of_string)
    |}
    [
      "Incompatible parameter type [6]: In call `expect_int`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];
  (* ParamSpec `P.args` should be treated as having type `Tuple[object, ...]`. *)
  assert_type_errors
    {|
      from pyre_extensions import ParameterSpecification, Unpack
      from typing import Callable, Tuple

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
  assert_type_errors
    {|
      from pyre_extensions import Unpack
      from typing import List, Tuple

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(xs: List[int]) -> None:
        expect_int( *xs, 42, *xs)
    |}
    [];
  assert_type_errors
    {|
      from pyre_extensions import Unpack
      from typing import List, Tuple

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(xs: List[int], ys: List[str]) -> None:
        expect_int( *xs, *ys)
    |}
    [
      "Invalid argument [32]: Argument types `*Tuple[typing.Union[int, str], ...]` are not \
       compatible with expected variadic elements `int, *Tuple[int, ...]`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import Unpack
      from typing import List, Tuple

      def expect_int( *args: Unpack[Tuple[int, Unpack[Tuple[int, ...]]]]) -> None: ...

      def main(x: int) -> None:
        expect_int( *x)
    |}
    [
      "Invalid argument [32]: Unpacked argument `x` must have an unpackable type but has type `int`.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import Unpack
      from typing import List, Tuple

      def expect_int( *args: int) -> None: ...

      def main(x: int) -> None:
        expect_int( *x)
    |}
    [
      "Invalid argument [32]: Unpacked argument `x` must have an unpackable type but has type `int`.";
    ];
  ()


let () =
  "tuple"
  >::: [
         "check_tuple" >:: test_check_tuple;
         "literal_access" >:: test_tuple_literal_access;
         "custom_tuple" >:: test_custom_tuple;
         "length" >:: test_length;
         "unpacking" >:: test_unpacking;
         "star_args" >:: test_star_args;
       ]
  |> Test.run
