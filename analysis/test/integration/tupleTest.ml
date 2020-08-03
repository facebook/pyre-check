(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `tuple.tuple_method` but got \
         `float`.";
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
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, str]` but got "
      ^ "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got "
      ^ "`typing.Tuple[int, str, int]`.";
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
    [
      "Incompatible return type [7]: Expected `typing.Tuple[str, int]` but got "
      ^ "`typing.List[typing.Union[int, str]]`.";
    ];
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
    ["Incompatible return type [7]: Expected `typing.Tuple[int, str]` but got `typing.Tuple[]`."];
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
    [
      "Incompatible return type [7]: Expected `typing.Iterable[str]` but "
      ^ "got `typing.Tuple[int, ...]`.";
    ];
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
      "Unbound name [10]: Name `T` is used but not defined in the current scope.";
      "Unbound name [10]: Name `$unparsed_annotation` is used but not defined in the current scope.";
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
    ["Incompatible return type [7]: Expected `int` but got `typing.Union[int, str]`."];
  assert_type_errors
    {|
      def foo() -> int:
        i = 0
        x = (0, "one", 2)
        return x[i]
    |}
    [];

  (* TODO(T41500114): This should trigger a separate error *)
  assert_type_errors
    {|
      def foo() -> int:
        x = (0, "one", 2)
        return x[3]
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Union[int, str]`."];

  (* TODO(T41500251): This would ideally work as well *)
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, int]:
        x = (0, 1, "two")
        return x[0:2]
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, int]` but got "
      ^ "`typing.Tuple[typing.Union[int, str], ...]`.";
    ];
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


let () =
  "tuple"
  >::: [
         "check_tuple" >:: test_check_tuple;
         "literal_access" >:: test_tuple_literal_access;
         "custom_tuple" >:: test_custom_tuple;
       ]
  |> Test.run
