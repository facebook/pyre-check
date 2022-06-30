(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test
open OUnit2
open IntegrationTest

let assert_type_errors
    ?include_line_numbers
    ?update_environment_with
    ?show_error_traces
    ~context
    source
    expected_errors
  =
  assert_type_errors
    ~context
    ~constraint_solving_style:Configuration.Analysis.ExpressionLevel
    ?include_line_numbers
    ?update_environment_with
    ?show_error_traces
    source
    expected_errors;
  assert_type_errors
    ~context
    ~constraint_solving_style:Configuration.Analysis.FunctionCallLevel
    ?include_line_numbers
    ?update_environment_with
    ?show_error_traces
    source
    expected_errors


let test_check_method_returns context =
  assert_type_errors
    ~context
    {|
      def foo(input: str) -> int:
          return input.lower()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    ~context
    {|
      def foo(input: str) -> int:
          return input.lower().upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    ~context
    {|
      def foo() -> int:
          return ''.upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    ~context
    {|
      import typing
      def foo() -> typing.Optional[typing.List[int]]:
          return [3]
    |}
    [];

  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> typing.Optional[typing.List[C]]:
          return [C()]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> typing.Optional[typing.List[C]]:
          return [D()]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> typing.Union[typing.List[C], int]:
          return [C()]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> None:
          x: typing.Union[typing.List[C], int] = [C()]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> None:
          x: typing.Union[typing.List[C], int] = [D()]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> None:
          x: typing.Union[typing.Dict[int, C], int] = {1: D()}
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> None:
          x: typing.Union[typing.Set[C], int] = {D()}
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C:
          pass
      class D(C):
          pass
      def foo() -> typing.Union[typing.List[C], int]:
          return [D()]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      def foo() -> typing.Dict[int, typing.Optional[bool]]:
        return {
          **{1: True},
          **{3: False},
        }
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      def foo() -> typing.Dict[int, typing.Optional[bool]]:
        d1 = {1: True}
        d2 = {3: False}
        return {
          **d1,
          **d2,
        }
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class C: ...
      class D(C): ...
      def foo() -> typing.Mapping[C, C]:
        return { D(): D() }

      def bar(x: typing.Mapping[typing.Union[str, int], str]) -> None: ...

      bar({'foo': 'foo'})
    |}
    [];
  assert_type_errors
    ~context
    {|
      from typing import Dict, Mapping, Optional, Sequence, Tuple, Union
      from enum import Enum
      class FooEnum(Enum, str):
          FOO_FIELD = "hello"

      dict_string_to_int: Dict[str, int]
      dict_string_to_string: Dict[str, str]
      d1: Mapping[str, Union[int, str]] = {**dict_string_to_int, FooEnum.FOO_FIELD: FooEnum.FOO_FIELD}
      d2: Dict[str, Union[int, str]] = {**dict_string_to_int, "hello": "world"}
      d3: Dict[str, Union[int, str]] = {**dict_string_to_int, **dict_string_to_string}

      xs: Sequence[Tuple[int, int, str, Dict[str, Optional[str]]]] = [
          (1, 2, "hello", {"foo": "hello", "bar": None, "baz": None}),
          (3, 4, "world", {"foo": "hello", "bar": "hello", "baz": "hello"}),
      ]
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      def foo() -> None:
          f: typing.Callable
          f(1, 2)
    |}
    ["Invalid type parameters [24]: Generic type `typing.Callable` expects 2 type parameters."];
  assert_type_errors
    ~context
    {|
      import typing
      def foo() -> None:
          x: typing.Tuple
          x = (1, 2)
    |}
    ["Invalid type parameters [24]: Generic type `tuple` expects at least 1 type parameter."];
  ()


let test_check_inverse_operator context =
  assert_type_errors
    ~context
    {|
      from typing import Optional, Tuple
      x: int
      optional_x: Optional[int]

      class C:
        def __rrshift__(self, other: int) -> int: ...

      class D:
        def __rshift__(self, other: int) -> int: ...

      # Only one of them has the operator.
      x < optional_x
      optional_x < x
      x + optional_x
      optional_x + x
      optional_x == x
      optional_x != x

      # Both have the operator.
      "foo" >> C()
      D() >> "foo"
      D() >> C()

      # Neither has the operator.
      D() + C()
      C() << C()
    |}
    [
      "Unsupported operand [58]: `<` is not supported for operand types `int` and `Optional[int]`.";
      "Unsupported operand [58]: `<` is not supported for operand types `Optional[int]` and `int`.";
      "Unsupported operand [58]: `+` is not supported for operand types `int` and `Optional[int]`.";
      "Unsupported operand [58]: `+` is not supported for operand types `Optional[int]` and `int`.";
      "Unsupported operand [58]: `>>` is not supported for operand types `str` and `C`.";
      "Unsupported operand [58]: `>>` is not supported for operand types `D` and `str`.";
      "Unsupported operand [58]: `>>` is not supported for operand types `D` and `C`.";
      "Unsupported operand [58]: `+` is not supported for operand types `D` and `C`.";
      "Unsupported operand [58]: `<<` is not supported for operand types `C` and `C`.";
    ];
  (* Explicit use of `__lt__` gets the full error message. *)
  assert_type_errors
    ~context
    {|
      from typing import Optional

      x: int
      optional_x: Optional[int]
      x.__lt__(optional_x)
      x < optional_x
    |}
    [
      "Incompatible parameter type [6]: In call `int.__lt__`, for 1st positional only parameter \
       expected `int` but got `Optional[int]`.";
      "Unsupported operand [58]: `<` is not supported for operand types `int` and `Optional[int]`.";
    ];
  assert_type_errors
    ~context
    {|
      def foo() -> None:
        y = 1
        reveal_type(y)
        y += "some string"
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `typing_extensions.Literal[1]`.";
      "Incomplete type [37]: Type `pyre_extensions.IntExpression[1 + N2]` inferred for `y` is \
       incomplete, add an explicit annotation.";
      "Unsupported operand [58]: `+` is not supported for operand types `int` and `str`.";
    ];
  assert_type_errors
    ~context
    {|
      class C:
        pass

      class D:
        def __gt__(self, other: object) -> bool: ...
        def __lt__(self, other: object) -> bool: ...
        def __le__(self, other: object) -> bool: ...
        def __ge__(self, other: object) -> bool: ...

      def expects_bool(x: bool) -> None: ...

      expects_bool(C() < D())
      expects_bool(C() > D())
      expects_bool(C() <= D())
      expects_bool(C() >= D())
    |}
    [];
  assert_type_errors
    ~context
    {|
      x1: bool = 5 < 6.0
      x2: bool = 5 > 6.0
      x3: bool = 5 <= 6.0
      x4: bool = 5 >= 6.0
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C:
        def __rshift__(self, other: object) -> int:
          return 1
      class D: pass

      def foo() -> int:
        return (C() >> D())
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C:
        pass
      class D:
        def __rshift__(self, other: object) -> int:
          return 1

      def foo() -> int:
        return (C() >> D())
    |}
    ["Unsupported operand [58]: `>>` is not supported for operand types `C` and `D`."];
  assert_type_errors
    ~context
    {|
      class C:
        pass
      class D:
        def __rrshift__(self, left: C) -> C:
          return left

      def foo() -> C:
        return (C() >> D())
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C:
        pass
      class D:
        def __rrshift__(self, left: C) -> C:
          return left

      def foo() -> C:
        return (C() >> D() >> D())
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C:
        pass
      class D:
        def __rrshift__(self, other: object) -> int:
          return 1

      def foo() -> int:
        return (C() >> D())
    |}
    [];
  assert_type_errors
    ~context
    {|
      class C:
        def __rrshift__(self, other: int) -> int:
          return 1

      def foo() -> None:
        z = ("foo" >> C())
    |}
    ["Unsupported operand [58]: `>>` is not supported for operand types `str` and `C`."];
  assert_type_errors
    ~context
    {|
      import typing
      def foo() -> None:
        d: typing.Dict[str, typing.Any] = {"foo": 3}
        z = d["foo"] + d["foo"]
    |}
    (* There should be no attribute error for Any. *)
    [];
  assert_type_errors
    ~context
    {|
      import typing
      def foo(xs: typing.Iterable[typing.Any]) -> None:
        a, b = xs
        reveal_type(a)
        z = a * b
    |}
    (* There should be no attribute error for undefined. *)
    [
      "Missing parameter annotation [2]: Parameter `xs` must have a type that does not contain \
       `Any`.";
      "Revealed type [-1]: Revealed type for `a` is `typing.Any`.";
    ];
  assert_type_errors
    ~context
    (* Any % int should return unknown, not int (via the int __mod__ operator). This is necessary
       for dealing with edge cases with old-style format strings when the format string happens to
       be typed as Any. *)
    {|
      import typing
      def baz(d: typing.Dict[str, typing.Any]) -> None:
        a = d["foo"]
        reveal_type(a)
        reveal_type(a % 3)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `a.__mod__(3)` is `typing.Any`.";
    ];
  assert_type_errors
    ~context
    (* Bottom % int should return unknown, not int (via the int __mod__ operator). *)
    {|
      import typing
      def foo(xs: typing.Iterable[typing.Any]) -> None:
        a, b = xs
        reveal_type(a)
        reveal_type(a % 3)
    |}
    [
      "Missing parameter annotation [2]: Parameter `xs` must have a type that does not contain \
       `Any`.";
      "Revealed type [-1]: Revealed type for `a` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `a.__mod__(3)` is `typing.Any`.";
    ];
  assert_type_errors
    ~context
    {|
      def foo(x: int, y: int) -> int:
          return y * x
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(x: float, y: float) -> float:
          return y * x
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(x: int, y: float) -> float:
          return x * y
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(x: float, y: int) -> float:
          return x * y
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(x: float, y: int) -> float:
          return x + y
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(x: int, y: float) -> float:
          return x + y
    |}
    [];
  assert_type_errors
    ~context
    {|
      class E:
        def __rrshift__(self, left: F) -> F:
          return left

      class F:
        def __rshift__(self, right: F) -> F:
          return right

      z: F = F() >> E()
    |}
    [];
  (* When an operator doesn't exist on either side, raise an undefined attribute error. *)
  assert_type_errors
    ~context
    {|
      class D: ...

      y1: D
      y1.__radd__(D())

      # Wrong number of arguments.
      y2: D
      y2.__add__(1, 2)

      my_union: str | D
      my_union.__radd__(D())
    |}
    [
      "Undefined attribute [16]: `D` has no attribute `__radd__`.";
      "Undefined attribute [16]: `D` has no attribute `__add__`.";
      "Undefined attribute [16]: `typing.Union` has no attribute `__radd__`.";
    ];
  (* When an object has type `Any`, we should not use the inverse operator since the `Any` object
     might have the original operator. Just treat it as an unknown callable. *)
  assert_type_errors
    ~context
    {|
      from typing import Any

      class D:
        def __radd__(self, other: D) -> D: ...

        def __call__(self, other: int) -> bool: ...

      # pyre-ignore[2]: Deliberate use of `Any`.
      def main(x: Any) -> None:
        y = x.__add__(D())
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Any`."];
  ()


let test_check_method_parameters context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  (* Calls to methods *)
  assert_type_errors {|
      def foo(input: str) -> None:
        input.substr(1)
    |} [];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: In call `str.substr`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      def foo(a: str, b: str) -> None:
        pass
      def bar() -> None:
        foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `str` but got `int`.";
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `str` but got `int`.";
    ];
  assert_type_errors
    {|
      from typing import Optional
      def bar(x: int) -> int: ...
      def baz(x: int, y: int) -> None: ...
      def foo(x: Optional[int]) -> None:
        baz(bar("derp"), x if x else 0)
    |}
    [
      "Incompatible parameter type [6]: In call `bar`, for 1st positional only parameter expected \
       `int` but got `str`.";
    ];
  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: In call `str.substr`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf').substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: In call `str.substr`, for 1st positional only parameter \
       expected `int` but got `str`.";
      "Incompatible parameter type [6]: In call `str.substr`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input + 1
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `str` and `int`."];
  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.__sizeof__()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      def foo(b: str, a: str) -> None: ...
      b: int = 1
      a: int = 1
      foo(b, a)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `str` but got `int`.";
      "Incompatible parameter type [6]: In call `foo`, for 2nd positional only parameter expected \
       `str` but got `int`.";
    ];

  (* Special Methods *)
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Type[int]) -> str:
        return str(x)
    |}
    [];
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Iterable[int]) -> int:
        return x[0]
    |}
    ["Undefined attribute [16]: `typing.Iterable` has no attribute `__getitem__`."];
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Type[int], y: object) -> bool:
        return x == y
    |}
    [];
  assert_strict_type_errors
    {|
      import typing
      # adding `or` to avoid triggering type alias validation
      x = typing.Mapping[int] or None
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has no type specified.";
      "Incompatible parameter type [6]: In call `typing.GenericMeta.__getitem__`, for 1st \
       positional only parameter expected `Tuple[Type[Variable[_KT]], \
       Type[Variable[_VT_co](covariant)]]` but got `Type[int]`.";
    ];
  assert_strict_type_errors
    {|
      import typing
      x = typing.Mapping[int, str]
      reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Type[typing.Mapping[int, str]]`."];
  assert_strict_type_errors
    {|
      class Meta(type):
          def foo(self) -> None: ...

      class Foo(metaclass=Meta):
          def foo(self) -> None: ...

      reveal_type(Foo.foo)
      reveal_type(Foo().foo)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo.foo` is "
      ^ "`typing.Callable(Foo.foo)[[Named(self, Foo)], None]`.";
      "Revealed type [-1]: Revealed type for `test.Foo().foo` is "
      ^ "`BoundMethod[typing.Callable(Foo.foo)[[Named(self, Foo)], None], Foo]`.";
    ];
  assert_strict_type_errors
    {|
      class Meta(type):
          def __getitem__(self, item: int) -> int: ...

      class Foo(metaclass=Meta):
          def __getitem__(self, item: int) -> str: ...

      reveal_type(Foo[1])
      reveal_type(Foo()[1])
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo[1]` is `int`.";
      "Revealed type [-1]: Revealed type for `test.Foo()[1]` is `str`.";
    ];
  assert_strict_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')

      class EnumMeta(type):
          def __getitem__(self: typing.Type[_T], name: str) -> _T: ...

      class Enum(metaclass=EnumMeta): ...

      # Definition in class str: def __getitem__(self, i: Union[int, slice]) -> str: ...

      class StringEnum(Enum, str): ...

      reveal_type(StringEnum["key"])

      class StringEnumTwo(str, Enum): ...

      reveal_type(StringEnumTwo["key"])
    |}
    [
      "Revealed type [-1]: Revealed type for `test.StringEnum[\"key\"]` is `StringEnum`.";
      "Revealed type [-1]: Revealed type for `test.StringEnumTwo[\"key\"]` is `StringEnumTwo`.";
    ];

  (* Defining methods *)
  assert_type_errors
    {|
      class Foo:
        def bar(self) -> None:
          def baz(x: int) -> int:
            return x
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def bar(x: int) -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `self` parameter."];
  assert_type_errors {|
      class Foo:
        def bar(x: Foo) -> None:
          return
    |} [];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar(x: int) -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `cls` parameter."];
  assert_type_errors
    {|
      import typing
      class Foo:
        @classmethod
        def bar(x: typing.Type[Foo]) -> None:
          return
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def bar() -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `self` parameter."];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar() -> None:
          return
    |}
    ["Invalid method signature [47]: Non-static method must specify `cls` parameter."];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def bar() -> None:
          return
    |}
    [];
  assert_type_errors
    {|
    import typing
    class Foo:
      @staticmethod
      def __new__(cls) -> typing.Type[Foo]: ...
    Foo()
  |}
    [];

  (* TODO(T45029821): Eliminate special casing so that calls to Foo() error here. *)
  assert_type_errors
    {|
    import typing
    class Foo:
      @staticmethod
      def __new__() -> typing.Type[Foo]: ...
    Foo()
  |}
    [];
  assert_type_errors
    {|
      import typing
      class Foo:
        def foo(self, x: typing.Optional[typing.Set[int]] = ...) -> None:
          self.x = x
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `x`."];
  assert_type_errors
    {|
      from typing import List, TypeVar
      T = TypeVar("T")
      def wrap(x: T) -> List[T]:
        return [x]
      class Foo:
        @wrap
        @classmethod
        def bar(cls) -> None:
          return
      reveal_type(Foo.bar)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo.bar` is \
       `List[typing.ClassMethod[typing.Callable(Foo.bar)[[Named(cls, typing.Type[Foo])], None]]]`.";
    ];
  assert_type_errors
    {|
      from typing import List, TypeVar
      T = TypeVar("T")
      def wrap(x: T) -> List[T]:
        return [x]
      class Foo:
        def __init_subclass__(cls) -> None:
          return
        @classmethod # shouldn't "double wrap" this
        def __class_getitem__(cls) -> None:
          return
      reveal_type(Foo().__init_subclass__)
      reveal_type(Foo().__class_getitem__)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo().__init_subclass__` is \
       `BoundMethod[typing.Callable(Foo.__init_subclass__)[[Named(cls, typing.Type[Foo])], None], \
       typing.Type[Foo]]`.";
      "Revealed type [-1]: Revealed type for `test.Foo().__class_getitem__` is \
       `BoundMethod[typing.Callable(Foo.__class_getitem__)[[Named(cls, typing.Type[Foo])], None], \
       typing.Type[Foo]]`.";
    ];
  assert_type_errors
    {|
      from typing import List, TypeVar, Callable
      T = TypeVar("T")
      def wrap(x: T) -> List[T]:
        return [x]
      class CallableClass:
        def __call__(self, x: int) -> str:
          return "A"
      def masquerade(x: object) -> Callable[[int], str]:
        return CallableClass()
      class Foo:
        @wrap # the magic should only be triggered on "plain functions", so this should be skipped
        def __init_subclass__(cls) -> None:
          return
                    # the type system isn't smart enough to distinguish that callable from a function,
        @masquerade # so we erroneously wrap it
        def __class_getitem__(cls) -> None:
          return

      reveal_type(Foo().__init_subclass__)
      reveal_type(Foo().__class_getitem__)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo().__init_subclass__` is \
       `List[typing.Callable(Foo.__init_subclass__)[[Named(cls, typing.Type[Foo])], None]]`.";
      "Revealed type [-1]: Revealed type for `test.Foo().__class_getitem__` is \
       `BoundMethod[typing.Callable[[int], str], typing.Type[Foo]]`.";
    ];
  assert_type_errors
    {|
      from typing import List, TypeVar
      T = TypeVar("T")
      def wrap(x: T) -> List[T]:
        return [x]
      class Foo:
        @wrap
        @staticmethod
        def s() -> None:
          return
      reveal_type(Foo().s)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Foo().s` is \
       `List[typing.StaticMethod[typing.Callable(Foo.s)[[], None]]]`.";
    ];
  assert_type_errors
    ~include_line_numbers:true
    {|
      xs = (1, 2, 3)
      def foo(x: int, y: int, z: str) -> None: ...
      foo( *xs )
    |}
    [
      "4: Incompatible parameter type [6]: In call `foo`, for 3rd positional only parameter \
       expected `str` but got `int`.";
    ];
  ()


let test_check_abstract_methods context =
  let update_environment_with =
    [
      {
        handle = "abc.pyi";
        (* This is just a mock stub of abc and is not meant to be accurate or complete *)
        source =
          {|
          from typing import Any
          def abstractmethod(funcobj: Any) -> Any: ...
          def abstractproperty(property: Any) -> Any: ...
        |};
      };
    ]
  in
  assert_type_errors
    ~context
    ~update_environment_with
    {|
      import abc
      @abc.abstractmethod
      def abstract()->int:
        pass
    |}
    [];
  assert_type_errors
    ~context
    ~update_environment_with
    {|
      import abc
      @abc.abstractproperty
      def abstract()->int:
        pass
    |}
    []


let test_check_behavioral_subtyping__strengthened_postcondition context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class Foo():
        def foo(self) -> int: ...
      class Bar(Foo):
        def foo(self) -> float: return 1.0
    |}
    [
      "Inconsistent override [15]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `float` is not a subtype of the overridden return `int`.";
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self) -> float: ...
      class Bar(Foo):
        def foo(self) -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo(self) -> int: ...
      class Bar(Foo):
        def foo(self) -> None: pass
    |}
    [
      "Inconsistent override [15]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `None` is not a subtype of the overridden return `int`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Bar(Foo[float]):
        def foo(self) -> str: return ""
    |}
    [
      "Inconsistent override [15]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `str` is not a subtype of the overridden return `float`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Bar(Foo[float]):
        def foo(self) -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def foo(self) -> str: return ""
    |}
    [
      "Inconsistent override [15]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Returned type `str` is not a subtype of the overridden return `float`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def foo(self) -> _T: ...
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def foo(self) -> int: return 1
    |}
    [];
  assert_type_errors
    ~show_error_traces:true
    {|
      import typing
      class Foo():
        def bar(self, x: int) -> int:
          return 1
      class Bar(Foo):
        def bar(self, x: int) -> typing.Union[str, int]:
          return 1
    |}
    [
      "Inconsistent override [15]: `test.Bar.bar` overrides method defined in `Foo` "
      ^ "inconsistently. Returned type `typing.Union[int, str]` is not a subtype "
      ^ "of the overridden return `int`.";
    ];

  (* Decorators are applied. *)
  assert_type_errors
    {|
      import typing
      import contextlib
      class Foo():
        @contextlib.contextmanager
        def foo(self) -> typing.Generator[int, None, None]: ...
      class Bar():
        @contextlib.contextmanager
        def foo(self) -> typing.Generator[int, None, None]: ...
    |}
    [];
  (* Method resolution order. TODO(T124880784): the reason Pyre behaves this way in terms of
     implementation is that when we resolve the parent attribute the lookup respects method
     resolution order. But this is not actually sound - an instance of Bar is a valid value of type
     Foo2, and therefore the `foo_legal_override` function is in fact not substitutable. *)
  assert_type_errors
    {|
     class Foo1:
       def foo_illegal_override(self, input: int) -> int:
         return input
       def foo_legal_override(self, input: int) -> int:
         return input


     class Foo2:
       def foo_illegal_override(self, input: int) -> str:
         return str(input)
       def foo_legal_override(self, input: int) -> str:
         return str(input)


     class Bar(Foo1, Foo2):
       def foo_illegal_override(self, input: int) -> str:
         return "Bar_A: " + str(input)

       def foo_legal_override(self, input: int) -> int:
         return 0

     |}
    [
      "Inconsistent override [15]: `test.Bar.foo_illegal_override` overrides method defined in \
       `Foo1` inconsistently. Returned type `str` is not a subtype of the overridden return `int`.";
    ];
  assert_type_errors
    {|
     class Foo:
       def same_method(self, input: int) -> int:
         return input

     class Bar(Foo):
       def same_method(self, input: int) -> str:
         return str(input)
  |}
    [
      "Inconsistent override [15]: `test.Bar.same_method` overrides method defined in `Foo` \
       inconsistently. Returned type `str` is not a subtype of the overridden return `int`.";
    ];
  ()


let test_check_behavioral_subtyping__weakened_precondition context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class Foo():
        @classmethod
        def foo(cls, a: float) -> None: ...
      class Bar(Foo):
        @classmethod
        def foo(cls, a: int) -> None: pass
      |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `int` is not a supertype of the overridden parameter `float`.";
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: float) -> None: ...
      class Bar(Foo):
        def foo(self, a: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `int` is not a supertype of the overridden parameter `float`.";
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: ...
      class Bar(Foo):
        def foo(self) -> None: pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `a` in overriding signature.";
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: ...
      class Bar(Foo):
        def foo(self, a) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, ) -> None: ...
      class Bar(Foo):
        def foo(self, a) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a) -> None: ...
      class Bar(Foo):
        def foo(self, a: int) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: pass
      class Bar(Foo):
        def foo(self, b: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `a` in overriding signature.";
    ];
  assert_type_errors
    ~show_error_traces:true
    {|
      import typing
      class Foo():
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
      class Bar(Foo):
        def bar(self, x: int) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.bar` overrides method defined in `Foo` "
      ^ "inconsistently. Parameter of type `int` is not a "
      ^ "supertype of the overridden parameter `typing.Union[int, str]`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Bar(Foo[float]):
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.bar` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `typing.Union[int, str]` is not a supertype "
      ^ "of the overridden parameter `typing.Union[float, str]`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Bar(Foo[int]):
        def bar(self, x: typing.Union[str, float]) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.bar` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `typing.Union[int, str]` is not a supertype "
      ^ "of the overridden parameter `typing.Union[float, str]`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[int]):
        def bar(self, x: typing.Union[str, float]) -> None:
          pass
    |}
    [];

  (* Starred arguments. *)
  assert_type_errors
    {|
      class C:
        def f(self, *args: int) -> None: ...
      class D(C):
        def f(self, *args: int) -> None: ...
    |}
    [];

  (* Keyword arguments. *)
  assert_type_errors
    {|
      class C:
        def f(self, **kwargs: str) -> None: ...
      class D(C):
        def f(self, **kwargs: str) -> None: ...
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo(self, input: int) -> int: ...
      class Bar(Foo):
        def foo(self, input) -> int: ...
    |}
    ["Missing parameter annotation [2]: Parameter `input` has no type specified."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T", bound=int)
      class Foo():
        def foo(self, x: T) -> str:
          return ""
      class Bar(Foo[str]):
        def foo(self, x: str) -> str:
          return x
    |}
    [
      "Invalid type parameters [24]: Non-generic type `Foo` cannot take parameters.";
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` \
       inconsistently. "
      ^ "Parameter of type `str` is not a supertype of the overridden parameter "
      ^ "`Variable[T (bound to int)]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class Foo(typing.Generic[T]):
        def foo(self) -> T: ...
      class Bar(Foo[int]):
        def foo(self) -> int:
          return 1
      class BarTwo(Foo[None]):
        def foo(self) -> None:
          pass
    |}
    [];

  (* Positional-only parameters *)
  assert_type_errors
    {|
      class Foo:
          def bar(self, __x: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, __y: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, x: int, /) -> str:
              return ""
      class Bar(Foo):
          def bar(self, y: int, /) -> str:
              return ""
    |}
    [];
  ()


let test_check_behavioral_subtyping__overloads context =
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_default_type_errors
    {|
      import typing

      class Foo():
        @typing.overload
        def foo(self, input: int) -> int: ...
        @typing.overload
        def foo(self, input: str) -> str: ...
        def foo(self, input: typing.Union[int, str]) -> typing.Union[int, str]:
          return input

      class Bar(Foo):
        @typing.overload
        def foo(self, input: int) -> int:
          return input
        @typing.overload
        def foo(self, input: str) -> str:
          return input
        def foo(self, input: typing.Union[int, str]) -> typing.Union[int, str]:
          return input
    |}
    [];
  assert_default_type_errors
    {|
      import typing

      class Foo():
        @typing.overload
        def foo(self, input: int) -> int: ...
        @typing.overload
        def foo(self, input: str) -> str: ...
        def foo(self, input: typing.Union[int, str]) -> typing.Union[int, str]:
          return input

      class Bar(Foo):
        @typing.overload
        def foo(self, input: int) -> int:
          return input
        def foo(self, input: typing.Union[int, str]) -> typing.Union[int, str]:
          return input
    |}
    [];
  assert_default_type_errors
    {|
      import typing

      class A: pass
      class B: pass
      class C: pass

      class Foo():
        @typing.overload
        def foo(self, input: A) -> A: ...
        @typing.overload
        def foo(self, input: B) -> B: ...
        @typing.overload
        def foo(self, input: C) -> C: ...
        def foo(self, input: typing.Union[A, B, C]) -> typing.Union[A, B, C]:
          return input

      class Bar(Foo):
        @typing.overload
        def foo(self, input: A) -> A:
          return input
        @typing.overload
        def foo(self, input: B) -> B:
          return input
        def foo(self, input: typing.Union[A, B]) -> typing.Union[A, B]:
          return input
    |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` \
       inconsistently. Parameter of type `typing.Union[A, B]` is not a supertype of the overridden \
       parameter `typing.Union[A, B, C]`.";
    ];
  assert_default_type_errors
    {|
      import typing

      class A: pass
      class B: pass
      class C: pass

      class Foo():
        @typing.overload
        def foo(self, input: A) -> A: ...
        @typing.overload
        def foo(self, input: B) -> B: ...
        @typing.overload
        def foo(self, input: C) -> C: ...
        def foo(self, input: typing.Union[A, B, C]) -> typing.Union[A, B, C]:
          return input

      class Bar(Foo):
        @typing.overload
        def foo(self, input: A) -> A:
          return input
        @typing.overload
        def foo(self, input: B) -> B:
          return input
    |}
    [
      "Missing overload implementation [42]: Overloaded function `Bar.foo` must have an \
       implementation.";
    ];
  ()


let test_check_behavioral_subtyping__special_cases context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  (* Don't warn on constructors. *)
  assert_type_errors
    {|
      class Foo():
        def __init__(self, a: float) -> None: ...
      class Bar(Foo):
        def __init__(self, a: int) -> None: pass
    |}
    [];

  (* Don't warn on allowlisted dunder methods *)
  assert_type_errors
    {|
      class Foo():
        def __eq__(self, other: object) -> bool: ...
      class Bar(Foo):
        def __eq__(self, other: int) -> bool: ...
    |}
    [];

  (* Do warn on other dunder methods. *)
  assert_type_errors
    {|
      class Foo():
        def __dunder__(self, a: float) -> None: ...
      class Bar(Foo):
        def __dunder__(self, a: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.__dunder__` overrides method defined in `Foo` \
       inconsistently. Parameter of type `int` is not a supertype of the overridden parameter \
       `float`.";
    ];

  (* Ensure that our preprocessing doesn't clobber starred argument names. *)
  assert_type_errors
    {|
      class Foo():
        def foo( **kwargs) -> int: ...
      class Bar(Foo):
        def foo( **kwargs) -> int: ...
    |}
    [];

  (* Ignore anything involving `Any`. *)
  assert_default_type_errors
    {|
      import typing
      class Foo():
        def __eq__(self, o: typing.Any) -> typing.Any: ...
      class Bar(Foo):
        def __eq__(self, o: int) -> int: pass
    |}
    [];

  (* Overrides when both *args and **kwargs exist are not inconsistent. *)
  assert_default_type_errors
    {|
      import typing
      class Foo():
        def f(self, a: float) -> None: ...
      class Bar(Foo):
        def f(self, *args: typing.Any) -> None: pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.f` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `a` in overriding signature.";
    ];
  assert_default_type_errors
    {|
      import typing
      class Foo():
        def f(self, b: int) -> None: ...
      class Bar(Foo):
        def f(self, **kwargs: typing.Any) -> None: pass
    |}
    [
      "Inconsistent override [14]: `test.Bar.f` overrides method defined in `Foo` inconsistently. "
      ^ "Could not find parameter `b` in overriding signature.";
    ];
  assert_default_type_errors
    {|
      import typing
      class Foo():
        def f(self, c: str) -> None: ...
      class Bar(Foo):
        def f(self, *args: typing.Any, **kwargs: typing.Any) -> None: pass
    |}
    [];
  ()


let test_check_behavioral_subtyping__attribute_kinds context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
     class Foo:
       foo: int = 3

     class Bar(Foo):
       def foo(self, input: int) -> str:
          ...
    |}
    [];
  ()


let test_check_nested_class_inheritance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class X():
          class Q():
              pass

      class Y(X):
          pass

      def foo() -> X.Q:
          return Y.Q()
    |}
    [];
  assert_type_errors
    {|
      class X():
          class Q():
              pass

      class Y(X):
          pass

      def foo() -> X.Q:
          return X.Q()
    |}
    [];
  assert_type_errors
    {|
      class X():
          class Q():
              pass

      class Y(X):
          pass

      class Z():
          class Q():
              pass

      def foo() -> X.Q:
          return Z.Q()
    |}
    ["Incompatible return type [7]: Expected `X.Q` but got `Z.Q`."];
  assert_type_errors
    {|
      class X:
        class N:
          class NN:
            class NNN:
              pass
      class Y(X):
        pass
      def foo() -> X.N.NN.NNN:
          return Y.N.NN.NNN()
    |}
    [];
  assert_type_errors
    {|
      class B1:
        class N:
          pass
      class B2:
        class N:
          pass
      class C(B1, B2):
        pass
      def foo() -> B1.N:
        return C.N()
    |}
    []


let test_check_method_resolution context =
  assert_type_errors
    ~context
    {|
      def foo() -> None:
        bar().baz()
    |}
    ["Unbound name [10]: Name `bar` is used but not defined in the current scope."];
  assert_type_errors ~context {|
      def foo(input: str) -> None:
        input.lower()
    |} [];
  assert_type_errors
    ~context
    {|
      import typing
      class Foo:
        def __getattr__(self, name: str) -> typing.Any: ...
      def foo(x: Foo) -> None:
        reveal_type(x.attribute)
        x.attribute + 1
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Revealed type [-1]: Revealed type for `x.attribute` is `typing.Any`.";
    ];
  assert_type_errors
    ~context
    {|
      import typing
      def foo(input: typing.Type[typing.Protocol]) -> None:
        reveal_type(input[42])
    |}
    ["Revealed type [-1]: Revealed type for `input[42]` is `typing.Type[typing.Protocol[]]`."];
  assert_type_errors
    ~context
    {|
      from typing import overload, Generic, TypeVar

      T = TypeVar("T")

      class Foo(Generic[T]):
        def __add__(self, other: Foo[T]) -> Foo[int]: ...

      def main() -> None:
        x: Foo[int]
        good: Foo[int]
        x += good

        reveal_type(x.__iadd__)
        bad: Foo[str]
        x += bad
    |}
    [
      "Revealed type [-1]: Revealed type for `x.__iadd__` is \
       `BoundMethod[typing.Callable(Foo.__add__)[[Named(self, Foo[int]), Named(other, Foo[int])], \
       Foo[int]], Foo[int]]`.";
      "Unsupported operand [58]: `+` is not supported for operand types `Foo[int]` and `Foo[str]`.";
    ];
  ()


let test_check_callables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      x: int = 1
      x()
    |}
    ["Call error [29]: `int` is not a function."];
  assert_type_errors {|
      import typing
      x: typing.Type[int]
      x()
    |} [];
  assert_type_errors
    {|
    import typing
    class A:
      def __init__(self) -> None: pass
    class B:
      def __init__(self) -> None: pass
    def foo(
      x: typing.Type[typing.Union[A, B]],
    ) -> None:
      x()
      [A, B][0]()
    |}
    [];
  assert_type_errors
    {|
    import typing
    x: typing.Type[typing.Union[int, str]]
    x()
    |}
    [];
  assert_type_errors {|
    types = [int, str]
    types[0]()
    |} [];
  assert_type_errors
    {|
    x: int
    x = 3
    [int, x][0]()
    |}
    ["Call error [29]: `typing.Union[typing.Type[int], int]` is not a function."];
  assert_type_errors
    {|
      def foo() -> None: pass
      def bar() -> None: pass
      x = foo
      x()
      x = bar
      x()
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Callable(foo)[[], None]` \
       but is used as type `typing.Callable(bar)[[], None]`.";
    ];
  assert_type_errors
    {|
      def foo() -> None: pass
      def bar() -> None: pass
      test: bool
      if test:
        x = foo
      else:
        x = bar
      reveal_type(x)
      x()
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Callable(foo)[[], None]` \
       but is used as type `typing.Callable(bar)[[], None]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable(foo)[[], None]`.";
    ];
  assert_type_errors
    {|
      import typing
      class A:
        def __init__(self) -> None: pass
      class B:
        def __init__(self) -> None: pass
      def foo(
        x: typing.Union[typing.Type[A], typing.Type[B]],
        y: typing.Type[A],
        z: typing.Type[B],
      ) -> None:
        x()
        y()
        z()
    |}
    [];
  assert_type_errors
    {|
      import typing
      class A:
        def __init__(self) -> None: pass
      def foo() -> None: pass
      def bar(x: typing.Union[typing.Type[A], typing.Callable[[], None]]) -> None:
        x()
    |}
    [];
  assert_type_errors
    {|
      from typing import Any
      def foo(x: BoundMethod[Any, int]) -> None:
        y = x()
        reveal_type(y)
    |}
    [
      "Missing parameter annotation [2]: Parameter `x` must have a type that does not contain `Any`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      from typing import overload, TypeVar, Tuple
      T = TypeVar("T")
      class CallableClass:
        @overload
        def __call__(self, x: str, t: T) -> Tuple[str, T]: ...
        @overload
        def __call__(self, x: int, t: T) -> Tuple[int, T]: ...
        def __call__(self, x: object, t: object) -> object: ...
      def foo(x: BoundMethod[CallableClass, int]) -> None:
        y = x(1.0)
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `Tuple[int, float]`."];
  ()


let test_check_callable_protocols context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  (* Objects with a `__call__` method are callables. *)
  assert_type_errors
    {|
      class Call:
        def __call__(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class Call(typing.Generic[T]):
        def __call__(self) -> T: ...
      def foo(call: Call[int]) -> int:
        return call()
    |}
    [];

  assert_type_errors
    {|
      import typing
      class Call:
        def __call__(self: typing.Callable[[int], str], x: int) -> str: ...
      def foo(call: Call) -> None:
        reveal_type(call())
    |}
    ["Revealed type [-1]: Revealed type for `call()` is `str`."];

  assert_type_errors
    {|
      import typing
      class Call:
        def __call__(self: typing.Callable[[int], str], x: int) -> int: ...
      def foo(call: Call) -> None:
        reveal_type(call())
    |}
    [
      "Invalid method signature [47]: `typing.Callable[[int], str]` cannot be the type of `self`.";
      "Revealed type [-1]: Revealed type for `call()` is `int`.";
    ];

  (* We handle subclassing. *)
  assert_type_errors
    {|
      import typing
      class BaseClass:
        def __call__(self, val: typing.Optional[str] = None) -> "BaseClass":
          ...
      class SubClass(BaseClass):
        pass
      def f(sc: SubClass) -> None:
        sc('foo')
    |}
    [];
  assert_type_errors
    {|
      class Call:
        def not_call(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    ["Call error [29]: `Call` is not a function."];
  assert_default_type_errors {|
      def foo(call) -> int:
        return call()
    |} [];

  (* Test for terminating fixpoint *)
  assert_type_errors
    {|
      class Call:
        def not_call(self) -> int: ...
      def foo(x: int, call: Call) -> int:
        for x in range(0, 7):
          call()
        return 7
    |}
    ["Call error [29]: `Call` is not a function."];
  assert_type_errors
    {|
      import unittest.mock

      class patch:
        def __call__(self) -> int: ...

      unittest.mock.patch: patch = ...

      def foo() -> None:
        unittest.mock.patch()
        unittest.mock.patch()  # subequent calls should not modify annotation map
    |}
    [
      "Illegal annotation target [35]: Target `unittest.mock.patch` cannot be annotated.";
      "Undefined attribute [16]: Module `unittest.mock` has no attribute `patch`.";
      "Undefined attribute [16]: Module `unittest.mock` has no attribute `patch`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [
      "Incompatible return type [7]: Expected `None` but got "
      ^ "`typing.Callable(Foo.bar)[[Named(self, Foo), Named(x, int)], str]`.";
    ];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [
      "Incompatible return type [7]: Expected `None` but got "
      ^ "`BoundMethod[typing.Callable(Foo.bar)[[Named(self, Type[Foo]), Named(x, int)], str], \
         Type[Foo]]`.";
    ];
  assert_type_errors
    {|
      class Call:
        def __call__(self, x: int) -> int: ...
      def foo(call: Call) -> int:
        return call("")
    |}
    [
      "Incompatible parameter type [6]: In call `Call.__call__`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];

  (* TODO(T54644856): Allow generator to initialize tuple so that this test does not expect any
     errors. *)
  assert_type_errors
    {|
      import typing
      def foo_try_with_tuple(obj: typing.Tuple[int, ...]) -> None:
        type(obj)(v for v in obj)
    |}
    [
      "Incompatible parameter type [6]: In call `tuple.__init__`, for 1st positional only \
       parameter expected `List[Variable[_T_co](covariant)]` but got `Generator[int, None, None]`.";
    ];

  (* TODO(T54644856): Allow generator to initialize tuple so that this test does not expect any
     errors. *)
  assert_type_errors
    {|
      def foo_try_with_tuple() -> None:
        tuple(v for v in (1, 2, 3))
    |}
    [
      "Incompatible parameter type [6]: In call `tuple.__init__`, for 1st positional only \
       parameter expected `List[Variable[_T_co](covariant)]` but got `Generator[int, None, None]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo_try_with_list(obj: typing.List[int]) -> None:
        type(obj)(v for v in obj)
    |}
    [];

  (* Calling is a special method access of `__call__`, thus calling Type[X] prefers a `__call__` in
     a metaclass over both the constructor and a `__call__` on X *)
  assert_type_errors
    {|
      from typing import Type

      class Metaclass(type):
        def __call__(self, x: int) -> str:
          return "A"

      class C(metaclass=Metaclass):
        def __call__(self, y: str) -> bool:
          return True

      def f() -> None:
        x = C(1)
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `str`."];
  assert_type_errors
    {|
      from typing import Callable
      def foo() -> None:
        q = Callable[[int], str]
        reveal_type(q)
        f = q(1)
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `q` is `typing.Type[typing.Callable[..., str]]`.";
      "Too many arguments [19]: Call `object.__init__` expects 0 positional arguments, 1 was \
       provided.";
      "Revealed type [-1]: Revealed type for `f` is `typing.Callable[..., str]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable
      class C:
        def __init__(self, x: int) -> None:
          pass

      f: Callable[[int], C] = C
    |}
    [];
  assert_type_errors
    {|
      from typing import Callable
      class C:
        def __init__(self, x: int) -> None:
          pass
        def __call__(self, x: str) -> int:
          return 42

      f: Callable[[C, str], int] = C
    |}
    [];
  assert_type_errors
    {|
      from typing import Callable
      class C:
        def __init__(self, x: int) -> None:
          pass
        def __call__(self, x: str) -> int:
          return 42

      f: Callable[[int], C] = C
    |}
    [
      "Incompatible variable type [9]: f is declared to have type `typing.Callable[[int], C]` but \
       is used as type `Type[C]`.";
    ];
  assert_default_type_errors
    {|
      from typing import Any

      class C:
        __call__: Any = 1

      def foo(c: C) -> None:
        x = c()
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  assert_default_type_errors
    {|
      from typing import Any

      class C:
        __init__: Any = 1

      def foo() -> None:
        x = C()
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  assert_default_type_errors
    {|
      from typing import Any

      def unannotated_decorator(x):
        return x

      class C:
        @unannotated_decorator
        def __init__(self) -> None:
          pass

      def foo() -> None:
        x = C()
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  ()


let test_check_explicit_method_call context =
  assert_type_errors
    ~context
    {|
      class Class:
        def method(self, i: int) -> None:
          pass
      Class.method(object(), 1)
    |}
    [
      "Incompatible parameter type [6]: In call `Class.method`, for 1st positional only parameter \
       expected `Class` but got `object`.";
    ]


let test_check_self context =
  let assert_type_errors = assert_type_errors ~context in
  (* Self parameter is typed. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return 1
        def bar(self) -> str:
          return self.foo()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Other:
          pass

      class Some:
          def one(self) -> None:
              self.two()

          def two(self: Other) -> None:
              pass
    |}
    ["Invalid method signature [47]: `Other` cannot be the type of `self`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class C:
        def f(self: T, x: int) -> T:
          return self
      class Subclass(C):
        pass
      def f() -> C:
        a = Subclass()
        b = a.f
        return b(1)
      def f() -> Subclass:
        a = Subclass()
        b = a.f
        return b(1)
    |}
    [];

  (* Make sure the SelfType pattern works *)
  assert_type_errors
    {|
      import typing
      TSelf = typing.TypeVar('TSelf', bound="C")
      class C:
        def inner(self, x: int) -> None:
          pass
        def verbose(self: TSelf, x: int) -> TSelf:
          self.inner(x)
          return self
      SubTSelf = typing.TypeVar('SubTSelf', bound="Subclass")
      class Subclass(C):
        def subinner(self, x:str) -> None:
          pass
        def interface(self: SubTSelf, x: str) -> SubTSelf:
          self.inner(7)
          self.subinner(x)
          return self
      class SubSubclass(Subclass): pass
      def f() -> SubSubclass:
        return SubSubclass().verbose(7).interface("A")
      def g() -> SubSubclass:
        return SubSubclass().interface("A").verbose(7)
    |}
    [];

  (* Make sure the SelfType pattern works for generics *)
  assert_type_errors
    {|
      import typing
      TSelf = typing.TypeVar('TSelf', bound="C")
      TG = typing.TypeVar('TG')
      class C:
        def inner(self, x: int) -> None:
          pass
        def verbose(self: TSelf, x: int) -> TSelf:
          self.inner(x)
          return self
      class G(C, typing.Generic[TG]): pass
      def foo(x: G[int], y: G[str]) -> typing.Tuple[G[int], G[str]]:
        return (x.verbose(1), y.verbose(1))
    |}
    [];
  assert_type_errors
    {|
      import typing
      TG = typing.TypeVar('TG')
      TSelf = typing.TypeVar('TSelf', bound="G")
      class G(typing.Generic[TG]):
        def inner(self, x: int) -> None:
          pass
        def verbose(self: TSelf, x: int) -> TSelf:
          self.inner(x)
          return self
      class C(G[TG]):
        def outer(self) -> TG: ...
      def foo(x: C[int]) -> None:
        reveal_type(x.verbose(1).outer())
    |}
    [
      "Invalid type parameters [24]: Generic type `G` expects 1 type parameter.";
      "Revealed type [-1]: Revealed type for `x.verbose(1).outer()` is `int`.";
    ];
  assert_type_errors
    {|
      import typing
      TSelf = typing.TypeVar('TSelf', bound="C")
      class C:
        flip: bool = True
        def flipflop(self: TSelf, other: TSelf) -> TSelf:
          self.flip = not self.flip
          if self.flip:
           return other
          else:
           return self
      class A(C):
        pass
      class B(C):
        pass
      def foo() -> None:
        x = A().flipflop(A())
        reveal_type(x)
        x = B().flipflop(B())
        reveal_type(x)
        x = A().flipflop(B())
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `A`.";
      "Revealed type [-1]: Revealed type for `x` is `B`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[A, B]`.";
    ];
  ()


let test_check_meta_self context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_default_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      S = typing.TypeVar('S')
      class C(typing.Generic[T]): pass
      def foo(input: typing.Any) -> None:
        typing.cast(C[int], input)
      class D(typing.Generic[T, S]): pass
      def foo(input: typing.Any) -> None:
        typing.cast(D[int, float], input)
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> C:
        return C.__construct__()
      def boo() -> Subclass:
        return Subclass.__construct__()
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo() -> C:
        return Subclass.__construct__()
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> Subclass:
        return C.__construct__()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class C:
        def f(self: T) -> T:
          ...
      class Subclass(C):
        ...
      def foo(s: Subclass) -> Subclass:
        to_call = s.f
        return to_call()
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      class C:
        def f(self: T) -> T:
          ...
      class Subclass(C):
        ...
      def foo(c: C)-> Subclass:
        to_call = c.f
        return to_call()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];
  assert_type_errors
    {|
      import typing
      class Foo:
        def foo(self) -> typing.Type[Foo]:
          return type(self)
        def bar(self) -> typing.Type[int]:
          return type(1)
    |}
    [];
  assert_type_errors
    {|
      import typing
      class Foo:
        ATTRIBUTE: typing.ClassVar[int] = 1
        def foo(self) -> int:
          return type(self).ATTRIBUTE
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(t: T) -> str:
        return type(t).__name__
    |}
    [];
  assert_type_errors {|
      def foo(x: int) -> str:
        return type(x).__name__
    |} [];
  assert_type_errors
    {|
      class C:
        pass
      R = C
      def foo() -> C:
        return R()
    |}
    []


let test_check_override_decorator context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
     from pyre_extensions import override

     class Foo:
       def different_method(self, input: int) -> int:
         return input

     class Bar(Foo):
       @override
       def foo(self, input:int) -> int:
         return input
    |}
    [
      "Invalid override [40]: `test.Bar.foo` is decorated with @override, but no method of the \
       same name exists in superclasses of `Bar`.";
    ];
  assert_type_errors
    {|
     from pyre_extensions import override

     class Foo:
       @staticmethod
       def same_method(input: int) -> int:
         return input

     class Bar(Foo):
       @override
       def same_method(input: int) -> int:
         return input + 1
  |}
    [
      "Invalid override [40]: Non-static method `test.Bar.same_method` cannot override a static \
       method defined in `Foo`.";
    ];
  assert_type_errors
    {|
     from pyre_extensions import override

     class Foo:
       @classmethod
       def same_method(cls, input: int) -> int:
         return input

     class Bar(Foo):
       @override
       def same_method(cls, input: int) -> int:
         return input + 1
  |}
    [];
  assert_type_errors
    {|
     from pyre_extensions import override

     @override
     def foo(input: int) -> int:
       return input
    |}
    [
      "Invalid override [40]: `test.foo` is illegally decorated with @override: @override may only \
       be applied to methods, but this element is not a method.";
    ];
  assert_type_errors
    {|
      from pyre_extensions import override

      def foo_outer(input: int) -> int:
        @override
        def foo_inner(input: int) -> int:
          return input

        return foo_inner(input)

     |}
    [
      "Invalid override [40]: `foo_inner` is illegally decorated with @override: @override may \
       only be applied to methods, but this element is not a method.";
    ];
  assert_type_errors
    {|
     from pyre_extensions import override

     class Foo1:
       def foo(self, input: int) -> int:
         return input

     class Foo2(Foo1): ...

     class Bar(Foo2):
       @override
       def foo(self, input: int) -> int:
         return 0
    |}
    [];
  assert_type_errors
    {|
     from pyre_extensions import override

     class Foo:
       def foo(self, input: int) -> str:
          ...

     @override
     class Bar(Foo):
       def foo(self, input: int) -> str:
         return "Bar: " + str(input)
    |}
    [ (* TODO: See T123628048. This should return an error, but isn't at the moment. Decorator logic
         needs to be checked. *) ];
  assert_type_errors
    {|
     from pyre_extensions import override

     class Foo:
       foo: int = 3

     class Bar(Foo):
       @override
       foo: str = "hello world"
    |}
    ["Parsing failure [404]: invalid syntax"];
  ()


let test_check_static context =
  let assert_type_errors = assert_type_errors ~context in
  (* No self parameter in static method. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def bar(input: str) -> str:
          return input.lower()

      class Bar:
        @classmethod
        def bar(cls, input: str) -> str:
          return input.lower()

        def baz(self) -> None:
          self.bar("")
    |}
    [];

  (* Static method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    [
      "Incompatible parameter type [6]: In call `Foo.foo`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

        def bar(self) -> None:
          self.foo('asdf')

    |}
    [
      "Incompatible parameter type [6]: In call `Foo.foo`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];

  (* Static methods throw override errors *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return 1

      class Bar(Foo):
        @staticmethod
        def foo(input: str) -> int:
          return 1
    |}
    [
      "Inconsistent override [14]: `test.Bar.foo` overrides method defined in `Foo` inconsistently. "
      ^ "Parameter of type `str` is not a supertype of the overridden parameter `int`.";
    ];

  (* Class method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls, input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    [
      "Incompatible parameter type [6]: In call `Foo.foo`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        @classmethod
        def foo(cls) -> typing.Type[Foo]:
          return cls
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.classmethod('1234')
    |}
    [
      "Incompatible parameter type [6]: In call `Foo.classmethod`, for 1st positional only \
       parameter expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def staticmethod(i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.staticmethod('1234')
    |}
    [
      "Incompatible parameter type [6]: In call `Foo.staticmethod`, for 1st positional only \
       parameter expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def instancemethod(self, i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.instancemethod(Foo(), '1234')
    |}
    [
      "Incompatible parameter type [6]: In call `Foo.instancemethod`, for 2nd positional only \
       parameter expected `int` but got `str`.";
    ];

  (* Special classmethods are treated properly without a decorator. *)
  assert_type_errors
    {|
      import typing
      class Foo:
        def __init_subclass__(cls) -> None:
          return
        def __new__(cls) -> typing.Type[Foo]:
          return cls
        def __class_getitem__(cls, key: int) -> typing.Type[Foo]:
          return cls
    |}
    [];
  assert_type_errors
    {|
      import typing
      class Foo:
        def __new__(cls) -> typing.Type[Foo]:
          return cls

        def test(self) -> typing.Type[Foo]:
          return self.__class__.__new__(self.__class__)
    |}
    []


let test_check_setitem context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Dict[str, int]) -> None:
        x["foo"] = "bar"
    |}
    [
      "Incompatible parameter type [6]: In call `dict.__setitem__`, for 2nd positional only \
       parameter expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      class A:
        pass
      def foo(x: typing.Dict[str, int], y: A) -> None:
        x["foo"] = y["bar"] = "baz"
    |}
    [
      "Undefined attribute [16]: `A` has no attribute `__setitem__`.";
      "Incompatible parameter type [6]: In call `dict.__setitem__`, for 2nd positional only \
       parameter expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Dict[str, typing.Dict[str, int]]) -> None:
        x["foo"]["bar"] = "baz"
    |}
    [
      "Incompatible parameter type [6]: In call `dict.__setitem__`, for 2nd positional only \
       parameter expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Dict[str, int]) -> None:
        x[7] = 7
    |}
    [
      "Incompatible parameter type [6]: In call `dict.__setitem__`, for 1st positional only \
       parameter expected `str` but got `int`.";
    ]


let test_check_in context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class WeirdContains:
        def __contains__(self, x: int) -> int:
          ...
      reveal_type(1 in WeirdContains())
    |}
    ["Revealed type [-1]: Revealed type for `1 in test.WeirdContains()` is `int`."];
  assert_type_errors
    {|
      import typing
      class WeirdIterator:
        def __eq__(self, other: object) -> str:
          ...
        def __iter__(self) -> typing.Iterator[WeirdIterator]:
          ...
      reveal_type(1 in WeirdIterator())
    |}
    ["Revealed type [-1]: Revealed type for `1 in test.WeirdIterator()` is `str`."];
  assert_type_errors
    {|
      import typing
      class WeirdEqual:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class WeirdGetItem:
        def __getitem__(self, x: int) -> WeirdEqual:
          ...
      reveal_type(1 in WeirdGetItem())
    |}
    ["Revealed type [-1]: Revealed type for `1 in test.WeirdGetItem()` is `typing.List[int]`."];
  assert_type_errors
    {|
      import typing
      class Equal:
        def __eq__(self, other: object) -> str:
          ...
      class Multiple:
        def __iter__(self, x: int) -> typing.Iterator[Equal]:
          ...
        def __contains__(self, a: object) -> bool:
          ...
      reveal_type(1 in Multiple())
    |}
    ["Revealed type [-1]: Revealed type for `1 in test.Multiple()` is `bool`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> str:
          ...
      class Multiple:
        def __getitem__(self, x: int) -> Equal:
          ...
        def __contains__(self, a: object) -> int:
          ...
      reveal_type(1 in Multiple())
    |}
    ["Revealed type [-1]: Revealed type for `1 in test.Multiple()` is `int`."];
  assert_type_errors
    {|
      import typing
      class Equal:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class GetItemA:
        def __getitem__(self, x: int) -> Equal:
          ...
      class GetItemB:
        def __getitem__(self, x: int) -> Equal:
          ...
      def foo(a: typing.Union[GetItemA, GetItemB]) -> None:
        5 in a
    |}
    [];

  (* Unions of classes and `in`. *)
  assert_type_errors
    {|
      import typing
      class UsesContainsStr:
        def __contains__(self, o: object) -> str:
          ...

      class UsesContainsInt:
        def __contains__(self, o: object) -> int:
          ...

      def foo(a: typing.Union[UsesContainsInt, UsesContainsStr]) -> None:
        reveal_type(5 in a)
    |}
    ["Revealed type [-1]: Revealed type for `5 in a` is `typing.Union[int, str]`."];
  assert_type_errors
    {|
      from typing import TypeVar, Generic, Union
      T = TypeVar("T")
      class Equal(Generic[T]):
        def __eq__(self, other: object) -> T:
          ...

      class UsesContainsStr:
        def __contains__(self, o: object) -> str:
          ...

      class WeirdIterator:
        def __iter__(self) -> WeirdIterator:
          ...

        def __next__(self) -> Equal[int]:
          ...

      def foo(a: Union[WeirdIterator, UsesContainsStr]) -> None:
        reveal_type(5 in a)
    |}
    ["Revealed type [-1]: Revealed type for `5 in a` is `Union[int, str]`."]


let test_check_enter context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> str:
          return ''

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)
    |}
    [];
  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> int:
          return 5

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)

    |}
    [
      "Incompatible parameter type [6]: In call `expect_string`, for 1st positional only parameter \
       expected `str` but got `int`.";
    ]


let test_check_private_member_access context =
  assert_type_errors
    ~context
    {|
      class Base:
        def __init__(self) -> None:
          self.__private = True
          self._not_so_private = True
        def method(self) -> bool:
          return self.__private
      class Child(Base):
        def __init__(self) -> None:
          self.__child_private = False
        def method(self) -> bool:
          return self._not_so_private
        def method2(self) -> bool:
          return self.__child_private
    |}
    [];
  assert_type_errors
    ~context
    {|
      class Base:
        def __init__(self) -> None:
          self.__private = True
      class Child(Base):
        def method(self) -> bool:
          return self.__private
    |}
    [
      "Incompatible return type [7]: Expected `bool` but got `unknown`.";
      "Undefined attribute [16]: `Child` has no attribute `__private`. `__private` looks like a \
       private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      class Base:
        def __private_method(self) -> None:
          return

      class Child(Base):
        def test(self) -> None:
          self.__private_method
    |}
    [
      "Undefined attribute [16]: `Child` has no attribute `__private_method`. `__private_method` \
       looks like a private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      class Foo:
          __private = 1

      def test(foo: Foo) -> None:
          access = foo.__private
          foo.__private = 1
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `__private`. `__private` looks like a \
       private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      import typing
      T = typing.TypeVar("T", bound="Base")

      class Base:
        def __init__(self: T) -> None:
          self.__private = 1
    |}
    [];
  assert_type_errors
    ~context
    {|
    from typing import *

    T = TypeVar("T", bound="Foo")

    class Foo:
        def test(self: T) -> T:
            self.derp
            self.__herp
            return self

        def __herp(self, value: bool) -> None:
            return

        def derp(self, value: bool) -> None:
            return
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class Base:
        def __init__(self) -> None:
          self.__private = True
      class Child(Base):
        def __init__(self) -> None:
          self.y = 1
      def foo(union: typing.Union[Base, Child], child: Child, base: Base) -> None:
        x = union.__private
        y = base.__private
        z = child.__private
    |}
    [
      "Undefined attribute [16]: Item `Base` of `typing.Union[Base, Child]` has no attribute \
       `__private`. `__private` looks like a private attribute, which is not accessible from \
       outside its parent class.";
      "Undefined attribute [16]: `Base` has no attribute `__private`. `__private` looks like a \
       private attribute, which is not accessible from outside its parent class.";
      "Undefined attribute [16]: `Child` has no attribute `__private`. `__private` looks like a \
       private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      class Base:
        def __init__(self) -> None:
          self.__private = True
      def foo(x: Base) -> bool:
        return x.__private
    |}
    [
      "Incompatible return type [7]: Expected `bool` but got `unknown`.";
      "Undefined attribute [16]: `Base` has no attribute `__private`. `__private` looks like a \
       private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      class Base:
        def __init__(self) -> None:
          self.__private = True
      class Child(Base):
        def __init__(self) -> None:
          self.x = 1
      def foo() -> bool:
        return [Base(), Child()][1].__private
    |}
    [
      "Incompatible return type [7]: Expected `bool` but got `unknown`.";
      "Undefined attribute [16]: `Base` has no attribute `__private`. `__private` looks like a \
       private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      class Base:
        def __private_method(self) -> None:
          pass
        def base_public_method(self) -> None:
          self.__private_method()
      class Child(Base):
        def public_method(self) -> None:
          self.__private_method()
      def foo() -> None:
        return [Base(), Child()][1].__private_method()
    |}
    [
      "Undefined attribute [16]: `Child` has no attribute `__private_method`. `__private_method` \
       looks like a private attribute, which is not accessible from outside its parent class.";
      "Undefined attribute [16]: `Base` has no attribute `__private_method`. `__private_method` \
       looks like a private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      import typing
      T = typing.TypeVar('T')
      class GenericBase(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          self.__private: T
          self.__private = x
        def method(self) -> T:
          return self.__private
      class GenericChild(GenericBase[T]):
        def method(self) -> T:
          return self.__private
      def foo(x: GenericBase[T]) -> T:
        return x.__private
    |}
    [
      "Incompatible return type [7]: Expected `Variable[T]` but got `unknown`.";
      "Undefined attribute [16]: `GenericChild` has no attribute `__private`. `__private` looks \
       like a private attribute, which is not accessible from outside its parent class.";
      "Incompatible return type [7]: Expected `Variable[T]` but got `unknown`.";
      "Undefined attribute [16]: `GenericBase` has no attribute `__private`. `__private` looks \
       like a private attribute, which is not accessible from outside its parent class.";
    ];
  assert_type_errors
    ~context
    {|
      class ExampleClass:
        @staticmethod
        def __private_static_method() -> float:
          return 1.0

        def foo(self) -> None:
          ExampleClass.__private_static_method()
          self.__class__.__private_static_method()
          self.__private_static_method()

        @classmethod
        def bar(cls) -> None:
          cls.__private_static_method()
          cls.__class__.__private_static_method()
    |}
    [];
  assert_type_errors
    ~context
    {|
      class ExampleClass:
        @staticmethod
        def __private_static_method() -> float:
          return 1.0

      class Child(ExampleClass):
        def foo1(self) -> None:
          ExampleClass.__private_static_method()

        def foo2(self) -> None:
          self.__class__.__private_static_method()

        def foo3(self) -> None:
          self.__private_static_method()

        @classmethod
        def foo4(cls) -> None:
          cls.__private_static_method()

        @classmethod
        def foo5(cls) -> None:
          cls.__class__.__private_static_method()

      class NonChild:
        def bar(self) -> None:
          ExampleClass.__private_static_method()

    |}
    [
      "Undefined attribute [16]: `ExampleClass` has no attribute `_Child__private_static_method`.";
      "Undefined attribute [16]: `Child` has no attribute `__private_static_method`. \
       `__private_static_method` looks like a private attribute, which is not accessible from \
       outside its parent class.";
      "Undefined attribute [16]: `Child` has no attribute `__private_static_method`. \
       `__private_static_method` looks like a private attribute, which is not accessible from \
       outside its parent class.";
      "Undefined attribute [16]: `Child` has no attribute `__private_static_method`. \
       `__private_static_method` looks like a private attribute, which is not accessible from \
       outside its parent class.";
      "Undefined attribute [16]: `Child` has no attribute `__private_static_method`. \
       `__private_static_method` looks like a private attribute, which is not accessible from \
       outside its parent class.";
      "Undefined attribute [16]: `ExampleClass` has no attribute \
       `_NonChild__private_static_method`.";
    ];
  ()


let test_enforce_dunder_params context =
  assert_type_errors
    ~context
    {|
      def foo(__f: str) -> int:
        return 1

      def bar() -> None:
        foo("A")
    |}
    [];
  assert_type_errors
    ~context
    {|
      def foo(__f: str) -> int:
        return 1

      def bar() -> None:
        foo(__f="A")
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `__f` to call `foo`."];
  assert_type_errors
    ~context
    {|
      def foo(__f__: str) -> int:
        return 1

      def bar() -> None:
        foo(__f__="A")
    |}
    [];
  ()


let test_fixpoint_threshold context =
  assert_type_errors
    ~context
    {|
      def foo() -> bool: ...

      def bar() -> None:
          l = [(0, 1)]
          for x in range(1000000):
              l = [(0, l*x)]
          reveal_type(l)
    |}
    [
      "Analysis failure [30]: Pyre gave up inferring types for some variables because function \
       `test.bar` was too complex.";
      "Unsupported operand [58]: `*` is not supported for operand types `unknown` and `int`.";
      "Revealed type [-1]: Revealed type for `l` is `unknown`.";
    ];
  assert_type_errors
    ~context
    {|
      from typing import Set
      def foo() -> None:
        for _ in range(10000):
          s: "Set[Missing]" = set()
          reveal_type(s)
    |}
    [
      "Undefined import [21]: Could not find a name `Set` defined in module `typing`.";
      "Undefined or invalid type [11]: Annotation `Missing` is not defined as a type.";
      "Revealed type [-1]: Revealed type for `s` is `Set[Variable[_T]]`.";
    ];
  ()


let () =
  "method"
  >::: [
         "check_method_returns" >:: test_check_method_returns;
         "check_inverse_operator" >:: test_check_inverse_operator;
         "check_method_parameters" >:: test_check_method_parameters;
         "check_private_member_access" >:: test_check_private_member_access;
         "check_abstract_methods" >:: test_check_abstract_methods;
         "check_behavioral_subtyping__strengthened_postcondition"
         >:: test_check_behavioral_subtyping__strengthened_postcondition;
         "check_behavioral_subtyping__weakened_precondition"
         >:: test_check_behavioral_subtyping__weakened_precondition;
         "check_behavioral_subtyping__overloads" >:: test_check_behavioral_subtyping__overloads;
         "check_behavioral_subtyping__special_cases"
         >:: test_check_behavioral_subtyping__special_cases;
         "check_behavioral_subtyping__attribute_kinds"
         >:: test_check_behavioral_subtyping__attribute_kinds;
         "check_nested_class_inheritance" >:: test_check_nested_class_inheritance;
         "check_method_resolution" >:: test_check_method_resolution;
         "check_callables" >:: test_check_callables;
         "check_callable_protocols" >:: test_check_callable_protocols;
         "check_explicit_method_call" >:: test_check_explicit_method_call;
         "check_self" >:: test_check_self;
         "check_meta_self" >:: test_check_meta_self;
         "check_setitem" >:: test_check_setitem;
         "check_override_decorator" >:: test_check_override_decorator;
         "check_static" >:: test_check_static;
         "check_in" >:: test_check_in;
         "check_enter" >:: test_check_enter;
         "enforce_dunder_params" >:: test_enforce_dunder_params;
         "fixpoint_threshold" >:: test_fixpoint_threshold;
       ]
  |> Test.run
