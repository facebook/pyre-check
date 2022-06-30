(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test
open OUnit2
open IntegrationTest

let test_check_attributes context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return self.bar
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int = 1
        def foo(self) -> int:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      import typing
      class Bar: ...
      class Foo(Bar):
        bar: typing.Optional[int] = None
        def foo(self) -> typing.Optional[int]:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo(Bar):
        def foo(self) -> int:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.bar: int = None
        def f(self) -> str:
            return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` "
      ^ "has type `int` but is used as type `None`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];
  assert_type_errors {|
      a = str
      b = 1
    |} [];
  assert_type_errors
    {|
      class Foo:
        a = None
        def __init__(self) -> None:
          self.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type \
       `typing.Optional[int]` but no type is specified.";
    ];
  assert_type_errors
    ~show_error_traces:true
    {|
      class Bar:
        bar: int = 1
      def foo() -> int:
        return Bar.bar
    |}
    [];
  assert_type_errors
    ~show_error_traces:true
    {|
      class Bar:
        bar: int = 1
      def foo() -> int:
        x = Bar()
        return x.baz
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`. Type `int` expected on "
      ^ "line 6, specified on line 4.";
      "Undefined attribute [16]: `Bar` has no attribute `baz`.";
    ];
  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo:
        def foo(self, other: Bar) -> int:
          return other.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Any
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have type \
       `typing.Any` but is never initialized.";
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` must have a type other \
       than `Any`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int
        def __init__(self, baz: int) -> None:
          self.baz = baz

    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have type `int` \
       but is never initialized.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Dict[str, typing.Any] = {}
        baz: typing.Dict[typing.Any, typing.Any] = {}
    |}
    [
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` must have a type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar = {}
        baz = {"key": "string"}
        def foo(self) -> None:
          self.bar["key"] = 1
          self.bar["key"] = "string"
          self.baz["key"] = 1
          self.baz["key"] = "string"
    |}
    [
      "Incomplete type [37]: Type `typing.Dict[Variable[_KT], Variable[_VT]]` inferred for \
       `test.Foo.bar` is incomplete, add an explicit annotation.";
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has no type specified.";
      "Incompatible parameter type [6]: In call `dict.__setitem__`, for 2nd positional only \
       parameter expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Any
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have type \
       `typing.Any` but is never initialized.";
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` must have a type other \
       than `Any`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int = 1
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` "
      ^ "but is used as type `str`.";
    ];
  assert_type_errors
    {|
          class Foo:
            a: str = ""
          Foo.a = 1
    |}
    [
      "Incompatible attribute type [8]: Attribute `a` declared in class `Foo` has type `str` "
      ^ "but is used as type `int`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int = 1
      def foo(param: Foo) -> int:
        param.bar = 'foo'
        return param.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` "
      ^ "but is used as type `str`.";
    ];
  assert_type_errors
    {|
      bar: int = 1
      def foo() -> int:
        bar = 'foo'
        return bar
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int = 1
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = "foo"
        return foo_obj.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` "
      ^ "but is used as type `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int = 1
      class Bar(Foo):
        def foo(self) -> int:
          self.bar = "foo"
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` "
      ^ "but is used as type `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.a = 3
        def foo(self) -> str:
            return self.a
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Optional[int]
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` "
      ^ "to have type `typing.Optional[int]` but is never initialized.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a and a.bar:
          return a.bar
        return 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` "
      ^ "to have type `typing.Optional[int]` but is never initialized.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a.bar and a:
          return a.bar
        return 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` "
      ^ "to have type `typing.Optional[int]` but is never initialized.";
      "Undefined attribute [16]: Optional type has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `Optional[int]`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar, baz = 1, 2
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        bar, baz = list(range(2))
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` "
      ^ "has type `int` but no type is specified.";
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` "
      ^ "has type `int` but no type is specified.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        def foo(self, bar: typing.Optional[int]) -> int:
          self.baz = bar
          if self.baz is None:
            self.baz = 5
          return self.baz
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `baz`."];

  (* Ensure synthetic attribute accesses don't mask errors on real ones. *)
  assert_strict_type_errors
    {|
      class Foo:
        pass

      def derp(foo: Foo) -> None:
        if not Foo.a:
          pass
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `a`."];

  (* TODO(T25072735): support attribute tests for: class variables, generic annotations *)
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.ClassVar[int] = 1
      def foo() -> int:
        Foo.bar = "foo"
        return Foo.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` "
      ^ "but is used as type `str`.";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      class Foo:
        bar: typing.Generic[_T]
        def foo(self) -> int:
          self.bar = 0
          return self.bar
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have "
      ^ "type `typing.Generic[Variable[_T]]` but is never initialized.";
      "Invalid type variable [34]: The current class isn't generic with respect to the type \
       variable `Variable[_T]`.";
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type "
      ^ "`Generic[Variable[_T]]` but is used as type `int`.";
      "Incompatible return type [7]: Expected `int` but got `Generic[Variable[_T]]`.";
    ];

  (* Static attributes are properly resolved. *)
  assert_type_errors
    {|
      import typing
      class Foo:
        attribute: typing.ClassVar[int] = 1

      def foo() -> str:
        return Foo.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Foo:
        class Bar:
          attribute: int = 1

      def foo() -> str:
        return Foo.Bar().attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      class Foo:
        DERP: typing.ClassVar[str] = "test"

        @staticmethod
        def derp() -> str:
          return Foo.DERP
    |}
    [];

  (* Attributes defined in constructor. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [];
  assert_type_errors
    {|
      from unittest import TestCase
      class Foo(TestCase):
        def setUp(self) -> None:
          self.attribute: int = 1
        def foo(self) -> str:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      from unittest.case import TestCase
      class Foo(TestCase):
        def setUp(self) -> None:
          self.attribute: int = 1
        def foo(self) -> str:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      from unittest.case import TestCase
      class Foo(TestCase):
        x: int
        def setUp(self) -> None:
          self.x = 1
    |}
    [];
  assert_type_errors
    {|
      from unittest.case import TestCase
      class Base():
        def setUp(self) -> None:
          self.x: int = 1
      class Foo(Base, TestCase):
        def foo(self) -> None:
          y = self.x
    |}
    [
      "Undefined attribute [16]: `Base` has no attribute `x`.";
      "Undefined attribute [16]: `Foo` has no attribute `x`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute: int = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [];
  assert_type_errors
    {|
      import unittest

      class TestFoo(unittest.TestCase):
        @classmethod
        def setUpClass(cls) -> None:
          cls.foo: int = 1

        def testFoo(self) -> None:
          reveal_type(self.foo)
    |}
    ["Revealed type [-1]: Revealed type for `self.foo` is `int`."];

  (* Undefined attributes. *)
  assert_strict_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 1
        def foo(self) -> None:
          self.attribute = 2
          self.undefined = 3
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `undefined`."];
  assert_strict_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = 1

      foo = Foo()
      foo.y = 1
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `y`."];
  assert_strict_type_errors
    {|
      from typing import Union
      class Foo:
        def __init__(self) -> None:
          self.x = 1

      def bar(foo_or_int: Union[Foo, int]) -> None:
        print(foo_or_int.x)
    |}
    ["Undefined attribute [16]: Item `int` of `typing.Union[int, Foo]` has no attribute `x`."];

  (* Class implements `__getattr__`. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def __getattr__(self, attribute: object) -> str: ...
        def foo(self) -> int:
          return self.undefined
        def bar(self) -> int:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  (* Attributes of other classes are properly resolved. *)
  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo:
        def foo(self, bar: Bar) -> int:
          return bar.bar
      def foo(bar: Bar) -> int:
        return bar.bar
    |}
    [];

  (* Things that inherit from any have all attributes *)
  assert_strict_type_errors
    {|
      from typing import Any
      from placeholder_stub import StubbedBase
      class Else(StubbedBase):
          def __init__(self, actually_there: int) -> None:
             self.actually_there = actually_there
             super().__init__()
      def main() -> None:
          instance = Else(1)
          a = instance.prop
          reveal_type(a)
          a = Else.class_prop
          reveal_type(a)
          a = instance.actually_there
          reveal_type(a)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `typing.Any`.";
      "Undefined attribute [16]: `Else` has no attribute `class_prop`.";
      "Revealed type [-1]: Revealed type for `a` is `unknown`.";
      "Revealed type [-1]: Revealed type for `a` is `int`.";
    ];

  (* We allow instance attributes to be accessed via class objects. *)
  assert_type_errors {|
      class Foo:
        attribute: int = 1
      Foo.attribute
    |} [];

  (* Check attribute type propagation. *)
  assert_type_errors
    {|
      from builtins import not_annotated
      class Foo:
        attribute: int = 1
        def foo(self) -> None:
          self.attribute = not_annotated()
          a = self.attribute.something
    |}
    [];

  assert_type_errors
    {|
      def returns_string() -> str:
        return ""

      class Foo:
        attribute: int = 1
        def foo(self) -> None:
          self.attribute = returns_string()
          a = self.attribute.something
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type \
       `int` but is used as type `str`.";
      "Undefined attribute [16]: `int` has no attribute `something`.";
    ];

  (* Do not resolve optional attributes to the optional type. *)
  assert_type_errors
    {|
      import typing
      class Foo:
        debug: int = 1
      def foo(f: typing.Optional[Foo]) -> int:
        return f.debug
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Optional type has no attribute `debug`.";
    ];

  assert_type_errors
    {|
      import typing
      class Foo:
        n = None
        l = typing.List
      def g(f: Foo) -> None:
        reveal_type(f.n)
        reveal_type(f.l)
    |}
    [
      "Missing attribute annotation [4]: Attribute `n` of class `Foo` has type `None` but no type \
       is specified.";
      "Missing attribute annotation [4]: Attribute `l` of class `Foo` has type `typing.Type[list]` \
       but no type is specified.";
      "Revealed type [-1]: Revealed type for `f.n` is `unknown`.";
      "Revealed type [-1]: Revealed type for `f.l` is `unknown`.";
    ];

  (* Try resolve to string literal types for __match_args__ *)
  assert_type_errors
    {|
      class Foo:
        __match_args__ = ("x", "y")
        x: int = 0
        y: str = ""

      def g(f: Foo) -> None:
        reveal_type(f.__match_args__)
    |}
    [
      "Revealed type [-1]: Revealed type for `f.__match_args__` is \
       `typing.Tuple[typing_extensions.Literal['x'], typing_extensions.Literal['y']]`.";
    ];
  assert_type_errors
    {|
      class Foo:
        __match_args__ = ("x", "y")
        x: int = 0
        y: str = ""

        def update_match_args(self) -> None:
          self.__match_args__ = ("x",)
    |}
    [
      "Incompatible attribute type [8]: Attribute `__match_args__` declared in class `Foo` has \
       type `Tuple[typing_extensions.Literal['x'], typing_extensions.Literal['y']]` but is used as \
       type `Tuple[typing_extensions.Literal['x']]`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      class Foo:
        __match_args__: Tuple[str]
        x: int = 0
        y: float = 0.

        def __init__(self, use_floats: bool) -> None:
          if use_floats:
            self.__match_args__ = ("y",)
          else:
            self.__match_args__ = ("x",)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        __match_args__ = ["x", "y"]
        x: int = 0
        y: str = ""

        def update_match_args(self) -> None:
          self.__match_args__ = ["x"]
    |}
    [];
  ()


let test_attribute_decorators context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  (* Attributes defined with property decorators. *)
  assert_type_errors
    {|
      class Foo:
        @property
        def prop(self) -> int: ...
      def foo() -> str:
        return Foo().prop
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  (* Attributes defined with getters and setters. *)
  assert_strict_type_errors
    {|
      class Foo:
        _a: int = 1
        def __init__(self) -> None:
          self.a = 1 + 1
        @property
        def a(self) -> int:
          return self._a
        @a.setter
        def a(self, value: int) -> None:
          self._a = value
    |}
    [];
  assert_strict_type_errors
    {|
      class Foo:
        _a: int = 1
        def __init__(self) -> None:
          self.a = 1 + 1
        @property
        def a(self) -> int:
          return self._a
    |}
    ["Invalid assignment [41]: `self.a` cannot be reassigned. It is a read-only property."];
  assert_type_errors
    {|
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, value: int) -> None: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> None:
        foo = Foo()
        foo.x = 1
        foo.x = None
        foo.x = "string"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`"
      ^ " has type `int` but is used as type `None`.";
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`"
      ^ " has type `int` but is used as type `str`.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.Optional[int]
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> None:
        foo = Foo()
        foo.x = 1
        foo.x = None
        foo.x = "string"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type \
       `Optional[int]` but is used as type `str`.";
    ];
  assert_type_errors
    {|
      import typing
      __property__: typing.Any = ...
      x: typing.Optional[int]
      class Foo:
        @__property__
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
        @__property__
        def y(self) -> int: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> int:
        foo = Foo()
        return foo.y
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `__property__` "
      ^ "must be specified as type other than `Any`.";
    ];
  assert_type_errors
    {|
      class Foo:
        @property
        def y(self) -> int: ...
        @property
        def x(self) -> int: ...
        @y.setter
        def x(self, value: int) -> None: ...
    |}
    [
      "Invalid decoration [56]: Invalid property setter `y.setter`: `y` does not match decorated \
       method `x`.";
    ];
  ()


let test_attribute_strict context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  (* Annotations containing `Any` in strict are not permitted. *)
  assert_type_errors
    {|
      import typing
      class Foo:
        bar: typing.Any
        def foo(self) -> int:
          self.bar = 'foo'
          self.bar = 1
          return self.bar
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have type \
       `typing.Any` but is never initialized.";
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` must have a type other \
       than `Any`.";
    ];

  (* Annotations containing aliases to `Any` in strict are permitted. Extra type inference errors
     are thrown in debug that are filtered away in strict. *)
  assert_strict_type_errors
    {|
      import typing
      MyType = typing.Any
      class Foo:
        bar: MyType
        def foo(self) -> int:
          self.bar = 'foo'
          self.bar = 1
          return self.bar
    |}
    [
      "Prohibited any [33]: `MyType` cannot alias to `Any`.";
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have type \
       `typing.Any` but is never initialized.";
    ];

  assert_strict_type_errors
    {|
      class Foo:
        a = None
        def __init__(self) -> None:
          self.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type \
       `typing.Optional[int]` but no type is specified.";
    ];
  assert_strict_type_errors
    {|
      class Foo:
        a = "string"
        def __init__(self) -> None:
          self.a = 1
    |}
    [
      "Incompatible attribute type [8]: Attribute `a` declared in class `Foo` has type `str` "
      ^ "but is used as type `int`.";
    ];
  assert_strict_type_errors
    {|
      import typing
      class Bar:
        def bar(self) -> None:
          pass
      class Foo:
        bar: typing.Optional[Bar] = None
        def foo(self) -> None:
          self.bar.bar()
    |}
    ["Undefined attribute [16]: Optional type has no attribute `bar`."];

  (* Any has all attributes in default mode, but not strict mode. *)
  assert_strict_type_errors
    {|
      import typing
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
  assert_default_type_errors
    {|
      import typing
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    []


let test_check_attribute_initialization context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
        class Foo:
          a: int
        Foo.a = 1
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have "
      ^ "type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
        import typing
        class Foo:
          a: typing.Optional[int]
        Foo.a = 1
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have type \
       `typing.Optional[int]` but is never initialized.";
    ];

  assert_type_errors
    {|
        class Foo:
          def __init__(self) -> None:
            self.a: int
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have type `int` \
       but is never initialized.";
    ];

  (* Test abstract base classes. *)
  assert_type_errors {|
        import abc
        class Foo(abc.ABC):
          x: int
    |} [];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
          def __init__(self) -> None:
            y = 1
    |}
    [];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
        class Bar(Foo):
          def __init__(self) -> None:
            self.y = 1
    |}
    [
      "Uninitialized attribute [13]: Attribute `x` inherited from abstract class `Foo` in class \
       `Bar` to have type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
          y: int
        class Bar(Foo):
          a = 1
    |}
    [
      "Uninitialized attribute [13]: Attribute `x` inherited from abstract class `Foo` in class \
       `Bar` to have type `int` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `y` inherited from abstract class `Foo` in class \
       `Bar` to have type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          a: int
        class Bar(Foo):
          a = 1
        class Baz(Bar):
          pass
    |}
    [];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
          y: int
        class Bar(Foo):
          a = 1
          def __init__(self) -> None:
            b = 2
    |}
    [
      "Uninitialized attribute [13]: Attribute `x` inherited from abstract class `Foo` in class \
       `Bar` to have type `int` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `y` inherited from abstract class `Foo` in class \
       `Bar` to have type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
          y: int
        class Bar(Foo):
          def __init__(self) -> None:
            self.x = 1
            self._y = self.y = 1
    |}
    [];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
        class Bar(Foo):
          @property
          def x(self) -> int:
            return 1
          @x.setter
          def x(self, value: int) -> None:
            self.x = value
    |}
    [];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
        class Bar(Foo):
          @property
          def x(self) -> int:
            return 1
    |}
    [];

  (* Test inheritance of both undefined attribute and definition. *)
  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
        class Bar:
          x = 1
        class Baz(Foo, Bar):
          pass
    |}
    [];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
        class Bar:
          y = 1
        class Baz(Foo, Bar):
          pass
    |}
    [
      "Uninitialized attribute [13]: Attribute `x` inherited from abstract class `Foo` in class \
       `Baz` to have type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
        import abc
        class Foo(abc.ABC):
          x: int
        class Bar:
          @property
          def x(self) -> int:
            return 1
        class Baz(Foo, Bar):
          pass
    |}
    [];

  assert_type_errors
    {|
        import abc

        class Foo(abc.ABC):
            LIMIT: int = NotImplemented

            def is_ok(self, var: int) -> bool:
                return var < self.LIMIT

        class Bar(Foo):
            LIMIT = 2
    |}
    [];

  (* Test instantiation of attributes via `__init_subclass__`. *)
  assert_type_errors
    {|
      from typing import ClassVar
      from abc import ABCMeta

      class AbstractBase(metaclass=ABCMeta):
          foo: ClassVar[int]
          def __init_subclass__(cls, foo: int) -> None:
              cls.foo = foo

      class SubClass(AbstractBase, foo=1):
        pass
    |}
    [];
  (* TODO(T98000466): `__init_subclass__` should not instantiate attributes for the current class
     when it is non abstract, only subclasses. Add an error here. *)
  assert_type_errors
    {|
      from typing import ClassVar

      class NonAbstractBase:
          foo: ClassVar[int]
          def __init_subclass__(cls, foo: int) -> None:
              cls.foo = foo

      class SubClass(NonAbstractBase, foo=1):
        pass
    |}
    [];
  ()


let test_check_missing_attribute context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      class Foo:
        a = unknown
      Foo.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` but no type \
       is specified.";
      "Unbound name [10]: Name `unknown` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a = 1
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a = []
      def test(foo: Foo) -> None:
        reveal_type(foo.a)
    |}
    [
      "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `self.a` is incomplete, \
       add an explicit annotation.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
      (* TODO(T78211867): We should know `foo.a` is a `List`. *)
      "Revealed type [-1]: Revealed type for `foo.a` is `unknown`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, a) -> None:
          self.a = a
    |}
    [
      "Missing parameter annotation [2]: Parameter `a` has no type specified.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, a: int, b: int) -> None:
          self.a = a
          self._b = b
    |}
    [];
  assert_type_errors
    {|
      import typing
      MyType = typing.Any
      class Foo:
        def __init__(self, a: MyType) -> None:
          self.a = a
          self.b: MyType = 1
    |}
    ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
  assert_type_errors
    {|
      class Foo:
        a = unknown
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
      "Unbound name [10]: Name `unknown` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
        import typing
        class Foo:
          a: typing.Any
        Foo.a = 1
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have "
      ^ "type `typing.Any` but is never initialized.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` must have a type other than \
       `Any`.";
    ];
  assert_default_type_errors ~handle:"stub.pyi" {|
        class Foo:
          a: int
    |} [];
  assert_type_errors
    {|
        import typing
        class Foo:
          def __init__(self, a: typing.Any) -> None:
            self.a = a
    |}
    ["Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`."];
  assert_type_errors
    {|
      import typing
      class Foo:
        def __init__(self) -> None:
          self.a: typing.Any
      Foo().a = 1
      def foo() -> bool:
        return Foo().a
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have type \
       `typing.Any` but is never initialized.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` must have a type other than \
       `Any`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, test:bool) -> None:
          if test:
            self.x = 1
          else:
            self.x = "hi"
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type "
      ^ "`typing.Union[int, str]` but no type is specified.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, test: bool, y: str) -> None:
          if test:
            self.x = 1
          else:
            self.x = y
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type "
      ^ "`typing.Union[int, str]` but no type is specified.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        x: typing.Any = 1
        y: typing.List[typing.Any] = [1]
        def __init__(self, test: bool, y: str) -> None:
          self.a: typing.Any = 1
          self.b: typing.List[typing.Any] = [1]
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` "
      ^ "but type `Any` is specified.";
      "Missing attribute annotation [4]: Attribute `y` of class `Foo` must have a type "
      ^ "that does not contain `Any`.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` "
      ^ "but type `Any` is specified.";
      "Missing attribute annotation [4]: Attribute `b` of class `Foo` must have a type "
      ^ "that does not contain `Any`.";
    ];

  (* Ensure we don't filter uninitialized attribute errors. *)
  assert_default_type_errors
    {|
      class Foo:
        a: int
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have type `int` \
       but is never initialized.";
    ];

  (* Don't report in non-debug mode. *)
  assert_default_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a = 1
    |}
    [];
  assert_default_type_errors
    {|
      import typing
      class Foo:
        a: typing.Any
      Foo.a = 1
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have type \
       `typing.Any` but is never initialized.";
    ];
  assert_strict_type_errors
    ~context
    ~update_environment_with:
      [
        {
          handle = "other.pyi";
          source =
            {|
             from typing import Any
             class O:
               x: Any
           |};
        };
      ]
    {|
      from other import O
      class H:
        o: O
        def __init__(self, x: str) -> None:
          self.o = O()
          self.o.x = x
    |}
    [];
  ()


let test_attribute_type_variable_resolution context =
  let assert_type_errors = assert_type_errors ~context in
  (* Check attribute type variable resolution. *)
  assert_type_errors
    {|
    import typing
    _VALUE = typing.TypeVar('_VALUE')
    class Wrapper(typing.Generic[_VALUE]):
      value: _VALUE

    def bar(wrapper: Wrapper[int]) -> int:
      return wrapper.value
  |}
    [
      "Uninitialized attribute [13]: Attribute `value` is declared in class `Wrapper` to have "
      ^ "type `Variable[_VALUE]` but is never initialized.";
    ];
  assert_type_errors
    {|
    import typing
    _VALUE = typing.TypeVar('_VALUE')
    class Wrapper(typing.Generic[_VALUE]):
      value: _VALUE

    class WrapperSubclass(Wrapper[int]):
      pass

    def bar(wrapper: WrapperSubclass) -> int:
      return wrapper.value
  |}
    [
      "Uninitialized attribute [13]: Attribute `value` is declared in class `Wrapper` to have "
      ^ "type `Variable[_VALUE]` but is never initialized.";
    ];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    class ReturnSelf(typing.Generic[_T]):
      def f(self) -> ReturnSelf[_T]:
        return self
  |}
    [];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    class ReturnClass(typing.Generic[_T]):
      @classmethod
      def f(cls) -> ReturnClass[_T]:
        return cls
  |}
    [
      "Incompatible return type [7]: Expected `ReturnClass[Variable[_T]]` but got \
       `Type[ReturnClass[Variable[_T]]]`.";
    ];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    class Class:
      @property
      def property(self: _T) -> typing.Sequence[_T]: ...
    def foo(c: Class) -> typing.Sequence[Class]:
      return c.property
  |}
    [];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    class Class(typing.Generic[_T]):
      @property
      def property(self) -> _T: ...
    def foo(c: Class[int]) -> int:
      return c.property
  |}
    [];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    class A:
      @property
      def property(self: _T) -> _T: ...
    class B(A):
      def foo(self) -> None:
        reveal_type(self.property)
  |}
    ["Revealed type [-1]: Revealed type for `self.property` is `B` (final)."];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    _U = typing.TypeVar('_U')
    class A:
      @property
      def property(self: typing.Callable[[_T], _U]) -> _T: ...
      def __call__(self, x: int) -> str: ...
    def foo(a : A) -> None:
      reveal_type(a.property)
  |}
    ["Revealed type [-1]: Revealed type for `a.property` is `int` (final)."];
  assert_type_errors
    {|
    import typing
    _T = typing.TypeVar('_T')
    class A:
      @property
      def property(self: typing.Callable[..., _T]) -> _T: ...
      def __call__(self, x: int) -> str: ...
    def foo(a : A) -> None:
      reveal_type(a.property)
  |}
    ["Revealed type [-1]: Revealed type for `a.property` is `str` (final)."];
  assert_type_errors
    {|
    import typing
    import pyre_extensions
    _T = typing.TypeVar('_T')
    _TParams = pyre_extensions.ParameterSpecification('_TParams')
    class A:
      @property
      def property(self: typing.Callable[_TParams, _T]) -> typing.Callable[_TParams, bool]: ...
      def __call__(self, __x: int) -> str: ...
    def foo(a : A) -> None:
      reveal_type(a.property)
  |}
    [
      "Revealed type [-1]: Revealed type for `a.property` is `typing.Callable[[int], bool]` (final).";
    ];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      _U = typing.TypeVar('_U')
      class A:
        @property
        def x(self: typing.Callable[[_T], _U]) -> _T: ...
        @x.setter
        def x(self: typing.Callable[[_U], _T], value: _T) -> None: ...
        def __call__(self, x: int) -> str: ...
      def foo(a : A) -> int:
        return a.x
      def bar(a : A) -> None:
        a.x = "string"
    |}
    [];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      _U = typing.TypeVar('_U')
      class A:
        @property
        def x(self: typing.Callable[[_T], _U]) -> _T: ...
        @x.setter
        def x(self: typing.Callable[[_U], _T], value: _T) -> None: ...
        def __call__(self, x: int) -> str: ...
      def foo(a : A) -> str:
        return a.x
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      _U = typing.TypeVar('_U')
      class A:
        @property
        def x(self: typing.Callable[[_T], _U]) -> _T: ...
        @x.setter
        def x(self: typing.Callable[[_U], _T], value: _T) -> None: ...
        def __call__(self, x: int) -> str: ...
      def foo(a : A) -> None:
        a.x = 1
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `A` has type `str` but is \
       used as type `int`.";
    ];
  assert_type_errors
    {|
    import typing
    from abc import ABCMeta
    _T = typing.TypeVar('_T')
    class FooMeta(ABCMeta):
      @property
      def __members__(cls: typing.Type[_T]) -> _T: ...
    class Foo(metaclass=FooMeta): ...
    reveal_type(Foo.__members__)
  |}
    ["Revealed type [-1]: Revealed type for `test.Foo.__members__` is `Foo` (final)."];
  assert_type_errors
    {|
    import typing
    T = typing.TypeVar('T')
    def f(t: typing.Type[T]) -> None:
      a = t()
  |}
    [];
  assert_type_errors
    {|
    import typing
    T = typing.TypeVar('T', bound=int)
    def f(t: typing.Type[T]) -> None:
      a = t()
  |}
    [];
  assert_type_errors
    {|
    import typing
    T = typing.TypeVar('T', int)
    def f(t: typing.Type[T]) -> None:
      a = t()
  |}
    [
      "Invalid type [31]: TypeVar can't have a single explicit constraint. Did you mean `bound=int`?";
    ];
  ()


let test_check_getattr context =
  let assert_test_getattr source =
    let getattr_stub =
      {
        handle = "has_getattr.pyi";
        source =
          {|
            from typing import Any
            def __getattr__(name: str) -> Any: ...
          |};
      }
    in
    let getattr_stub_str =
      {
        handle = "has_getattr_str.pyi";
        source = {|
            def __getattr__(name: str) -> str: ...
          |};
      }
    in
    let getattr_stub_untyped =
      {
        handle = "has_getattr_untyped.pyi";
        source = {|
            def __getattr__(name): ...
          |};
      }
    in
    let getattr_stub_invalid_arity =
      {
        handle = "has_getattr_invalid_arity.pyi";
        source = {|
            def __getattr__(x: int, y: str) -> str: ...
          |};
      }
    in
    let getattr_stub_not_callable =
      {
        handle = "has_getattr_not_callable.pyi";
        source = {|
            __getattr__ = 3
          |};
      }
    in
    assert_type_errors
      ~context
      ~update_environment_with:
        [
          getattr_stub;
          getattr_stub_str;
          getattr_stub_untyped;
          getattr_stub_invalid_arity;
          getattr_stub_not_callable;
        ]
      source
  in
  assert_test_getattr
    {|
      import has_getattr
      def foo() -> None:
        has_getattr.any_attribute
    |}
    [];
  assert_test_getattr
    {|
      import has_getattr_str
      def foo() -> str:
        return has_getattr_str.any_attribute
    |}
    [];
  assert_test_getattr
    {|
      import has_getattr_untyped
      def foo() -> None:
        has_getattr_untyped.any_attribute
    |}
    [];
  assert_test_getattr
    {|
      from has_getattr import any_attribute
      def foo() -> None:
        any_attribute
    |}
    [];
  assert_test_getattr
    {|
      from has_getattr_str import any_attribute
      def foo() -> str:
        return any_attribute
    |}
    [
      "Undefined import [21]: Could not find a name `any_attribute` defined in module \
       `has_getattr_str`.";
    ];
  assert_test_getattr
    {|
      import has_getattr_invalid_arity
      def foo() -> None:
         has_getattr_invalid_arity.any_attribute
    |}
    [
      "Undefined attribute [16]: Module `has_getattr_invalid_arity` "
      ^ "has no attribute `any_attribute`.";
    ];
  assert_test_getattr
    {|
      import has_getattr_not_callable
      def foo() -> None:
         has_getattr_not_callable.any_attribute
    |}
    [
      "Undefined attribute [16]: Module `has_getattr_not_callable` "
      ^ "has no attribute `any_attribute`.";
    ]


let test_getattr_literal_access context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class A:
          def __init__(self) -> None:
              self.a = 1

      reveal_type(getattr(A(), "a"))
      reveal_type(getattr(A(), "a", None))
      reveal_type(getattr(A(), "a", 1))
      reveal_type(getattr(A(), "b"))
      reveal_type(getattr(A(), "c", None))
      getattr(A(), "a")
      getattr(A(), "b")
      getattr(A(), "c", None)
    |}
    [
      "Revealed type [-1]: Revealed type for `getattr(test.A(), \"a\")` is `int`.";
      "Revealed type [-1]: Revealed type for `getattr(test.A(), \"a\", None)` is `int`.";
      "Revealed type [-1]: Revealed type for `getattr(test.A(), \"a\", 1)` is `int`.";
      "Revealed type [-1]: Revealed type for `getattr(test.A(), \"b\")` is `unknown`.";
      "Revealed type [-1]: Revealed type for `getattr(test.A(), \"c\", None)` is `unknown`.";
      "Undefined attribute [16]: `A` has no attribute `b`.";
    ];
  ()


let test_check_metaclass_attributes context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class Meta(type):
        def f(cls) -> int:
          return 0
      class Instance(metaclass=Meta):
        pass
      def g() -> str:
        return Instance.f()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class MetaMake(type):
        in_metaclass: int = 1

      class Make(object, metaclass=MetaMake):
        existent: int = 1

      def foo() -> None:
        y = Make.existent
        reveal_type(y)
        z = Make.in_metaclass
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `z` is `int`.";
    ];
  assert_type_errors
    {|
      class MetaMake(type):
        in_metaclass: int = 1

      class Make(object, metaclass=MetaMake):
        existent: int = 1

      def foo() -> None:
        make_instance: Make
        y = make_instance.existent
        reveal_type(y)
        z = make_instance.in_metaclass
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Undefined attribute [16]: `Make` has no attribute `in_metaclass`.";
      "Revealed type [-1]: Revealed type for `z` is `unknown`.";
    ];
  assert_type_errors
    {|
      from typing import Any

      class MetaMake(type):
        # pyre-ignore[3]: Return type cannot be Any.
        def __getattr__(cls, key: str) -> Any: ...

      class Make(object, metaclass=MetaMake):
        existent: int = 1

      def foo() -> None:
        y = Make.existent
        reveal_type(y)
        z = Make.non_existent
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `z` is `typing.Any`.";
    ];
  (* Instances should not use `__getattr__` from the metaclass. *)
  assert_type_errors
    {|
      from typing import Any

      class MetaMake(type):
        # pyre-ignore[3]: Return type cannot be Any.
        def __getattr__(cls, key: str) -> Any: ...

      class Make(object, metaclass=MetaMake):
        existent: int = 1

      def foo() -> None:
        x: Make
        y = x.existent
        reveal_type(y)
        z = x.non_existent
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Undefined attribute [16]: `Make` has no attribute `non_existent`.";
      "Revealed type [-1]: Revealed type for `z` is `unknown`.";
    ];
  (* __getattr__ for class attributes should be looked up on the metaclass. *)
  assert_type_errors
    {|
      from typing import Any

      class MetaMake(type):
        def __getattr__(cls, key: str) -> str: ...

      class Make(object, metaclass=MetaMake):
        def __getattr__(cls, key: str) -> int: ...
        existent: int = 1

      def foo() -> None:
        y = Make.existent
        reveal_type(y)
        z = Make.non_existent
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `z` is `str`.";
    ];
  assert_type_errors
    {|
      from typing import Any

      class Make:
        def __getattr__(cls, key: str) -> int: ...
        existent: int = 1

      def foo() -> None:
        y = Make.existent
        reveal_type(y)
        z = Make.non_existent
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Undefined attribute [16]: `Make` has no attribute `non_existent`.";
      "Revealed type [-1]: Revealed type for `z` is `unknown`.";
    ];
  ()


let test_check_annotated context =
  assert_type_errors
    ~context
    {|
      from typing import Annotated
      class A:
        x: int = 0
      def f(a: Annotated[A, int]) -> int:
        return a.x
    |}
    [];
  assert_type_errors
    ~context
    {|
      from typing import Annotated
      class A:
        def x(self) -> int: ...
      def f(a: Annotated[A, int]) -> int:
        return a.x()
    |}
    [];
  assert_type_errors
    ~context
    {|
      from typing import Annotated
      def f(a: Annotated[str, int]) -> int:
        return a.foo()
    |}
    ["Undefined attribute [16]: `str` has no attribute `foo`."];
  assert_type_errors
    ~context
    {|
      class C:
        a: int = 0
      class D(C):
        a: str = ""
    |}
    [
      "Inconsistent override [15]: `a` overrides attribute defined in `C` inconsistently. Type \
       `str` is not a subtype of the overridden attribute `int`.";
    ];
  assert_type_errors
    ~context
    {|
      class C:
        __a: int = 0
      class D(C):
        __a: str = ""
    |}
    [];
  ()


let test_class_with_same_name_as_local_variable context =
  let assert_type_errors = assert_type_errors ~context in
  (* TODO(T121169620): Reproduce a bug where Pyre failed analyzing the function because of the `Foo
     = None` before the `Foo` class definition. Note that the file is also named `Foo.py`. *)
  assert_type_errors
    ~handle:"Foo.py"
    {|
      Foo = None

      class Foo:
        def some_method(self) -> None:
          x: Foo
    |}
    [
      "Incompatible variable type [9]: Foo is declared to have type `Type[Foo]` but is used as \
       type `None`.";
      "Analysis failure [30]: Terminating analysis because type `$local_Foo$Foo` is not defined.";
    ];
  (* TODO(T121169620): Pyre should recognize the `x` attribute. *)
  assert_type_errors
    ~handle:"Foo.py"
    {|
      Foo = None

      class Foo:
        x: int = 42

        def some_method(self) -> None:
          print(self.x)
    |}
    [
      "Incompatible variable type [9]: Foo is declared to have type `Type[Foo]` but is used as \
       type `None`.";
      "Undefined attribute [16]: `$local_Foo$Foo` has no attribute `x`.";
    ];
  (* TODO(T121169620): Due to a comedy of errors, Pyre resolves `some_module.Foo.Foo` to
     `some_module.Foo.Foo.Foo`, which does not exist. *)
  assert_type_errors
    ~update_environment_with:
      [
        (* This makes `some_module.Foo` map to `some_module.Bar.Foo`. *)
        {
          handle = "some_module/__init__.py";
          source = {|
            from some_module.Bar import Foo
          |};
        };
        (* This makes `some_module.Bar.Foo` map to `some_module.Foo.Foo`. Put together with the
           previous import, we map the module `some_module.Foo` to the class `some_module.Foo.Foo`,
           and the class `some_module.Foo.Foo` to `some_module.Foo.Foo.Foo`. *)
        {
          handle = "some_module/Bar.py";
          source = {|
            from some_module.Foo import Foo
          |};
        };
        {
          handle = "some_module/Foo.py";
          source = {|
            class Foo:
              x: int = 42
          |};
        };
      ]
    ~handle:"Baz.py"
    {|
      from some_module.Foo import Foo

      y: Foo
      reveal_type(y)
      reveal_type(y.x)
    |}
    [
      "Undefined or invalid type [11]: Annotation `Foo.Foo` is not defined as a type.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Any`.";
      "Revealed type [-1]: Revealed type for `y.x` is `unknown`.";
    ];
  ()


let () =
  "attribute"
  >::: [
         "check_attributes" >:: test_check_attributes;
         "check_attribute_decorators" >:: test_attribute_decorators;
         "check_attribute_strict" >:: test_attribute_strict;
         "check_attribute_initialization" >:: test_check_attribute_initialization;
         "check_missing_attribute" >:: test_check_missing_attribute;
         "check_attribute_type_variable_resolution" >:: test_attribute_type_variable_resolution;
         "check_getattr" >:: test_check_getattr;
         "check_getattr_literal_access" >:: test_getattr_literal_access;
         "check_metaclass_attributes" >:: test_check_metaclass_attributes;
         "check_annotated" >:: test_check_annotated;
         "class_with_same_name_as_local_variable" >:: test_class_with_same_name_as_local_variable;
       ]
  |> Test.run
