(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open Test
open IntegrationTest


let test_check_attributes _ =
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
        bar = 1 # type: int
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
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` " ^
      "has type `int` but is used as type `None`.";
      "Incompatible return type [7]: Expected `str` but got `int`."
    ];


  assert_strict_type_errors
    {|
      class Bar:
        def bar() -> None:
          pass
      class Foo:
        bar: typing.Optional[Bar] = None
        def foo(self) -> None:
          self.bar.bar()
    |}
    ["Undefined attribute [16]: Optional type has no attribute `bar`."];

  assert_type_errors
    {|
      a = str
      b = 1
    |}
    [];

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
      "Incompatible return type [7]: Expected `int` but got `unknown`. Type `int` expected on " ^
      "line 6, specified on line 4.";
      "Undefined attribute [16]: `Bar` has no attribute `baz`."
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
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Any
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has no type specified.";
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have " ^
      "non-optional type `typing.Any` but is never initialized."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Any
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but type " ^
      "`Any` is specified.";
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have " ^
      "non-optional type `typing.Any` but is never initialized.";
      "Incompatible return type [7]: Expected `int` but got `str`."
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
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
          class Foo:
            a: str = ""
          Foo.a = 1
    |}
    [
      "Incompatible attribute type [8]: Attribute `a` declared in class `Foo` has type `str` " ^
      "but is used as type `int`."
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
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
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
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
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
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
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
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.a = 3
        def foo(self) -> str:
            return self.a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified.";
      "Incompatible return type [7]: Expected `str` but got `int`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a and a.bar:
          return a.bar
        return 0
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a.bar and a:
          return a.bar
        return 0
    |}
    [
      "Undefined attribute [16]: Optional type has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`.";
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
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` " ^
      "has type `int` but no type is specified.";
    ];

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
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` " ^
      "has type `int` but no type is specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        def foo(self, bar: typing.Optional[int]) -> int:
          self.baz = bar
          if self.baz is None:
            self.baz = 5
          return self.baz
    |}
    [
      (* TODO(T24330702): we should only report this once. *)
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
    ];

  (* TODO(T25072735): support attribute tests for: class variables, generic annotations *)
  assert_type_errors
    {|
      class Foo:
        bar: typing.ClassVar[int] = 1
      def foo() -> int:
        Foo.bar = "foo"
        return Foo.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Generic[_T]
        def foo(self) -> int:
          self.bar = 0
          return self.bar
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have " ^
      "non-optional type `typing.Generic[Variable[_T]]` but is never initialized.";
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type " ^
      "`typing.Generic[Variable[_T]]` but is used as type `int`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Generic[Variable[_T]]`.";
    ];

  (* Static attributes are properly resolved. *)
  assert_type_errors
    {|
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
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
    ];
  assert_type_errors
    {|
      class unittest.TestCase: ...
      class Foo(unittest.TestCase):
        def setUp(self) -> None:
          self.attribute: int = 1
        def foo(self) -> str:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class unittest.case.TestCase: ...
      class Foo(unittest.case.TestCase):
        def setUp(self) -> None:
          self.attribute: int = 1
        def foo(self) -> str:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute: int = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [];

  (* Class implements `__getattr__`. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def __getattr__(self, attribute) -> str: ...
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

  (* Any has all attributes in default mode, but not strict mode. *)
  assert_strict_type_errors
    {|
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
  assert_default_type_errors
    {|
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    [];

  (* We allow instance attributes to be accessed via class objects. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      Foo.attribute
    |}
    [];

  (* Check attribute type propagation. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def foo(self) -> None:
          self.attribute = not_annotated()
          a = self.attribute.something
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `unknown`.";
    ];

  (* Check attribute type variable resolution. *)
  assert_type_errors
    {|
      _VALUE = typing.TypeVar('_VALUE')
      class Wrapper(typing.Generic[_VALUE]):
        value: _VALUE

      def bar(wrapper: Wrapper[int]) -> int:
        return wrapper.value
    |}
    [
      "Uninitialized attribute [13]: Attribute `value` is declared in class `Wrapper` to have " ^
      "non-optional type `Variable[_VALUE]` but is never initialized.";
    ];
  assert_type_errors
    {|
      _VALUE = typing.TypeVar('_VALUE')
      class Wrapper(typing.Generic[_VALUE]):
        value: _VALUE

      class WrapperSubclass(Wrapper[int]):
        pass

      def bar(wrapper: WrapperSubclass) -> int:
        return wrapper.value
    |}
    [
      "Uninitialized attribute [13]: Attribute `value` is declared in class `Wrapper` to have " ^
      "non-optional type `Variable[_VALUE]` but is never initialized.";
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class ReturnSelf(typing.Generic[_T]):
        def f(self) -> ReturnSelf[_T]:
          return self
    |}
    [];

  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class ReturnClass(typing.Generic[_T]):
        @classmethod
        def f(cls) -> ReturnClass[_T]:
          return cls
    |}
    [
      "Incompatible return type [7]: Expected `ReturnClass[Variable[_T]]` but got \
       `typing.Type[ReturnClass[Variable[_T]]]`.";
    ];
  assert_type_errors
    {|
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
      T = typing.TypeVar('T')
      def f(t: typing.Type[T]) -> None:
        a = t()
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def f(t: typing.Type[T]) -> None:
        a = t()
    |}
    ["Missing argument [20]: Call `int.__init__` expects argument `value`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', int)
      def f(t: typing.Type[T]) -> None:
        a = t()
    |}
    ["Missing argument [20]: Call `int.__init__` expects argument `value`."];

  (* Do not resolve optional attributes to the optional type. *)
  assert_type_errors
    {|
      class Foo:
        debug: int = 1
      def foo(f: typing.Optional[Foo]) -> int:
        return f.debug
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Optional type has no attribute `debug`.";
    ];

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
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `int` but is used as type `None`.";
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `int` but is used as type `str`.";
    ];

  assert_type_errors
    {|
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
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type `int` but \
       is used as type `None`.";
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type `int` but \
       is used as type `str`.";
    ];

  assert_type_errors
    {|
      __property__: typing.Any = ...
      x: typing.Optional[int]
      class Foo:
        @__property__
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
        @$local_file$__property__
        def y(self) -> int: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> int:
        foo = Foo()
        return foo.y
    |}
    [];

  assert_type_errors
    {|
      class C(enum.IntEnum):
        a: int = 1
    |}
    [];
  assert_type_errors
    {|
      class C(enum.IntEnum):
        a = 1
    |}
    [];
  assert_type_errors
    {|
      class C(enum.IntEnum):
        a: int
    |}
    [
      "Uninitialized attribute [13]: Attribute `a` is declared in class `C` to have non-optional \
       type `C` but is never initialized.";
    ];
  assert_type_errors
    {|
      class C(enum.IntEnum):
        a: str = 1
    |}
    [
      "Incompatible attribute type [8]: Attribute `a` declared in class `C` has type `str` but is \
       used as type `int`.";
    ]


let test_check_missing_attribute _ =
  assert_type_errors
    {|
      class Foo:
        a = unknown
      Foo.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified.";
      "Undefined name [18]: Global name `unknown` is undefined.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified."
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
        a = unknown
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
      "Undefined name [18]: Global name `unknown` is undefined.";
    ];

  assert_type_errors
    {|
        class Foo:
          a: typing.Any
        Foo.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but type `Any` is specified.";
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have " ^
      "non-optional type `typing.Any` but is never initialized."
    ];

  assert_type_errors
    {|
        class Foo:
          def __init__(self, a: typing.Any) -> None:
            self.a = a
    |}
    [
      "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a: typing.Any
      Foo().a = 1
      def foo() -> bool:
        return Foo().a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but type `Any` is specified.";
      "Incompatible return type [7]: Expected `bool` but got `int`."
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
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type " ^
      "`typing.Union[int, str]` but no type is specified."
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
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type " ^
      "`typing.Union[int, str]` but no type is specified."
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
      class Foo:
        a: typing.Any
      Foo.a = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo(enum.IntEnum):
        A: int = 1
      class Bar:
        A = Foo.A.value
    |}
    [
      "Missing attribute annotation [4]: Attribute `A` of class `Bar` has type `int` but no type \
       is specified.";
    ]


let test_check_getattr _ =
  let open Ast.Expression in
  let assert_test_getattr source =
    let getattr_stub =
      {
        qualifier = Access.create "has_getattr";
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
        qualifier = Access.create "has_getattr_str";
        handle = "has_getattr_str.pyi";
        source =
          {|
            def __getattr__(name: str) -> str: ...
          |};
      }
    in
    let getattr_stub_untyped =
      {
        qualifier = Access.create "has_getattr_untyped";
        handle = "has_getattr_untyped.pyi";
        source =
          {|
            def __getattr__(name): ...
          |};
      }
    in
    let getattr_stub_invalid_arity =
      {
        qualifier = Access.create "has_getattr_invalid_arity";
        handle = "has_getattr_invalid_arity.pyi";
        source =
          {|
            def __getattr__(x: int, y: str) -> str: ...
          |};
      }
    in
    let getattr_stub_not_callable =
      {
        qualifier = Access.create "has_getattr_not_callable";
        handle = "has_getattr_not_callable.pyi";
        source =
          {|
            __getattr__ = 3
          |};
      }
    in
    assert_type_errors
      ~update_environment_with:[
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
    [];
  assert_test_getattr
    {|
      import has_getattr_invalid_arity
      def foo() -> None:
         has_getattr_invalid_arity.any_attribute
    |}
    [
      "Undefined attribute [16]: Module `has_getattr_invalid_arity` " ^
      "has no attribute `any_attribute`."
    ];
  assert_test_getattr
    {|
      import has_getattr_not_callable
      def foo() -> None:
         has_getattr_not_callable.any_attribute
    |}
    [
      "Undefined attribute [16]: Module `has_getattr_not_callable` " ^
      "has no attribute `any_attribute`."
    ]


let () =
  "attribute">:::[
    "check_attributes">::test_check_attributes;
    "check_missing_attribute">::test_check_missing_attribute;
    "check_getattr">::test_check_getattr;
  ]
  |> Test.run
