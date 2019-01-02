(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open IntegrationTest


let test_check_method_returns _ =
  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower().upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo() -> int:
          return ''.upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."]


let test_check_method_parameters _ =
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr(1)
    |}
    [];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(a: str, b: str) -> None:
        pass
      def bar() -> None:
        foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `str` for 2nd anonymous parameter to call `foo` but got `int`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf').substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input + 1
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.__sizeof__()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

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
        def bar(x: int) -> int:
          return x
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type `Foo`.";
      "Incompatible return type [7]: Expected `int` but got `Foo`.";
    ]


let test_check_method_resolution _ =
  assert_type_errors
    {|
      def foo() -> None:
        bar().baz()
    |}
    ["Undefined name [18]: Global name `bar` is undefined."];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.lower()
    |}
    []


let test_check_self _ =
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
    [
      "Incompatible variable type [9]: self is declared to have type `Other` but is used as type \
       `Some`.";
    ];

  assert_type_errors
    {|
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
    |}
    []


let test_check_static _ =
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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.foo` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

        def bar(self) -> None:
          self.foo('asdf')

    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.foo` but got `str`."];

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
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Foo.foo` but got `str`.";
    ];

  assert_type_errors
    {|
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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.classmethod` but got `str`."];

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
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.staticmethod` but got `str`."];

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
      "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `Foo.instancemethod` but got `str`.";
    ];

  (* Special classmethods are treated properly without a decorator. *)
  assert_type_errors
    {|
      class Foo:
        def __init_subclass__(cls) -> typing.Type[Foo]:
          return cls
        def __new__(cls) -> typing.Type[Foo]:
          return cls
        def __class_getitem__(cls, key: int) -> typing.Type[Foo]:
          return cls
    |}
    []


let test_check_setitem _ =
  assert_type_errors
    {|
      def foo(x: typing.Dict[str, int]) -> None:
        x["foo"] = "bar"
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`."];

  assert_type_errors
    {|
      class A:
        pass
      def foo(x: typing.Dict[str, int], y: A) -> None:
        x["foo"] = y["bar"] = "baz"
    |}
    [
      "Undefined attribute [16]: `A` has no attribute `__setitem__`.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Dict[str, typing.Dict[str, int]]) -> None:
        x["foo"]["bar"] = "baz"
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`."];

  assert_type_errors
    {|
      def foo(x: typing.Dict[str, int]) -> None:
        x[7] = 7
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `dict.__setitem__` but got `int`."]


let test_check_callable_protocols _ =
  (* Objects with a `__call__` method are callables. *)
  assert_type_errors
    {|
      class Call:
        def __call__(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [];

  (* We handle subclassing. *)
  assert_type_errors
    {|
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
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Call error [29]: `Call` is not a function.";
    ];

  assert_type_errors
    ~debug:false
    {|
      def foo(call) -> int:
        return call()
    |}
    [];

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
    [
      "Call error [29]: `Call` is not a function.";
    ];

  assert_type_errors
    {|
      class patch:
        def __call__(self) -> int: ...

      unittest.mock.patch: patch = ...

      def foo() -> None:
        unittest.mock.patch()
        unittest.mock.patch()  # subequent calls should not modify annotation map
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Callable(Foo.bar)[[Named(self, unknown), Named(x, int)], str]`.";
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
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Callable(Foo.bar)[[Named(x, int)], str]`.";
    ];

  assert_type_errors
    {|
      class Call:
        def __call__(self, x: int) -> int: ...
      def foo(call: Call) -> int:
        return call("")
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Call.__call__` but got `str`.";
    ]


let test_check_explicit_method_call _ =
  assert_type_errors
    {|
      class Class:
        def method(self, i: int) -> None:
          pass
      Class.method(object(), 1)
    |}
    []


let () =
  "method">:::[
    "check_callable_protocols">::test_check_callable_protocols;
    "check_explicit_method_call">::test_check_explicit_method_call;
    "check_method_returns">::test_check_method_returns;
    "check_method_parameters">::test_check_method_parameters;
    "check_method_resolution">::test_check_method_resolution;
    "check_self">::test_check_self;
    "check_setitem">::test_check_setitem;
    "check_static">::test_check_static;
  ]
  |> Test.run
