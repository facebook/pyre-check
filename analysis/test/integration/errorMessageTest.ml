(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Test
open OUnit2
open IntegrationTest


let test_show_error_traces _ =
  assert_type_errors ~show_error_traces:true
    "def foo() -> int: return 1.0"
    [
      "Incompatible return type [7]: Expected `int` but got `float`. Type `int` expected on line " ^
      "1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> str: return"
    [
      "Incompatible return type [7]: Expected `str` but got `None`. " ^
      "Type `str` expected on line 1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> typing.List[str]: return 1"
    [
      "Incompatible return type [7]: Expected `typing.List[str]` but got `int`. Type " ^
      "`typing.List[str]` expected on line 1, specified on line 1.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type " ^
      "that does not contain `Any`.";
      "Missing return annotation [3]: Return type must be specified as type " ^
      "that does not contain `Any`.";
    ];


  assert_type_errors ~show_error_traces:true
    "def foo(): pass"
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. " ^
      "Type `None` was returned on line 1, return type should be specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo():
        return None
    |}
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. " ^
      "Type `None` was returned on line 3, return type should be specified on line 2.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = ""
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `str`. Attribute `attribute` declared on line 3, incorrectly " ^
      "used on line 5.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      constant: int
      def foo() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as " ^
      "type `str`. Redeclare `constant` on line 5 if you wish to override the previously " ^
      "declared type.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> None:
        a = 1
        b = 2
        reveal_type(a + b)
    |}
    [
      "Revealed type [-1]: Revealed type for `a.__add__.(...)` is `int`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          attribute = 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "type `int` but is never initialized. Attribute `attribute` is declared on " ^
      "line 3, never initialized and therefore must be `typing.Optional[int]`.";
    ];
  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute = x
      class Bar:
        def bar(self) -> str:
          foo = Foo()
          foo.attribute = 'string'
          return foo.attribute
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `str` " ^
      "but no type is specified. Attribute `attribute` declared on line 3, type `str` deduced " ^
      "from test.py:7:4.";
      "Undefined name [18]: Global name `x` is not defined, or there is at least one control \
       flow path that doesn't define `x`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      constant = x
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified. Global variable `constant` declared on line 2, type `int` deduced " ^
      "from test.py:5:2.";
      "Undefined name [18]: Global name `x` is not defined, or there is at least one control \
       flow path that doesn't define `x`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      constant = x
      def foo() -> None:
        global constant
        constant = "hi"
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `constant` declared " ^
      "on line 2, type `typing.Union[int, str]` deduced from test.py:5:2, test.py:6:2.";
      "Undefined name [18]: Global name `x` is not defined, or there is at least one control \
       flow path that doesn't define `x`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Other():
        attribute = x
        def foo(self) -> None:
          self.attribute = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Other` has type " ^
      "`int` but no type is specified. Attribute `attribute` declared on line 3, " ^
      "type `int` deduced from test.py:5:4.";
      "Undefined name [18]: Global name `x` is not defined, or there is at least one control \
       flow path that doesn't define `x`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> None:
        global x
        x = 5
      def bar() -> None:
        global x
        x = "str"
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `x` " ^
      "declared on line 4, type `typing.Union[int, str]` deduced from test.py:4:2, " ^
      "test.py:7:2.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      a: typing.List[float] = [1]
      b: typing.List[int] = [2]
      a = b
    |}
    [
      "Incompatible variable type [9]: a is declared to have type `typing.List[float]` but is \
       used as type `typing.List[int]`. Redeclare `a` on line 4 if you wish to override the \
       previously declared type. See https://pyre-check.org/docs/error-types.html#list-and-\
       dictionary-mismatches-with-subclassing for mutable container errors.";
    ];
  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> typing.List[float]:
        l = [1]
        return l
    |}
    [
      "Incompatible return type [7]: Expected `typing.List[float]` but got `typing.List[int]`. \
       Type `typing.List[float]` expected on line 4, specified on line 2. \
       See https://pyre-check.org/docs/error-types.html#list-and-dictionary-mismatches-with-\
       subclassing for mutable container errors.";
    ]


let test_concise _ =
  (* Illegal Annotation Target *)
  assert_type_errors ~concise:true
    {|
      class Foo: ...
      Foo().a: int = 1
    |}
    ["Illegal annotation target [35]: Target cannot be annotated."];

  (* Impossible Isinstance *)
  assert_type_errors ~concise:true
    {|
      def foo(x: int) -> None:
        assert not isinstance(x, int)
    |}
    ["Impossible isinstance check [25]: isinstance check will always fail."];

  (* Incompatible Awaitable *)
  assert_type_errors ~concise:true
    {|
      await 1
    |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `int`."];

  assert_type_errors ~concise:true
    {|
    def foo(x: int) -> None: ...
    await foo
  |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `Callable[[int], None]`."];

  (* Prohibited Any *)
  assert_type_errors ~concise:true
    {|
      def foo() -> None:
        x: typing.Any = 1
    |}
    ["Prohibited any [33]: Given annotation cannot be `Any`."];

  (* Missing Annotation *)
  assert_type_errors ~concise:true
    {|
      x: typing.Any = 1
    |}
    ["Missing global annotation [5]: Global annotation cannot be `Any`."];

  assert_type_errors ~concise:true
    {|
      def foo():
        return 1
    |}
    ["Missing return annotation [3]: Return type must be annotated."];

  assert_type_errors ~concise:true
    {|
      def foo(x = 1) -> None:
        return
    |}
    ["Missing parameter annotation [2]: Parameter must be annotated."];

  (* Incompatible Annotation *)
  assert_type_errors ~concise:true
    {|
      def foo(x: typing.Union[int, str] = 1.0) -> None:
        return
    |}
    ["Incompatible variable type [9]: x has type `Union[int, str]`; used as `float`."];

  assert_type_errors ~concise:true
    {|
      def foo() -> int:
        return
    |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];

  assert_type_errors ~concise:true
    {|
      class Foo:
        a: int = 1
      Foo.a = "string"
    |}
    ["Incompatible attribute type [8]: Attribute has type `int`; used as `str`."];

  assert_type_errors ~concise:true
    {|
      x: int = "string"
    |}
    ["Incompatible variable type [9]: x has type `int`; used as `str`."];

  assert_type_errors ~concise:true
    ~update_environment_with:[
      {
        qualifier = !&"export";
        handle = "export.py";
        source = "class Foo:\n  a: int = 1"
      };
    ]
    {|
      from export import Foo
      Foo.a = "string"
    |}
    ["Incompatible attribute type [8]: Attribute has type `int`; used as `str`."];

  assert_type_errors ~concise:true
    ~update_environment_with:[
      {
        qualifier = !&"export";
        handle = "export.py";
        source = "a: int = 1"
      };
    ]
    {|
      import export
      export.a = "string"
    |}
    ["Incompatible variable type [9]: a has type `int`; used as `str`."];

  (* Inconsistent Override *)
  assert_type_errors ~concise:true
    ~update_environment_with:[
      {
        qualifier = !&"export";
        handle = "export.py";
        source = {|
          class Foo:
            def foo(self, x: int) -> None:
              return
        |}
      };
    ]
    {|
      from export import Foo
      class Bar(Foo):
        def foo(self, x: str) -> None:
          return
    |}
    ["Inconsistent override [14]: `foo` overrides method defined in `Foo` inconsistently."];

  assert_type_errors ~concise:true
    {|
      class Foo:
        def foo(self, x: int) -> int:
          return 1
      class Bar(Foo):
        def foo(self, x: int) -> str:
          return "string"
    |}
    ["Inconsistent override [15]: `foo` overrides method defined in `Foo` inconsistently."];

  (* Invalid Type *)
  assert_type_errors ~concise:true
    {|
      MyType = 1
      def foo() -> MyType:
        return
    |}
    ["Invalid type [31]: Expression `MyType` is not a valid type."];

  (* Argument Errors *)
  assert_type_errors ~concise:true
    {|
      def foo(x: int) -> None:
       return
      foo(y=1)
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `y`."];

  assert_type_errors ~concise:true
    {|
      def foo(x: int, y: int) -> None:
       return
      foo(1, 2, 3)
    |}
    ["Too many arguments [19]: Expected 2 positional arguments."];

  assert_type_errors ~concise:true
    {|
      def foo(x: int, y: int) -> None:
       return
      foo(1)
    |}
    ["Missing argument [20]: Argument `y` expected."];

  (* Not Callable *)
  assert_type_errors ~concise:true
    {|
      x = 1
      x()
    |}
    ["Call error [29]: `int` is not a function."];

  (* TypedDict *)
  assert_type_errors ~concise:true
    {|
      Cat = mypy_extensions.TypedDict('Cat', {'name': str, 'breed': str})
      def foo(x: Cat) -> None:
          y = x["year"]
    |}
    [
      "TypedDict accessed with a missing key [27]: TypedDict `Cat` has no key `year`.";
    ];

  (* Redundant Cast *)
  assert_type_errors ~concise:true
    {|
      x: int
      y: int = typing.cast(int, x)
    |}
    ["Redundant cast [22]: The cast is redundant."];

  (* Undefined Import, Name, Type *)
  assert_type_errors ~concise:true
    {|
      from a.b import c
    |}
    ["Undefined import [21]: Could not find `b`."];

  assert_type_errors ~concise:true
    {|
      def foo() -> None:
        y = x
    |}
    ["Undefined name [18]: Global name `x` is undefined."];

  assert_type_errors ~concise:true
    {|
      def foo(x: X) -> None:
        return
    |}
    ["Undefined type [11]: Type `X` is not defined."];

  (* Uninitialized Attribute *)
  assert_type_errors ~concise:true
    {|
      class Foo:
        x: int
    |}
    ["Uninitialized attribute [13]: Attribute `x` is never initialized."]


let () =
  "errorMessage">:::[
    "check_show_error_traces">::test_show_error_traces;
    "check_concise">::test_concise;
  ]
  |> Test.run
