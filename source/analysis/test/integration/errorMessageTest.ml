(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test
open OUnit2
open IntegrationTest

let test_show_error_traces context =
  let assert_type_errors = assert_type_errors ~context ~show_error_traces:true ~handle:"test.py" in
  assert_type_errors
    "def foo() -> int: return 1.0"
    [
      "Incompatible return type [7]: Expected `int` but got `float`. Type `int` expected on line "
      ^ "1, specified on line 1.";
    ];
  assert_type_errors
    "def foo() -> str: return"
    [
      "Incompatible return type [7]: Expected `str` but got `None`. "
      ^ "Type `str` expected on line 1, specified on line 1.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[str]: return 1
    |}
    [
      "Incompatible return type [7]: Expected `List[str]` but got `int`. Type `List[str]` expected \
       on line 3, specified on line 3.";
    ];
  assert_type_errors
    {|
      import typing
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    "def foo(): pass"
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. "
      ^ "Type `None` was returned on line 1, return type should be specified on line 1.";
    ];
  assert_type_errors
    {|
      def foo():
        return None
    |}
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. "
      ^ "Type `None` was returned on line 3, return type should be specified on line 2.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = ""
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type "
      ^ "`int` but is used as type `str`.";
    ];
  assert_type_errors
    {|
      constant: int
      def foo() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as type \
       `str`.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        a = 1
        b = 2
        reveal_type(a + b)
    |}
    ["Revealed type [-1]: Revealed type for `a.__add__(b)` is `typing_extensions.Literal[3]`."];
  assert_type_errors
    {|
      from typing import List
      def foo(b: List[int]) -> None:
        a = 1
        b = [1,2,3]
        reveal_type(a in b)
    |}
    ["Revealed type [-1]: Revealed type for `a in b` is `bool`."];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          attribute = 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have "
      ^ "type `int` but is never initialized.";
    ];
  assert_type_errors
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
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `str` but \
       no type is specified. Attribute `attribute` declared on line 3, type `str` deduced from \
       test.py:7:4.";
      "Unbound name [10]: Name `x` is used but not defined in the current scope. Did you forget to \
       import it or assign to it?";
    ];
  assert_type_errors
    {|
      constant = x
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but \
       no type is specified. Global variable `constant` declared on line 2, type `int` deduced \
       from test.py:5:2.";
      "Unbound name [10]: Name `x` is used but not defined in the current scope. Did you forget to \
       import it or assign to it?";
    ];
  assert_type_errors
    {|
      constant = x
      def foo() -> None:
        global constant
        constant = "hi"
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type \
       `typing.Union[int, str]` but no type is specified. Global variable `constant` declared on \
       line 2, type `typing.Union[int, str]` deduced from test.py:5:2, test.py:6:2.";
      "Unbound name [10]: Name `x` is used but not defined in the current scope. Did you forget to \
       import it or assign to it?";
    ];
  assert_type_errors
    {|
      class Other():
        attribute = x
        def foo(self) -> None:
          self.attribute = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Other` has type `int` but \
       no type is specified. Attribute `attribute` declared on line 3, type `int` deduced from \
       test.py:5:4.";
      "Unbound name [10]: Name `x` is used but not defined in the current scope. Did you forget to \
       import it or assign to it?";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        a: typing.List[float] = [1]
        b: typing.List[int] = [2]
        a = b
    |}
    [
      "Incompatible variable type [9]: a is declared to have type `List[float]` but is used as \
       type `List[int]`. See https://pyre-check.org/docs/errors#covariance-and-contravariance for \
       mutable container errors.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[float]:
        l = [1]
        return l
    |}
    [
      "Incompatible return type [7]: Expected `List[float]` but got `List[int]`. Type \
       `List[float]` expected on line 5, specified on line 3. See \
       https://pyre-check.org/docs/errors#covariance-and-contravariance for mutable container \
       errors.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar
      T = TypeVar("T")
      class C:
        x: T
    |}
    [
      "Uninitialized attribute [13]: Attribute `x` is declared in class `C` to have type \
       `Variable[T]` but is never initialized.";
      "Invalid type variable [34]: The current class isn't generic with respect to the type \
       variable `Variable[T]`. To reference the type variable, you can modify the class to inherit \
       from `typing.Generic[T]`.";
    ]


let test_concise context =
  let assert_type_errors = assert_type_errors ~context ~concise:true in
  (* Illegal Annotation *)
  assert_type_errors
    {|
      class Foo: ...
      Foo().a: int = 1
    |}
    [
      "Illegal annotation target [35]: Target cannot be annotated.";
      "Undefined attribute [16]: `Foo` has no attribute `a`.";
    ];

  (* Incompatible Awaitable *)
  assert_type_errors
    {|
      await 1
    |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `int`."];
  assert_type_errors
    {|
    def foo(x: int) -> None: ...
    await foo
  |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `(x: int) -> None`."];

  (* Prohibited Any *)
  assert_type_errors
    {|
      import typing
      def foo() -> None:
        x: typing.Any = 1
    |}
    ["Prohibited any [33]: Given annotation cannot be `Any`."];

  (* Missing Annotation *)
  assert_type_errors
    {|
      import typing
      x: typing.Any = 1
    |}
    ["Missing global annotation [5]: Global annotation cannot be `Any`."];
  assert_type_errors
    {|
      def foo():
        return 1
    |}
    ["Missing return annotation [3]: Return type must be annotated."];
  assert_type_errors
    {|
      def foo(x = 1) -> None:
        return
    |}
    ["Missing parameter annotation [2]: Parameter must be annotated."];

  (* Incompatible Annotation *)
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Union[int, str] = 1.0) -> None:
        return
    |}
    ["Incompatible variable type [9]: x has type `Union[int, str]`; used as `float`."];
  assert_type_errors
    {|
      def foo() -> int:
        return
    |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];
  assert_type_errors
    {|
      class Foo:
        a: int = 1
      Foo.a = "string"
    |}
    ["Incompatible attribute type [8]: Attribute has type `int`; used as `str`."];
  assert_type_errors
    {|
      x: int = "string"
    |}
    ["Incompatible variable type [9]: x has type `int`; used as `str`."];
  assert_type_errors
    ~update_environment_with:[{ handle = "export.py"; source = "class Foo:\n  a: int = 1" }]
    {|
      from export import Foo
      Foo.a = "string"
    |}
    ["Incompatible attribute type [8]: Attribute has type `int`; used as `str`."];
  assert_type_errors
    ~update_environment_with:[{ handle = "export.py"; source = "a: int = 1" }]
    {|
      import export
      export.a = "string"
    |}
    ["Incompatible variable type [9]: a has type `int`; used as `str`."];

  (* Inconsistent Override *)
  assert_type_errors
    ~update_environment_with:
      [
        {
          handle = "export.py";
          source =
            {|
          class Foo:
            def foo(self, x: int) -> None:
              return
        |};
        };
      ]
    {|
      from export import Foo
      class Bar(Foo):
        def foo(self, x: str) -> None:
          return
    |}
    ["Inconsistent override [14]: `foo` overrides method defined in `Foo` inconsistently."];
  assert_type_errors
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
  assert_type_errors
    {|
      MyType = 1
      def foo() -> MyType:
        return
    |}
    ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];

  (* Argument Errors *)
  assert_type_errors
    {|
      def foo(x: int) -> None:
       return
      foo(y=1)
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `y`."];
  assert_type_errors
    {|
      def foo(x: int, y: int) -> None:
       return
      foo(1, 2, 3)
    |}
    ["Too many arguments [19]: Expected 2 positional arguments."];
  assert_type_errors
    {|
      def foo(x: int, y: int) -> None:
       return
      foo(1)
    |}
    ["Missing argument [20]: Argument `y` expected."];

  (* Not Callable *)
  assert_type_errors {|
      x = 1
      x()
    |} ["Call error [29]: `int` is not a function."];

  (* TypedDict *)
  assert_type_errors
    {|
      import mypy_extensions
      Cat = mypy_extensions.TypedDict('Cat', {'name': str, 'breed': str})
      def foo(x: Cat) -> None:
          y = x["year"]
    |}
    [
      "Undefined import [21]: Could not find module `mypy_extensions`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Cat` has no key `year`.";
    ];

  (* Redundant Cast *)
  assert_type_errors
    {|
      import typing
      x: int
      y: int = typing.cast(int, x)
    |}
    ["Redundant cast [22]: The cast is redundant."];

  (* Undefined Import, Name, Type *)
  assert_type_errors
    {|
      from a.b import c
    |}
    ["Undefined import [21]: Could not find module `a.b`."];
  assert_type_errors
    {|
      def foo() -> None:
        y = x
    |}
    ["Unbound name [10]: Name `x` is used but not defined."];
  assert_type_errors
    {|
      def foo(x: X) -> None:
        return
    |}
    ["Unbound name [10]: Name `X` is used but not defined."];

  (* Uninitialized Attribute *)
  assert_type_errors
    {|
      class Foo:
        x: int
    |}
    ["Uninitialized attribute [13]: Attribute `x` is never initialized."];

  (* ClassVar *)
  assert_type_errors
    {|
      from typing import ClassVar
      class Base:
        y: ClassVar[int] = 0
      b = Base()
      b.y = 12 # error
    |}
    ["Invalid assignment [41]: Assigning to class variable through instance."];
  assert_type_errors
    {|
        from abc import abstractmethod, ABCMeta
        class Foo(metaclass=ABCMeta):
          @abstractmethod
          def bar(self) -> None:
            pass
        def foo() -> None:
          Foo()
        |}
    ["Invalid class instantiation [45]: Cannot instantiate abstract class `Foo`."]


let test_reveal_type context =
  let assert_type_errors = assert_type_errors ~context ~handle:"test.py" in
  assert_type_errors
    {|
      class A: pass
      a: A
      reveal_type(a)
      reveal_type(a, qualify=True)
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `A`.";
      "Revealed type [-1]: Revealed type for `a` is `test.A`.";
    ]


let () =
  "errorMessage"
  >::: [
         "check_show_error_traces" >:: test_show_error_traces;
         "check_concise" >:: test_concise;
         "reveal_types" >:: test_reveal_type;
       ]
  |> Test.run
