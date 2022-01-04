(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest
open Test

let test_check_with_qualification context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors {|
      x: int = 1
      def foo(x: str) -> str:
        return x
    |} [];
  assert_type_errors
    {|
      x: int = 1
      def foo(y: str) -> str:
        return x
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      l: typing.List[int] = [1]
      def hello() -> int:
        for i in l:
          return i
        return -1
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 123

      def duh(global_number: str) -> int:
          return len(global_number)
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 123
      def wut(global_number: str) -> None:
          def nonglobal_inner_access() -> int:
              return len(global_number)
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 123
      def wut(global_number: str) -> None:
          def wut_inner_global() -> int:
              global global_number
              return global_number

    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      global_number: int = 123
      def rly() -> int:
          def rly_inner(global_number: str) -> None:
              pass
          return global_number
      def len(s: str) -> int:
          return 1
      def assign() -> int:
          global_number: str = "a"
          return len(global_number)
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 1
      def len(s: str) -> int:
        return 1
      def assign_outer() -> None:
          global_number: str ="a"
          def assign_inner_access() -> int:
              return len(global_number)
          def assign_inner_global() -> int:
              global global_number
              return global_number
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def derp() -> int:
          def derp_inner() -> None:
              global_number: str = "a"
              pass
          return global_number
    |}
    [];
  assert_type_errors
    {|
      def access_side_effect(global_number: str) -> int:
          side_effect=global_number
          return len(global_number)
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 1
      def access_side_effect_2() -> int:
          side_effect=global_number
          return global_number
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 1
      def pure_sideffect() -> None:
          side_effect=global_number
          def pure_side_effect_inner() -> int:
              return global_number
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 1
      def access_transitive() -> int:
          transitive=global_number
          return transitive
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 1
      def assign_transitive() -> None:
          another=global_number
          # TODO(T27001301): uncomment next two lines when nested scopes will work
          #def out_of_ideas_3() -> int:
          #    return another
      def assign_transitive_2() -> int:
          transitive=global_number
          def assign_transitive_inner() -> None:
              global_number="a"
          return transitive
    |}
    []


let test_check_globals context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      constant: int = 1
      def foo() -> str:
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      constant: typing.Union[int, str] = 1
      def foo() -> int:
        return constant
    |}
    ["Incompatible return type [7]: Expected `int` but got `Union[int, str]`."];
  assert_type_errors
    {|
      constant: int = 1
      constant: str = ""
      def foo() -> str:
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      constant = 1
      constant = ""
      def foo() -> str:
        return constant
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` "
      ^ "but is used as type `str`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
      x = 1
      constant = x
      def foo() -> str:
        return constant
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` "
      ^ "but no type is specified.";
    ];
  assert_type_errors
    {|
      nasty_global = foo()
      def foo() -> int:
        a = nasty_global
        return 0
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `nasty_global` "
      ^ "has type `int` but no type is specified.";
    ];
  assert_type_errors
    {|
      a, b = 1, 2
      def foo() -> str:
        return a
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      a: int
      b: int
      a, b = 1, 2
      def foo() -> str:
        return a
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      x: typing.List[int]
      def foo() -> int:
        return x[0]
    |}
    [];
  assert_type_errors
    {|
      import typing
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x[0:1]
    |}
    [];
  assert_default_type_errors
    {|
      import typing
      x: typing.List = [1,2,3]
      def foo() -> typing.List[typing.Any]:
        return x
    |}
    [];
  assert_type_errors
    {|
      x = []
      def foo() -> None:
        reveal_type(x)
    |}
    [
      "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `x` is incomplete, add \
       an explicit annotation.";
      "Missing global annotation [5]: Globally accessible variable `x` has no type specified.";
      "Revealed type [-1]: Revealed type for `x` is `typing.List[typing.Any]`.";
    ];
  assert_default_type_errors
    {|
      import typing
      x: typing.Dict = { "derp": 42 }
      def foo() -> typing.Dict[typing.Any, typing.Any]:
        return x
    |}
    [];
  assert_type_errors
    ~update_environment_with:[{ handle = "export.py"; source = "a, b, c = 1, 2, 3" }]
    {|
      from export import a
      def foo() -> str:
        return a
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    ~update_environment_with:
      [{ handle = "export.py"; source = {|
          str_to_int_dictionary = {"a": 1}
        |} }]
    {|
      from export import str_to_int_dictionary
      def foo() -> str:
        return str_to_int_dictionary
    |}
    ["Incompatible return type [7]: Expected `str` but got `Dict[str, int]`."];
  assert_type_errors
    ~update_environment_with:[{ handle = "export.py"; source = "x = 1" }]
    {|
      from export import x
      def foo() -> str:
        return x
      def bar() -> str:
        global x
        x = ""
        return x
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible variable type [9]: export.x is declared to have type `int` "
      ^ "but is used as type `str`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      x = None
      y = []
      def foo() -> str:
        global x
        x = ""
        return x
      def bar() -> typing.List[int]:
        global y
        y.append(1)
        return y
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has type \
       `typing.Optional[str]` but no type is specified.";
      "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `y` is incomplete, add \
       an explicit annotation.";
      "Missing global annotation [5]: Globally accessible variable `y` has no type specified.";
    ];
  assert_type_errors {|
      import typing
      A = typing.Mapping[int, str]
    |} [];
  assert_type_errors
    {|
      A = MappBoo[int, str]
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `A` has no type specified.";
      "Unbound name [10]: Name `MappBoo` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
      import typing
      MyType = typing.List[typing.Any]
    |}
    ["Prohibited any [33]: `MyType` cannot alias to a type containing `Any`."];
  assert_type_errors
    {|
      import typing
      GLOBAL: typing.Optional[int]
      def foo() -> int:
        if GLOBAL:
          return GLOBAL
        return 0
    |}
    [];
  assert_type_errors
    {|
      import typing
      GLOBAL: typing.Optional[int]
      def call() -> None: pass
      def foo() -> int:
        if GLOBAL:
          call()
          return GLOBAL
        return 0
    |}
    ["Incompatible return type [7]: Expected `int` but got `Optional[int]`."];
  assert_type_errors
    {|
      from typing import Any, Callable, Mapping, Union
      a: Union[Callable]
      b: Union[Mapping[str, Any]]
      c: Union[Callable, Mapping[str, Any]]
      d: Callable
      e: Mapping[str, Any]
    |}
    [
      "Invalid type parameters [24]: Generic type `Callable` expects 2 type parameters.";
      "Missing global annotation [5]: Globally accessible variable `c` must be specified as type \
       that does not contain `Any`.";
      "Invalid type parameters [24]: Generic type `Callable` expects 2 type parameters.";
      "Invalid type parameters [24]: Generic type `Callable` expects 2 type parameters.";
    ];
  (* Verify resolve literal for unannotated globals doesn't create huge types that cause OOMs. *)
  assert_default_type_errors
    {|
      def cos(x: int) -> int: ...
      def sin(x: int) -> int: ...
      def exp(x: int) -> int: ...
      def pow(x: int) -> int: ...
      def acos(x: int) -> int: ...
      def acosh(x: int) -> int: ...
      def asin(x: int) -> int: ...
      def asinh(x: int) -> int: ...
      def cosh(x: int) -> int: ...
      def tan(x: int) -> int: ...
      def tan(x: int) -> int: ...
      def tanh(x: int) -> int: ...
      def log(x: int) -> int: ...
      def atan(x: int) -> int: ...
      def atanh(x: int) -> int: ...

      method_dictionary = {
          "sin":     [sin, sin],
          "cos":     [cos, cos],
          "pow":     [],
          "exp":     [exp, exp],
          "log":     [log, log],
          "acos":    [acos],
          "acosh":   [acosh],
          "asin":    [asin],
          "asinh":   [asinh],
          "atan2":   [],
          "atan":    [atan],
          "atanh":   [atanh],
          "cbrt":       [],
          "cdfnorm":    [],
          "cdfnorminv": [],
          "ceil":       [],
          "cosd":       [],
          "cosh":    [cosh, cosh],
      }
  |}
    [];
  ()


let test_check_builtin_globals context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      reveal_type(...)
    |}
    ["Revealed type [-1]: Revealed type for `...` is `typing.Any`."];
  assert_type_errors
    {|
      reveal_type(__debug__)
    |}
    ["Revealed type [-1]: Revealed type for `__debug__` is `bool`."];
  assert_type_errors
    ~update_environment_with:
      [
        { handle = "__init__.pyi"; source = {|
              def bar() -> None: pass
            |} };
      ]
    {|
      # Builtin name
      locals()

      # Non-builtin name
      foo()

      # Defined in global with empty qualifier
      bar()
    |}
    ["Unbound name [10]: Name `foo` is used but not defined in the current scope."]


let () =
  "global"
  >::: [
         "check_with_qualification" >:: test_check_with_qualification;
         "check_globals" >:: test_check_globals;
         "check_builtin_globals" >:: test_check_builtin_globals;
       ]
  |> Test.run
