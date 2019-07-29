(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
          global_number="a" # type: str
          return len(global_number)
    |}
    [];
  assert_type_errors
    {|
      global_number: int = 1
      def len(s: str) -> int:
        return 1
      def assign_outer() -> None:
          global_number="a" # type: str
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
              global_number="a" # type: str
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
  assert_type_errors
    {|
      constant: int = 1
      def foo() -> str:
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      constant: typing.Union[int, str] = 1
      def foo() -> int:
        return constant
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Union[int, str]`."];
  assert_type_errors
    {|
      constant: int = 1
      constant: str = ""
      def foo() -> str:
        return constant
    |}
    [];
  assert_type_errors
    {|
      constant = 1
      constant = ""
      def foo() -> str:
        return constant
    |}
    [ "Incompatible variable type [9]: constant is declared to have type `int` "
      ^ "but is used as type `str`.";
      "Incompatible return type [7]: Expected `str` but got `int`." ];
  assert_type_errors
    {|
      x = 1
      constant = x
      def foo() -> str:
        return constant
    |}
    [ "Missing global annotation [5]: Globally accessible variable `constant` has type `int` "
      ^ "but no type is specified.";
      "Incompatible return type [7]: Expected `str` but got `unknown`." ];
  assert_type_errors
    {|
      nasty_global = foo()
      def foo() -> int:
        a = nasty_global
        return 0
    |}
    [ "Missing global annotation [5]: Globally accessible variable `nasty_global` "
      ^ "has type `int` but no type is specified." ];
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
      x: typing.List[int]
      def foo() -> int:
        return x[0]
    |}
    [];
  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x[0:1]
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
    ~update_environment_with:[{ handle = "export.py"; source = "a, (b, c) = 1, (2, 3)" }]
    {|
      from export import b
      def foo() -> str:
        return b
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    ~update_environment_with:
      [ {
          handle = "export.py";
          source = "(a, b), (c, d): typing.Tuple[typing.Tuple[int, int], ...] = ...";
        } ]
    {|
      from export import b
      def foo() -> str:
        return b
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    ~update_environment_with:
      [ {
          handle = "export.py";
          source = {|
          class Foo:
            a, b = 1, 2
        |};
        } ]
    {|
      from export.Foo import a
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
    ["Incompatible return type [7]: Expected `str` but got `typing.Dict[str, int]`."];
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
    [ "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible variable type [9]: export.x is declared to have type `int` "
      ^ "but is used as type `str`.";
      "Incompatible return type [7]: Expected `str` but got `int`." ];
  assert_type_errors
    {|
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
    [ "Missing global annotation [5]: Globally accessible variable `x` has type `str` but no type \
       is specified.";
      "Missing global annotation [5]: Globally accessible variable `x` has type `None` but no \
       type is specified.";
      "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `y` is incomplete, "
      ^ "add an explicit annotation.";
      "Missing global annotation [5]: Globally accessible variable `y` has no type specified.";
      "Incompatible return type [7]: Expected `typing.List[int]` but got `unknown`." ];
  assert_type_errors {|
      A = typing.Mapping[int, str]
    |} [];
  assert_type_errors
    {|
      A = MappBoo[int, str]
    |}
    [ "Undefined name [18]: Global name `MappBoo` is not defined, or there is at least one \
       control flow path that doesn't define `MappBoo`.";
      "Missing global annotation [5]: Globally accessible variable `A` has no type specified." ];
  assert_type_errors
    {|
      MyType = typing.List[typing.Any]
    |}
    ["Prohibited any [33]: Explicit annotation for `MyType` cannot contain `Any`."]


let () =
  "global"
  >::: [ "check_with_qualification" >:: test_check_with_qualification;
         "check_globals" >:: test_check_globals ]
  |> Test.run
