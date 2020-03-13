(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_scheduling context =
  let assert_type_errors = assert_type_errors ~context in
  (* Top-level is scheduled. *)
  assert_type_errors
    "'string' + 1"
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int.__radd__` but got `str`.";
    ];

  (* Functions are scheduled. *)
  assert_type_errors
    {|
      def bar() -> None: ...
      def foo() -> None:
        'string' + 1
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int.__radd__` but got `str`.";
    ];
  assert_type_errors
    {|
      def bar() -> None:
        def foo() -> None:
          'string' + 1
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int.__radd__` but got `str`.";
    ];

  (* Class bodies are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        'string' + 1
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int.__radd__` but got `str`.";
    ];

  (* Methods are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> None:
          'string' + 1
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int.__radd__` but got `str`.";
    ];

  (* Property getters and setters are both scheduled *)
  assert_type_errors
    {|
     class Foo:
       @property
       def foo(self) -> int:
         return "abc"
       @foo.setter
       def foo(self, value: int) -> None:
         return 42
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Incompatible return type [7]: Expected `None` but got `int`.";
    ];

  assert_type_errors
    {|
      def foo() -> None:
        def bar() -> None:
          bar()
    |}
    [];

  (* Functions defined after try/except blocks are typechecked. *)
  assert_type_errors
    {|
      class Exception: pass
      try:
        pass
      except Exception:
        pass

      def expect_string(a: str) -> None:
        pass
      def foo() -> None:
        expect_string(1)
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st positional only parameter to call `expect_string` but got `int`.";
    ];
  assert_type_errors
    {|
      try:
        pass
      finally:
        pass

      def expect_string(a: str) -> None:
        pass
      def foo() -> None:
        expect_string(1)
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `str` for 1st positional only parameter to call `expect_string` but got `int`.";
    ]


let test_check_excepts context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class Exception: pass
      def takes_exception(e: Exception) -> None: pass
      def foo() -> None:
        try:
          pass
        except Exception as e:
          takes_exception(e)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Optional[int]:
        try:
          x = 1
        except:
          return None
        else:
          return x
    |}
    [];
  assert_type_errors
    {|
      def use(i: int) -> None: pass
      def foo(x: bool) -> None:
        try:
          pass
        finally:
          if x:
            use("error")
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `use` "
      ^ "but got `str`.";
    ]


let test_check_ternary context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Optional
      def foo(x: Optional[int]) -> int:
        if x > 42:
          return 0
        else:
          return 1
    |}
    [
      "Undefined attribute [16]: `Optional` has no attribute `__gt__`.";
      (* TODO: We might want to get rid of this in the future *)
      "Undefined attribute [16]: `Optional` has no attribute `__le__`.";
    ];
  assert_type_errors
    {|
      def foo() -> int:
        x: typing.Optional[int]
        y: int
        z = x if x else y
        return z
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        y: typing.Optional[int]
        return y if y else 5
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> int:
        if x > 10:
          y = None
        else:
          y = 5
        y = y if y else 0
        return y
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        y: typing.Optional[int]
        x: int
        return y if x else 5
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      def foo(a:typing.Optional[int])->str:
        return int_to_str(a) if a else ""
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
          int_to_int(x) if x else 0
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
          return int_to_int(x if x is not None else 1)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        a, b = ("hi", int_to_int(x) if x is not None else 1)
        return b
    |}
    [];
  assert_type_errors
    {|
      def f(s: str) -> None:
        pass

      def pick_alternative3(s: typing.Optional[str]) -> None:
        x = "foo" if s is None else s
        f(x)

      def pick_target(s: typing.Optional[str]) -> None:
        f(s if s is not None else "foo")

      def pick_target2(s: typing.Optional[str]) -> None:
        f(s if s else "foo")

      def pick_target3(s: typing.Optional[str]) -> None:
        x = s if s is not None else "foo"
        f(x)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[bytes]) -> None: ...
      def bar() -> None:
        a: typing.Union[int, bytes]
        foo(x=a if isinstance(a, bytes) else None)
    |}
    []


let test_check_unbound_variables context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = 1
        else:
          other = 1
        return result
    |}
    [
      "Incompatible return type [7]: Expected `int` but got "
      ^ "`typing.Union[int, typing.Undeclared]`.";
      "Undefined name [18]: Global name `result` is not defined, or there is at least one control \
       flow path that doesn't define `result`.";
    ];
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = narnia()
        return result
    |}
    [
      "Undefined name [18]: Global name `narnia` is not defined, or there is at least one control \
       flow path that doesn't define `narnia`.";
      "Incompatible return type [7]: Expected `int` but got "
      ^ "`typing.Union[typing.Undeclared, unknown]`.";
      "Undefined name [18]: Global name `result` is not defined, or there is at least one control \
       flow path that doesn't define `result`.";
    ];
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = narnia()
        else:
          other = 1
        return result
    |}
    [
      "Undefined name [18]: Global name `narnia` is not defined, or there is at least one control \
       flow path that doesn't define `narnia`.";
      "Incompatible return type [7]: Expected `int` but got "
      ^ "`typing.Union[typing.Undeclared, unknown]`.";
      "Undefined name [18]: Global name `result` is not defined, or there is at least one control \
       flow path that doesn't define `result`.";
    ];
  assert_type_errors
    {|
      def foo() -> int:
        assert unknown is None or 1
        return unknown
    |}
    [
      "Undefined name [18]: Global name `unknown` is not defined, or there is at least one control \
       flow path that doesn't define `unknown`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: bool = False
        def foo(self) -> int:
          if not self.attribute:
            self.attribute = True
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `int` but got `bool`."]


let test_check_nested context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> None:
        def nested() -> None:
          int_to_int(1.0)
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int_to_int` but got `float`.";
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `int_to_int` but got `float`.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        def g() -> None:
          return
        a = g()
    |}
    [];
  assert_type_errors
    {|
      class Derp:
          Word = collections.namedtuple("word", ("verb", "noun"))
      def foo() -> Derp.Word: pass
    |}
    [
      "Missing attribute annotation [4]: Attribute `noun` of class `Derp.Word` must have a type \
       other than `Any`.";
      "Missing attribute annotation [4]: Attribute `verb` of class `Derp.Word` must have a type \
       other than `Any`.";
      "Missing parameter annotation [2]: Parameter `noun` must have a type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `verb` must have a type other than `Any`.";
      "Incompatible return type [7]: Expected `Derp.Word` but got "
      ^ "implicit return value of `None`.";
    ];
  assert_type_errors
    ~handle:"shadowing.py"
    {|
      def shadowing(i: int) -> None: ...
      shadowing('asdf')
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st positional only parameter to call `shadowing` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        def bar() -> None:
          reveal_type(x)
        bar()
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      def foo() -> None:
        def baz():
          pass
        def bar() -> None:
          baz()  # Don't warn on captured functions
        bar()
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> None:
          def bar() -> None:
            reveal_type(self)  # Don't warn on captured self
          bar()
    |}
    ["Revealed type [-1]: Revealed type for `self` is `Foo`."];
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls) -> None:
          def bar() -> None:
            reveal_type(cls)  # Don't warn on captured cls
          bar()
    |}
    ["Revealed type [-1]: Revealed type for `cls` is `typing.Type[Foo]`."];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        def bar() -> None:
          reveal_type(x)
        bar()
    |}
    [
      "Missing annotation for captured variable [53]: Captured variable `x` is not annotated.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
    ];
  assert_default_type_errors
    {|
      def foo() -> None:
        x = 1
        def bar() -> None:
          reveal_type(x)
        bar()
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        def bar() -> None:
          def baz() -> None:
            reveal_type(x)
          baz()
        bar()
    |}
    [
      "Missing annotation for captured variable [53]: Captured variable `x` is not annotated.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        x, y = (1, 2)
        def bar() -> None:
          reveal_type(x)
        bar()
    |}
    [
      "Missing annotation for captured variable [53]: Captured variable `x` is not annotated.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      def can_fail() -> None:
        try:
          x = 3
        except:
          pass
        always_declared = 4
        def bar() -> int:
          return always_declared
    |}
    [
      "Missing annotation for captured variable [53]: Captured variable `always_declared` is not \
       annotated.";
    ]


let () =
  "controlFlow"
  >::: [
         "scheduling" >:: test_scheduling;
         "check_excepts" >:: test_check_excepts;
         "check_ternary" >:: test_check_ternary;
         "check_unbound_variables" >:: test_check_unbound_variables;
         "check_nested" >:: test_check_nested;
       ]
  |> Test.run
