(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_scheduling context =
  let assert_type_errors = assert_type_errors ~context in
  (* Top-level is scheduled. *)
  assert_type_errors
    "'string' + 1"
    ["Unsupported operand [58]: `+` is not supported for operand types `str` and `int`."];

  (* Functions are scheduled. *)
  assert_type_errors
    {|
      def bar() -> None: ...
      def foo() -> None:
        'string' + 1
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `str` and `int`."];
  assert_type_errors
    {|
      def bar() -> None:
        def foo() -> None:
          'string' + 1
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `str` and `int`."];

  (* Class bodies are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        'string' + 1
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `str` and `int`."];

  (* Methods are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> None:
          'string' + 1
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `str` and `int`."];

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
      "Incompatible parameter type [6]: In call `expect_string`, for 1st positional only parameter \
       expected `str` but got `int`.";
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
      "Incompatible parameter type [6]: In call `expect_string`, for 1st positional only parameter \
       expected `str` but got `int`.";
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
      class Exception: pass
      class Foo(Exception): pass
      class Bar(Exception): pass
      def foo() -> None:
        try:
          pass
        except (Foo, Bar) as e:
          reveal_type(e)
    |}
    ["Revealed type [-1]: Revealed type for `e` is `typing.Union[Bar, Foo]`."];
  assert_type_errors
    {|
      import typing
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
      "Incompatible parameter type [6]: In call `use`, for 1st positional only parameter expected \
       `int` but got `str`.";
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
    ["Unsupported operand [58]: `>` is not supported for operand types `Optional[int]` and `int`."];
  assert_type_errors
    {|
      import typing
      def foo() -> int:
        x: typing.Optional[int]
        y: int
        z = x if x else y
        return z
    |}
    [];
  assert_type_errors
    {|
      import typing
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
      import typing
      def foo() -> int:
        y: typing.Optional[int]
        x: int
        return y if x else 5
    |}
    ["Incompatible return type [7]: Expected `int` but got `Optional[int]`."];
  assert_type_errors
    {|
      from builtins import int_to_str
      import typing
      def foo(a:typing.Optional[int])->str:
        return int_to_str(a) if a else ""
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
          int_to_int(x) if x else 0
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      import typing
      def foo(x: typing.Optional[int]) -> int:
          return int_to_int(x if x is not None else 1)
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      import typing
      def foo(x: typing.Optional[int]) -> int:
        a, b = ("hi", int_to_int(x) if x is not None else 1)
        return b
    |}
    [];
  assert_type_errors
    {|
      import typing
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
      import typing
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
    ["Uninitialized local [61]: Local variable `result` is undefined, or not always defined."];
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = narnia()
        return result
    |}
    [
      "Unbound name [10]: Name `narnia` is used but not defined in the current scope.";
      "Uninitialized local [61]: Local variable `result` is undefined, or not always defined.";
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
      "Unbound name [10]: Name `narnia` is used but not defined in the current scope.";
      "Uninitialized local [61]: Local variable `result` is undefined, or not always defined.";
    ];
  assert_type_errors
    {|
      def foo() -> int:
        assert unknown is None or 1
        return unknown
    |}
    [
      "Unbound name [10]: Name `unknown` is used but not defined in the current scope.";
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
      from builtins import int_to_int
      def foo() -> None:
        def nested() -> None:
          int_to_int(1.0)
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `float`.";
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `float`.";
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
      import collections
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
      "Incompatible return type [7]: Expected `Word` but got implicit return value of `None`.";
    ];
  assert_type_errors
    ~handle:"shadowing.py"
    {|
      def shadowing(i: int) -> None: ...
      shadowing('asdf')
    |}
    [
      "Incompatible parameter type [6]: In call `shadowing`, for 1st positional only parameter \
       expected `int` but got `str`.";
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
    ];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo(x:int) -> None:
        match x:
          case _:
            def nested() -> None:
              int_to_int(1.0)
            int_to_int("hi")
    |}
    [
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `float`.";
      "Incompatible parameter type [6]: In call `int_to_int`, for 1st positional only parameter \
       expected `int` but got `str`.";
    ]


let test_check_while context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def produce() -> bool: ...

      def foo() -> int:
        x = "A"
        while(produce()):
         if (produce()):
          x = 1
          break
        else:
          x = 2
        reveal_type(x)
        return x
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      def foo() -> None:
        x = "A"
        while True:
          x = 1
          break
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[1]`."];
  assert_type_errors
    {|
      from typing import Optional
      def produces() -> Optional[int]: ...
      def foo() -> None:
        x = None
        while not x:
          x = produces()
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      from typing import Optional
      def produces() -> Optional[int]: ...
      def foo() -> None:
        x = None
        i = 0
        while not x:
          x = produces()
          if i > 10:
           break
          i += 1
        else:
          reveal_type(x)
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `Optional[int]`.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        while True:
          print("infinity!")
        reveal_type("never reached")
    |}
    [];
  (* https://github.com/facebook/pyre-check/issues/251 *)
  assert_type_errors
    {|
      def random() -> bool: ...

      while True:
          if random():
              some_var = True
              break

      print(some_var)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        while x < 1.0:
          pass
    |}
    [];
  assert_type_errors
    {|
      from typing import Dict, Any

      def foo() -> Dict[str, Any]: ...
      def bar() -> None:
        try:
          x = foo()
        except Exception as ex:
          x = {"a": False}
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Dict[str, typing.Any]`."];
  assert_type_errors
    {|
      from typing import Dict, Any

      def foo() -> Dict[str, Any]: ...
      def bar(b: bool) -> None:
        if b:
          x = foo()
        else:
          x = {"a": False}
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Dict[str, typing.Any]`."];
  assert_type_errors
    {|
      from typing import NoReturn

      def foo() -> NoReturn: ...

      def bar() -> None:
        try:
          reveal_type(1)
        except:
          pass
        foo()

      def baz() -> None:
        try:
          reveal_type(2)
        except:
          pass
      |}
    [
      "Revealed type [-1]: Revealed type for `1` is `typing_extensions.Literal[1]`.";
      "Revealed type [-1]: Revealed type for `2` is `typing_extensions.Literal[2]`.";
    ];
  ()


let () =
  "controlFlow"
  >::: [
         "scheduling" >:: test_scheduling;
         "check_excepts" >:: test_check_excepts;
         "check_ternary" >:: test_check_ternary;
         "check_unbound_variables" >:: test_check_unbound_variables;
         "check_nested" >:: test_check_nested;
         "check_while" >:: test_check_while;
       ]
  |> Test.run
