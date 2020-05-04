(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_boolean_literal context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing_extensions import Literal
      def foo(b: Literal[True]) -> None: ...
      def bar() -> None:
        foo(True)
    |}
    [];
  assert_type_errors
    {|
      from typing_extensions import Literal
      def foo(b: Literal[True]) -> None: ...
      def bar() -> None:
        foo(False)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing_extensions.Literal[True]` for 1st \
       positional only parameter to call `foo` but got `typing_extensions.Literal[False]`.";
    ];
  assert_type_errors
    {|
      from typing_extensions import Literal
      def foo(b: Literal[True]) -> None: ...
      def bar(b: bool) -> None:
        foo(b)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing_extensions.Literal[True]` for 1st \
       positional only parameter to call `foo` but got `bool`.";
    ]


let test_enumeration_literal context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import enum
      from typing_extensions import Literal

      class MyEnum(enum.Enum):
        HELLO = "hello"
        WORLD = "world"

      x1: Literal[MyEnum.HELLO] = MyEnum.HELLO

      x2: Literal[MyEnum.HELLO] = "hello"
      x3: Literal[MyEnum.HELLO] = MyEnum.WORLD
      x4: Literal[MyEnum.HELLO] = "world"
      x5: Literal[MyEnum.HELLO] = 1
    |}
    [
      "Incompatible variable type [9]: x2 is declared to have type \
       `typing_extensions.Literal[MyEnum.HELLO]` but is used as type \
       `typing_extensions.Literal['hello']`.";
      "Incompatible variable type [9]: x3 is declared to have type \
       `typing_extensions.Literal[MyEnum.HELLO]` but is used as type \
       `typing_extensions.Literal[MyEnum.WORLD]`.";
      "Incompatible variable type [9]: x4 is declared to have type \
       `typing_extensions.Literal[MyEnum.HELLO]` but is used as type \
       `typing_extensions.Literal['world']`.";
      "Incompatible variable type [9]: x5 is declared to have type \
       `typing_extensions.Literal[MyEnum.HELLO]` but is used as type \
       `typing_extensions.Literal[1]`.";
    ];
  assert_type_errors
    {|
      import enum
      from typing_extensions import Literal

      class MyIntEnum(enum.Enum):
        ONE = 1
        TWO = 2
      x1: Literal[MyIntEnum.ONE] = MyIntEnum.ONE

      x2: Literal[MyIntEnum.ONE] = 1
      x3: Literal[MyIntEnum.ONE] = MyIntEnum.TWO
      x4: Literal[MyIntEnum.ONE] = 2
      x5: Literal[MyIntEnum.ONE] = "foo"
    |}
    [
      "Incompatible variable type [9]: x2 is declared to have type \
       `typing_extensions.Literal[MyIntEnum.ONE]` but is used as type \
       `typing_extensions.Literal[1]`.";
      "Incompatible variable type [9]: x3 is declared to have type \
       `typing_extensions.Literal[MyIntEnum.ONE]` but is used as type \
       `typing_extensions.Literal[MyIntEnum.TWO]`.";
      "Incompatible variable type [9]: x4 is declared to have type \
       `typing_extensions.Literal[MyIntEnum.ONE]` but is used as type \
       `typing_extensions.Literal[2]`.";
      "Incompatible variable type [9]: x5 is declared to have type \
       `typing_extensions.Literal[MyIntEnum.ONE]` but is used as type \
       `typing_extensions.Literal['foo']`.";
    ];
  assert_type_errors
    {|
      import enum
      from typing_extensions import Literal

      class MyEnum(enum.Enum):
        HELLO = "hello"
        WORLD = "world"

      def foo(x: Literal[MyEnum.HELLO]) -> None: ...

      foo(MyEnum.HELLO)
      foo(MyEnum.WORLD)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing_extensions.Literal[MyEnum.HELLO]` for 1st \
       positional only parameter to call `foo` but got `typing_extensions.Literal[MyEnum.WORLD]`.";
    ];
  assert_type_errors
    {|
      import enum
      from typing_extensions import Literal

      class MyEnum(enum.Enum):
        HELLO = "hello"
        WORLD = "world"

      x1: Literal[MyEnum.HELLO, MyEnum.WORLD] = MyEnum.HELLO
      x2: Literal[MyEnum.HELLO, MyEnum.WORLD] = MyEnum.WORLD
    |}
    [];
  assert_type_errors
    {|
      import enum
      class A(enum.Enum):
          ONE = 1
          TWO = 2
      def expects_string(x: str) -> None: ...

      expects_string(A.ONE)
    |}
    [
      "Incompatible parameter type [6]: Expected `str` for 1st positional only parameter to call \
       `expects_string` but got `A`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      from typing_extensions import Literal
      import enum
      class NotEnum:
          ONE: int = 1
          TWO: int = 2
      class ActualEnum(enum.Enum):
        ONE = 1
      x1: Literal[NonExistentClass]
      x2: Literal[NonExistentClass.ONE]
      x3: Literal[NotEnum.ONE]
      x4: Tuple[Literal[NotEnum.ONE], Tuple[Literal[NotEnum.TWO]]]
      x5: Literal[ActualEnum.NON_EXISTENT_MEMBER]
      x6: Literal[ActualEnum.NON_EXISTENT_MEMBER2, ActualEnum.NON_EXISTENT_MEMBER3]
    |}
    [
      "Invalid type [31]: Expression `typing_extensions.Literal[NonExistentClass]` is not a valid \
       type.";
      "Undefined or invalid type [11]: Annotation `NonExistentClass` is not defined as a type.";
      "Invalid type [31]: Expression `typing_extensions.Literal[NotEnum.ONE]` is not a valid type.";
      "Invalid type [31]: Expression `typing_extensions.Literal[NotEnum.ONE]` is not a valid type.";
      "Invalid type [31]: Expression `typing_extensions.Literal[NotEnum.TWO]` is not a valid type.";
      "Invalid type [31]: Expression `typing_extensions.Literal[ActualEnum.NON_EXISTENT_MEMBER]` \
       is not a valid type.";
      "Invalid type [31]: Expression `typing_extensions.Literal[ActualEnum.NON_EXISTENT_MEMBER2]` \
       is not a valid type.";
      "Invalid type [31]: Expression `typing_extensions.Literal[ActualEnum.NON_EXISTENT_MEMBER3]` \
       is not a valid type.";
    ];
  assert_type_errors
    {|
      from typing import Final, List
      from typing_extensions import Literal
      class NotEnum:
          ONE: int = 1
      def foo(x: Literal[NotEnum.ONE]) -> None: ...
    |}
    ["Invalid type [31]: Expression `typing_extensions.Literal[NotEnum.ONE]` is not a valid type."];
  ()


let () =
  "literal"
  >::: [
         "boolean_literal" >:: test_boolean_literal;
         "enumeration_literal" >:: test_enumeration_literal;
       ]
  |> Test.run
