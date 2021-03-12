(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_boolean_literal context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Literal
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
      from typing import Literal

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
      "Unbound name [10]: Name `NonExistentClass` is used but not defined in the current scope.";
      "Invalid type [31]: Expression `NonExistentClass.ONE` is not a literal value.";
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


let test_ternary_with_literals context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Union, Literal

      def takes_literal(x: Union[Literal["a"], Literal["b"]]) -> None: ...

      some_bool: bool

      y = "a" if some_bool else "b"
      reveal_type(y)
      takes_literal(y)

      reveal_type("a" if some_bool else "b")
      takes_literal("a" if some_bool else "b")
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `str` (inferred: \
       `Union[typing_extensions.Literal['a'], typing_extensions.Literal['b']]`).";
      "Revealed type [-1]: Revealed type for `\"a\" if some_bool else \"b\"` is \
       `Union[typing_extensions.Literal['a'], typing_extensions.Literal['b']]`.";
    ];
  assert_type_errors
    {|
      from typing import Union
      from typing_extensions import Literal
      import enum

      class ActualEnum(enum.Enum):
        A = "a"
        B = "b"

      def takes_literal(x: Union[Literal[ActualEnum.A], Literal[ActualEnum.B]]) -> None: ...

      some_bool: bool

      y: Union[Literal[ActualEnum.A], Literal[ActualEnum.B]] = (
        ActualEnum.A if some_bool else ActualEnum.B
      )
      reveal_type(y)
      takes_literal(y)

      reveal_type(ActualEnum.A if some_bool else ActualEnum.B)
      takes_literal(ActualEnum.A if some_bool else ActualEnum.B)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is \
       `Union[typing_extensions.Literal[ActualEnum.A], typing_extensions.Literal[ActualEnum.B]]`.";
      "Revealed type [-1]: Revealed type for `test.ActualEnum.A if some_bool else \
       test.ActualEnum.B` is `Union[typing_extensions.Literal[ActualEnum.A], \
       typing_extensions.Literal[ActualEnum.B]]`.";
    ];
  ()


let test_bytes_literals context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Literal

      def expects_bytes(s: bytes) -> None: ...

      x: Literal[b"byte1"] = b"byte1"
      x: Literal[b"byte1"] = u"byte1"

      y: Literal[b"byte2"]
      expects_bytes(y)

      x2: Literal[b"byte1", b"byte2", u"string", 42] = b"byte1"
      x2 = b"byte1"
    |}
    [
      "Incompatible variable type [9]: x is declared to have type \
       `typing_extensions.Literal[b'byte1']` but is used as type \
       `typing_extensions.Literal['byte1']`.";
    ];
  ()


let test_literal_none context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Literal

      def expects_literal_none(s: Literal[None]) -> None: ...

      x: Literal[None] = None

      x2: Literal[42, None] = None
      reveal_type(x2)

      y: Literal[None]
      expects_literal_none(y)
      expects_literal_none(None)

      y2: None = y
    |}
    [
      "Revealed type [-1]: Revealed type for `x2` is \
       `typing.Optional[typing_extensions.Literal[42]]` (inferred: `None`).";
    ];
  ()


let test_literal_alias context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import *
      from typing_extensions import Literal as MyLiteral
      import typing_extensions

      x: int = 7
      valid_string_literal: MyLiteral["x"]

      class Foo:
        x: int = 7
        def treats_x_as_string_literal(self, a: MyLiteral["x"]) -> int: ...
    |}
    [];
  assert_type_errors
    {|
      from typing import Generic, TypeVar

      T = TypeVar("T")
      class NotLiteral(Generic[T]): ...

      x: int = 7
      treats_x_as_annotation: NotLiteral["x"]
    |}
    ["Undefined or invalid type [11]: Annotation `x` is not defined as a type."];
  assert_type_errors
    {|
      from typing import Generic, TypeVar

      T = TypeVar("T")
      class NotLiteral(Generic[T]): ...

      class Foo:
        x: int = 7

        def treats_x_as_attribute(self, a: NotLiteral["x"]) -> int: ...
    |}
    ["Undefined or invalid type [11]: Annotation `Foo.x` is not defined as a type."];
  ()


let () =
  "literal"
  >::: [
         "boolean_literal" >:: test_boolean_literal;
         "enumeration_literal" >:: test_enumeration_literal;
         "ternary_with_literals" >:: test_ternary_with_literals;
         "bytes_literals" >:: test_bytes_literals;
         "literal_none" >:: test_literal_none;
         "literal_alias" >:: test_literal_alias;
       ]
  |> Test.run
