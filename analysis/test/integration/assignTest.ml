(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_assign context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x = 'string'  # Reassignment is okay.
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: str = 1
        reveal_type(x)
        x = 1
    |}
    [ "Incompatible variable type [9]: x is declared to have type `str` "
      ^ "but is used as type `int`.";
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Incompatible variable type [9]: x is declared to have type `str` "
      ^ "but is used as type `int`." ];
  assert_default_type_errors {|
      def foo(x: typing.Any) -> None:
        y: int = x
    |} [];
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x += 'asdf'
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`." ];

  (* Prune `undeclared` from assignments. *)
  assert_type_errors
    {|
      def foo() -> None:
        y = x
        z = y
    |}
    [ "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ];
  assert_type_errors
    {|
      def foo(a: bool) -> None:
        if a:
          x = 12
        y = x
        z = y
    |}
    [ "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ];
  assert_type_errors
    {|
      def foo() -> None:
        y = [x]
        z = y
    |}
    [ "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ];
  assert_type_errors
    {|
      def foo(a: bool) -> None:
        if a:
          x = 12
        y = [x]
        z = y
    |}
    [ "Undefined name [18]: Global name `x` is not defined, or there is at least one control flow \
       path that doesn't define `x`." ];
  assert_type_errors
    {|
      from typing import Final
      x: Final[int] = 3
      x = 200
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
  assert_type_errors
    {|
      from typing import Final
      i = 0
      while i < 10:
        i += 1
        x: Final[int] = i
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
  assert_type_errors
    {|
      from typing import Final
      class A:
        x: Final[int] = 10
      A.x = 20
    |}
    ["Invalid assignment [41]: Cannot reassign final attribute `A.x`."];
  assert_type_errors
    {|
      from typing import ClassVar
      class Base:
          y: ClassVar[int] = 0
      Base.y = 100 # ok
      b = Base()
      b.y = 12 # error
    |}
    [ "Invalid assignment [41]: Assigning to class variable through instance, did you mean to \
       assign to `Base.y` instead?" ];
  assert_type_errors
    {|
      from typing import ClassVar
      class Base:
          y: ClassVar[int] = 0

      x = Base
      x.y = 100
    |}
    [];
  assert_type_errors
    {|
      from dataclasses import dataclass
      @dataclass(frozen=True)
      class A:
          foo:int = 1
      a = A()
      a.foo = 2
    |}
    ["Invalid assignment [41]: `a.foo` cannot be reassigned. It is a read-only property."];
  assert_type_errors
    {|
      class A:
          @property
          def foo(self) -> int:
            ...
      a = A()
      a.foo = 1
    |}
    ["Invalid assignment [41]: `a.foo` cannot be reassigned. It is a read-only property."];
  assert_type_errors
    {|
      class A:
          @property
          def foo(self) -> int:
            ...
          @foo.setter
          def foo(self, value: int) -> None:
            ...
      a = A()
      a.foo = 1
    |}
    []


let () = "assign" >::: ["check_assign" >:: test_check_assign] |> Test.run
