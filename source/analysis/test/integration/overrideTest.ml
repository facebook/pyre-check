(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_extra_overriding_parameter context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")

      # lets AstLintRule ignore these no_op implementations
      def decorate(f: typing.Callable[['C', T], None]) -> typing.Callable[['C', T], None]:
        ...

      class C:
        @decorate
        def f(self, x: int) -> None: # registered type is typing.Callable[[C, int], None]
          pass

      class D(C):
        def f(self, y: int) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      import abc
      class Abstract:
          @abc.abstractclassmethod
          def a_thing(cls) -> None:
              ...

      class Intermediate(Abstract):
        ...

      class Concrete(Intermediate):
          @classmethod
          def a_thing(cls) -> None:
              ...
    |}
    [];
  assert_type_errors
    {|
      import abc
      class Abstract:
          @abc.abstractclassmethod
          def a_thing(self) -> None:
              ...


      class Concrete(Abstract):
          def a_thing(self, param: int) -> None:
              ...
    |}
    [];
  assert_type_errors
    {|
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          def test(self, n: int) -> int:
              return n
    |}
    [
      "Inconsistent override [14]: `test.B.test` overrides method defined in `A` inconsistently. \
       Could not find parameter `n` in overridden signature.";
    ];
  assert_type_errors
    {|
      class A:
          def test(self, /, n: int) -> int:
              return 5


      class B(A):
          def test(self, /, n: int, m: int) -> int:
              return n
    |}
    [
      "Inconsistent override [14]: `test.B.test` overrides method defined in `A` inconsistently. \
       Could not find parameter `m` in overridden signature.";
    ];
  assert_type_errors
    {|
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          def test(self, *, n: int) -> int:
              return n
    |}
    [
      "Inconsistent override [14]: `test.B.test` overrides method defined in `A` inconsistently. \
       Could not find parameter `n` in overridden signature.";
    ];
  assert_type_errors
    {|
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          def test(self, n: int) -> int:
              return n
    |}
    [
      "Inconsistent override [14]: `test.B.test` overrides method defined in `A` inconsistently. \
       Could not find parameter `n` in overridden signature.";
    ];
  assert_type_errors
    {|
      from typing import Dict, Any
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          def test(self, **kwargs: Dict[str, Any]) -> int:
              return 5
    |}
    [];
  assert_type_errors
    {|
      from typing import List, Any
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          def test(self, *args: List[Any]) -> int:
              return 5
    |}
    [];
  assert_type_errors
    {|
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          def test(self, n: int = 5) -> int:
              return n
    |}
    [];
  ()


let () =
  "override" >::: ["extra_overriding_parameter" >:: test_extra_overriding_parameter] |> Test.run
