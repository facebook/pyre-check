(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_suppression =
  test_list
    [
      (* suppressed in unsafe mode *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
      class C:
        def f(self) -> None:
          pass

      class D(C):
        def f(self) -> None:
          pass
    |}
           [];
      (* suppressed in strict mode until we codemod *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      class C:
        def f(self) -> None:
          pass

      class D(C):
        def f(self) -> None:
          pass
    |}
           [];
      (* show error in strict mode if flag is set *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           ~enable_strict_override_check:true
           {|
      class C:
        def f(self) -> None:
          pass

      class D(C):
        def f(self) -> None:
          pass
    |}
           [
             "Invalid override [40]: `test.D.f` is not decorated with @override, but overrides a \
              method with the same name in superclasses of `D`.";
           ];
      (* do not show error in unsafe mode if flag is set *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           ~enable_strict_override_check:true
           {|
      class C:
        def f(self) -> None:
          pass

      class D(C):
        def f(self) -> None:
          pass
    |}
           [];
    ]


let test_extra_overriding_parameter =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
     from abc import ABC, abstractmethod
     class Base(ABC):
       @abstractmethod
       def foo(self, a: int) -> None:
          pass
     class Derived(Base):
       def foo(
          self,
          b: dict[str, str],
          a: int,
       ) -> None:
          pass
    |}
           [
             "Inconsistent override [14]: `test.Derived.foo` overrides method defined in `Base` \
              inconsistently. Could not find parameter `b` in overridden signature.";
             "Invalid override [40]: `test.Derived.foo` is not decorated with @override, but \
              overrides a method with the same name in superclasses of `Derived`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      class C:
        def f(self) -> None:
          pass

      class D(C):
        def f(self, x: int) -> None:
          pass
    |}
           [
             "Inconsistent override [14]: `test.D.f` overrides method defined in `C` \
              inconsistently. Could not find parameter `x` in overridden signature.";
             "Invalid override [40]: `test.D.f` is not decorated with @override, but overrides a \
              method with the same name in superclasses of `D`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class C:
        def f(self) -> None:
          pass

      class D(C):
        @override
        def f(self, x: int) -> None:
          pass
    |}
           [
             "Inconsistent override [14]: `test.D.f` overrides method defined in `C` \
              inconsistently. Could not find parameter `x` in overridden signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      class Obj:
        def __format__(self, __format_spec: str) -> str:
          return 'hello'

      class Data(Obj):
        def __format__(self, format_spec: str) -> str:
          return 'hello ' + format_spec
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      class Data:
        def __format__(self, format_spec: str) -> str:
          return 'hello ' + format_spec
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      import typing
      from typing import override
      T = typing.TypeVar("T")

      def decorate(f: typing.Callable[['C', T], None]) -> typing.Callable[['C', T], None]:
        ...

      class C:
        @decorate
        def f(self, x: int) -> None: # registered type is typing.Callable[[C, int], None]
          pass

      class D(C):
        @override
        def f(self, x: int, y: int, z: int) -> None:
          pass
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import TypeVar, override
      T = TypeVar("T")

      def decorate(f: T) -> T:
        ...

      class C:
        def f(self) -> None:
          pass

      class D(C):
        @decorate
        @override
        def f(self, x: int) -> None:
          pass
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class C:
        def f(self) -> None:
          pass

      class D(C):
        @classmethod
        @override
        def f(self, x: int) -> None:
          pass
    |}
           [
             "Inconsistent override [14]: `test.D.f` overrides method defined in `C` \
              inconsistently. Could not find parameter `x` in overridden signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      import abc
      class Abstract:
          @abc.abstractclassmethod
          def a_thing(cls) -> None:
              ...

      class Intermediate(Abstract):
        ...

      class Concrete(Intermediate):
          @classmethod
          @override
          def a_thing(cls) -> None:
              ...
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      import abc
      class Abstract:
          @abc.abstractclassmethod
          def a_thing(self) -> None:
              ...


      class Concrete(Abstract):
          @override
          def a_thing(self, param: int) -> None:
              ...
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          @override
          def test(self, n: int) -> int:
              return n
    |}
           [
             "Inconsistent override [14]: `test.B.test` overrides method defined in `A` \
              inconsistently. Could not find parameter `n` in overridden signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self, n: int, /) -> int:
              return 5


      class B(A):
          @override
          def test(self, n: int, m: float, /) -> int:
              return n
    |}
           [
             "Inconsistent override [14]: `test.B.test` overrides method defined in `A` \
              inconsistently. Could not find parameter of type `float` at index 2 in overridden \
              signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self, __n: int) -> int:
              return 5


      class B(A):
          @override
          def test(self, __n: int, __m: float) -> int:
              return 5
    |}
           [
             "Inconsistent override [14]: `test.B.test` overrides method defined in `A` \
              inconsistently. Could not find parameter of type `float` at index 2 in overridden \
              signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self, n: int, /) -> int:
              return 5


      class B(A):
          @override
          def test(self, m: int, /) -> int:
              return m
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          @override
          def test(self, *, n: int) -> int:
              return n
    |}
           [
             "Inconsistent override [14]: `test.B.test` overrides method defined in `A` \
              inconsistently. Could not find parameter `n` in overridden signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          @override
          def test(self, n: int) -> int:
              return n
    |}
           [
             "Inconsistent override [14]: `test.B.test` overrides method defined in `A` \
              inconsistently. Could not find parameter `n` in overridden signature.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import Dict, Any, override
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          @override
          def test(self, **kwargs: Dict[str, Any]) -> int:
              return 5
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import List, Any, override
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          @override
          def test(self, *args: List[Any]) -> int:
              return 5
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
      from typing import override
      class A:
          def test(self) -> int:
              return 5


      class B(A):
          @override
          def test(self, n: int = 5) -> int:
              return n
    |}
           [];
    ]


let () = "override" >::: [test_suppression; test_extra_overriding_parameter] |> Test.run
