(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_implementation context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      from typing import overload
      @overload
      def foo() -> None:
          pass

      @overload
      def foo() -> None:
          pass
    |}
    ["Missing overload implementation [42]: Overloaded function `foo` must have an implementation."];
  assert_default_type_errors
    ~handle:"stub.pyi"
    {|
      from typing import overload
      @overload
      def foo() -> None:
          pass

      @overload
      def foo() -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      from typing import overload
      @overload
      def foo() -> None:
          pass

      def foo() -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      from typing import overload
      @overload
      def foo(x: int) -> bool:
          pass

      @overload
      def foo(x: object) -> int:
          pass

      def foo(x: object) -> float:
          return 1
    |}
    [
      "Incompatible overload [43]: The return type of overloaded function `foo` (`bool`) is \
       incompatible with the return type of the implementation (`float`).";
    ];
  assert_type_errors
    {|
         from typing import overload

         @overload
         def foo(bar: object, x: object) -> int:
             pass

         @overload
         def foo(bar: int, x: int) -> str:
             pass

         @overload
         def foo(bar: str, x: str) -> object:
             pass

         def foo(bar: object, x: object) -> object: pass
       |}
    [
      "Incompatible overload [43]: The overloaded function `foo` on line 9 will never be matched. \
       The signature `(bar: object, x: object) -> int` is the same or broader.";
      "Incompatible overload [43]: The overloaded function `foo` on line 13 will never be matched. \
       The signature `(bar: object, x: object) -> int` is the same or broader.";
    ];
  assert_type_errors
    {|
         from typing import overload

         @overload
         def foo(bar: int, x: int) -> str:
             pass

         @overload
         def foo(bar: str, x: str) -> object:
             pass

         @overload
         def foo(bar: object, x: object) -> int:
             pass

         def foo(bar: object, x: object) -> object: pass
       |}
    [];

  assert_type_errors
    {|
         from typing import overload

         @overload
         def foo(bar: int, x: int) -> str:
             pass

         @overload
         def foo(bar: str, x: str, y: int) -> object:
             pass

         def foo(bar: object, x: object, y:object) -> object: pass
       |}
    [
      "Incompatible overload [43]: The implementation of `foo` does not accept all possible \
       arguments of overload defined on line `5`.";
    ];

  assert_type_errors
    {|
      from typing import overload
      @overload
      def foo(bar: object) -> None:
        pass
      def foo(bar: int) -> None:
        pass
    |}
    [
      "Incompatible overload [43]: The implementation of `foo` does not accept all possible \
       arguments of overload defined on line `4`.";
    ];

  assert_type_errors
    {|
      from typing import overload
      @overload
      def foo() -> None:
        pass
      def foo(bar: int) -> None:
        pass
    |}
    [
      "Incompatible overload [43]: The implementation of `foo` does not accept all possible \
       arguments of overload defined on line `4`.";
    ];

  assert_type_errors
    {|
      from typing import overload

      @overload
      def f( **kwargs:int) -> int:
          pass

      def f( *args: int, **kwargs: int) -> int:
          return 1
    |}
    [];

  ()


let test_check_decorated_overloads context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import overload, Callable
      def decorator(x: object) -> Callable[[int], int]: ...

      @overload
      @decorator
      def foo(a: str) -> str: ...

      @overload
      @decorator
      def foo(a: bool) -> bool: ...

      @decorator
      def foo(a: object) -> int:
        return 1

      reveal_type(foo)
    |}
    [
      "Incompatible overload [43]: The return type of overloaded function `foo` (`str`) is \
       incompatible with the return type of the implementation (`int`).";
      "Incompatible overload [43]: The return type of overloaded function `foo` (`bool`) is \
       incompatible with the return type of the implementation (`int`).";
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable[[int], int]`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Callable
      def decoratorA(x: object) -> Callable[[int], int]: ...
      def decoratorB(x: object) -> Callable[[int], int]: ...

      @overload
      @decoratorA
      def foo(a: str) -> str: ...

      @overload
      @decoratorB
      def foo(a: bool) -> bool: ...

      @decoratorB
      def foo(a: object) -> object:
        return 1

      reveal_type(foo)
    |}
    [
      "Incompatible overload [43]: This definition does not have the same decorators as the \
       preceding overload(s)";
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Any`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Callable, TypeVar

      TReturn = TypeVar("TReturn")

      def decorator(f: Callable[[str], TReturn]) -> Callable[[int], TReturn]: ...

      @overload
      @decorator
      def foo(a: bool) -> int: ...

      @overload
      @decorator
      def foo(a: str) -> bool: ...

      @overload
      @decorator
      def foo(a: int) -> str: ...

      @decorator
      def foo(a: object) -> object: ...

      reveal_type(foo)
    |}
    [
      (* We "select" the relevant overload *)
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable[[int], bool]`.";
    ];
  assert_type_errors
    {|
      from typing import overload, Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      TParams = ParameterSpecification("TParams")
      TReturn = TypeVar("TReturn")

      def decorator(f: Callable[TParams, TReturn]) -> Callable[TParams, TReturn]: ...

      @overload
      @decorator
      def foo(a: bool) -> int: ...

      @overload
      @decorator
      def foo(a: str) -> bool: ...

      @overload
      @decorator
      def foo(a: int) -> str: ...

      @decorator
      def foo(a: object) -> object: ...

      reveal_type(foo)
    |}
    [
      (* But when you "crack the egg" of an overloaded callable, it's not sound to assume it comes
         back together. When multiple overloads match, we select the first one *)
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable[[Named(a, bool)], \
       int]`.";
    ];
  ()


let () =
  "overload"
  >::: [
         "check_implementation" >:: test_check_implementation;
         "decorated_overloads" >:: test_check_decorated_overloads;
       ]
  |> Test.run
