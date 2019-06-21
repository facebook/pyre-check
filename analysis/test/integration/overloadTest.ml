(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_implementation _ =
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
    [ "Missing overload implementation [42]: Overloaded function `foo` must have an implementation.";
      "Missing overload implementation [42]: Overloaded function `foo` must have an implementation."
    ];
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
      def foo(x: str) -> int:
          pass

      def foo(x: str) -> float:
          return 1
    |}
    [ "Incompatible overload [43]: The return type of overloaded function `foo` (`bool`) is \
       incompatible with the return type of the implementation (`float`)." ];
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

         def foo(bar: str) -> object: pass
       |}
    [ "Incompatible overload [43]: The overloaded function `foo` on line 9 will never be matched. \
       The signature of overload on line 5 is the same or broader.";
      "Incompatible overload [43]: The overloaded function `foo` on line 13 will never be \
       matched. The signature of overload on line 5 is the same or broader." ];
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

         def foo(bar: str) -> object: pass
       |}
    [];
  assert_type_errors
    {|
         from typing import overload

         @overload
         def foo(bar: int, x: int) -> str:
             pass

         @overload
         def foo(bar: str, x: str, x: int) -> object:
             pass

         def foo(bar: str) -> object: pass
       |}
    []


let () = "method" >::: ["check_implementation" >:: test_check_implementation] |> Test.run
