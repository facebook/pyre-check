(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


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
    [
      "Missing overload implementation [42]: Overloaded function `foo` \
       must have an implementation."
    ];

  assert_type_errors
    {|
      from typing import overload
      @overload
      def foo() -> None:
          pass

      def foo() -> None:
          pass
    |}
    []


let () =
  "method">:::[
    "check_implementation">::test_check_implementation;
  ]
  |> Test.run
