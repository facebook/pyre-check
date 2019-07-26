(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_imports context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors {|
      import typing
    |} [];
  assert_type_errors
    {|
      import typing, durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors {|
      from typing import durp
    |} [];
  assert_type_errors
    {|
      from durp import typing
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];

  (* Ensure we don't double-error. *)
  assert_type_errors
    {|
      a = durp.x
    |}
    [ "Missing global annotation [5]: Globally accessible variable `a` has no type specified.";
      "Undefined name [18]: Global name `durp` is not defined, or there is at least one control \
       flow path that doesn't define `durp`." ];
  assert_type_errors
    {|
      import durp
      a = durp.x
    |}
    [ "Undefined import [21]: Could not find a module corresponding to import `durp`.";
      "Missing global annotation [5]: Globally accessible variable `a` has no type specified." ];
  assert_type_errors
    {|
      from typing import Optional
      def foo() -> None: return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."]


let () = "import" >::: ["check_imports" >:: test_check_imports] |> Test.run
