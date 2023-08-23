(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_inconsistent_mro context =
  assert_type_errors
    ~context
    {|
        class A(B): pass
        class B(A): pass
    |}
    [
      "Inconsistent method resolution order [64]: Class `A` does not have a consistent method \
       resolution order";
      "Inconsistent method resolution order [64]: Class `B` does not have a consistent method \
       resolution order";
    ];
  (* Inconsistent MRO would shadow other checks like override validity *)
  assert_type_errors
    ~context
    {|
        class A(B):
          def foo(self) -> int: ...
        class B(A):
          def foo(self) -> str: ...
    |}
    [
      "Inconsistent method resolution order [64]: Class `A` does not have a consistent method \
       resolution order";
      "Inconsistent method resolution order [64]: Class `B` does not have a consistent method \
       resolution order";
    ];
  ()


let () = "class" >::: ["inconsistent_mro" >:: test_inconsistent_mro] |> Test.run
