(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_delete =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                  x = 10
                  del x
                  return x # Error
            |}
           ["Incompatible return type [7]: Expected `None` but got `unknown`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int) -> int:
                if x > 100:
                  del x
                else:
                  x =+ 1
                return x
            |}
           [];
    ]


let () = "delete" >::: [test_delete] |> Test.run
