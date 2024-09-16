(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_readonly =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from typing import TypedDict
           from typing_extensions import ReadOnly

           class Band(TypedDict):
             name: str
             members: ReadOnly[list[str]]

           blur: Band = {"name": "blur", "members": []}
           blur["members"] = ["Damon Albarn"]  # Type check error: "members" is read-only
            |}
           ["Invalid type parameters [24]: Non-generic type `ReadOnly` cannot take parameters."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from typing import TypedDict
           from typing_extensions import ReadOnly

           class Band(TypedDict):
             name: str
             members: ReadOnly[list[str]]

           blur: Band = {"name": "blur", "members": []}
           blur["members"].append("Damon Albarn")  # OK: list is mutable
            |}
           ["Invalid type parameters [24]: Non-generic type `ReadOnly` cannot take parameters."];
    ]


let () = "readOnly" >::: [test_readonly] |> Test.run
