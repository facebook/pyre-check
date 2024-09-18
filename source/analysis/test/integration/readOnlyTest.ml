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
           [];
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
           [];
    ]


let test_interaction =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
          from typing import Annotated, TypedDict
          from typing_extensions import NotRequired, ReadOnly, Required

          class TD(TypedDict):
            x: Annotated[ReadOnly[int], 42]
            y: Required[ReadOnly[str]]
            z: NotRequired[ReadOnly[bytes]]

          # Test that NotRequired on 'z' is respected
          td1: TD = {'x': 0, 'y': ""}  # OK

          # Test that Required on 'y' is respected
          td2: TD = {'x': 0, 'z': b""}  # Error
          |}
           ["TypedDict initialization error [55]: Missing required field `y` for TypedDict `TD`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
          from typing import Annotated, TypedDict
          from typing_extensions import NotRequired, ReadOnly, Required

          class TD(TypedDict):
            x: ReadOnly[Annotated[int, 42]]
            y: ReadOnly[Required[str]]
            z: ReadOnly[NotRequired[bytes]]

          # Test that NotRequired on 'z' is respected
          td1: TD = {'x': 0, 'y': ""}  # OK

          # Test that Required on 'y' is respected
          td2: TD = {'x': 0, 'z': b""}  # Error
            |}
           ["TypedDict initialization error [55]: Missing required field `y` for TypedDict `TD`."];
    ]


let () = "readOnly" >::: [test_readonly; test_interaction] |> Test.run
