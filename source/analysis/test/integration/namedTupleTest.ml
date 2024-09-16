(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest
open Test

let test_unpack =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple, Any
class Point(NamedTuple):
    x: int
    y: int
    units: str = "meters"
p = Point(x=1, y=2, units="inches")
v1: tuple[int, int, str] = p
v5: tuple[float, float, str] = p
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class Point(NamedTuple):
    x: int
    y: int
p = Point(x=1, y=2)
a: int
b: int
a, b = p
            |}
           [];
    ]


let () = "named_tuple" >::: [test_unpack] |> Test.run
