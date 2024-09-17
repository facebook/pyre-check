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


let test_index =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class MyTuple(NamedTuple):
    field1: int
    field2: str
p = MyTuple(field1=1, field2="abc")
reveal_type(p[0])
reveal_type(p[1])
reveal_type(p[-1])
reveal_type(p[-2])
            |}
           [
             "Revealed type [-1]: Revealed type for `p[0]` is `int`.";
             "Revealed type [-1]: Revealed type for `p[1]` is `str`.";
             "Revealed type [-1]: Revealed type for `p[-1]` is `str`.";
             "Revealed type [-1]: Revealed type for `p[-2]` is `int`.";
           ];
    ]


let () = "named_tuple" >::: [test_unpack; test_index] |> Test.run
