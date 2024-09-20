(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest
open Test

let test_inheritance =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class MyTuple(NamedTuple, object):
  pass
            |}
           [
             "Invalid inheritance [39]: If NamedTuple is included as a base class, the class may \
              not extend anything else besides Generic.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class MyTuple(NamedTuple):
  pass
class MyTuple2(MyTuple, object):
  pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple, Generic, TypeVar
T = TypeVar('T')
class MyTuple(NamedTuple, Generic[T]):
  pass
            |}
           [];
    ]


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
      (* TODO(yangdanny): named tuple classes do not function correctly if they do not directly
         extend NamedTuple *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class Point(NamedTuple):
    x: int
    y: int
class Point3D(Point):
    z: int
p = Point3D(x=1, y=2, z=3)
a: int
b: int
c: int
a, b, c = p
            |}
           [
             "Uninitialized attribute [13]: Attribute `z` is declared in class `Point3D` to have \
              type `int` but is never initialized.";
             "Unexpected keyword [28]: Unexpected keyword argument `z` to call `Point.__init__`.";
             "Unable to unpack [23]: Unable to unpack 0 values, 3 were expected.";
           ];
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class MyTuple(NamedTuple):
    field1: int
    field2: str
p = MyTuple(field1=1, field2="abc")
p[2]
p[-3]
            |}
           [
             "Invalid tuple index [73]: Index 2 is out of bounds for concrete tuple with 2 members.";
             "Invalid tuple index [73]: Index -3 is out of bounds for concrete tuple with 2 \
              members.";
           ];
    ]


let test_delete =
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
del p.field1
            |}
           [
             "Unable to delete tuple member [72]: Tuples are immutable, so their members may not \
              be deleted.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
from typing import NamedTuple
class MyTuple(NamedTuple):
    field1: int
    field2: str
p = MyTuple(field1=1, field2="abc")
del p[1]
            |}
           [
             "Unable to delete tuple member [72]: Tuples are immutable, so their members may not \
              be deleted.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
from typing import Any
def foo() -> None:
  x: Any = ...
  del x[0]
            |}
           [];
    ]


let () = "named_tuple" >::: [test_inheritance; test_unpack; test_index; test_delete] |> Test.run
