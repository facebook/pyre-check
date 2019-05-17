(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_assert_is_none _ =
  assert_type_errors
    {|
      from typing import Optional
      import unittest
      class A:
        def __init__(self, x:Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          self.assertIsNotNone(a.x)
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `int`."];

  assert_type_errors
    {|
      from typing import Optional
      import unittest
      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          self.assertIsNotNone(2)
    |}
    [];

  assert_type_errors
    {|
      from typing import Optional
      import unittest
      class A:
        def __init__(self, x:Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          self.assertTrue(a.x is not None)
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `int`."];

  assert_type_errors
    {|
      from typing import Optional
      import unittest
      class A:
        def __init__(self, x:Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          self.assertFalse(a.x is None)
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `int`."]


let () =
  "assert_is_not_none">:::[
    "check_assert_is_none">::test_assert_is_none;
  ]
  |> Test.run
