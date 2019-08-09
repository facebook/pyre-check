(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_assert_is_none context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          self.assertIsNotNone(2)
    |}
    [];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertIsNotNone(x)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self, iter: typing.List[A]) -> None:
          a = None
          for i in iter:
            a = i
          self.assertIsNotNone(a)
          attribute = a.x
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = 1

      def foo(f: Foo) -> None:
        assert f.x is None
        reveal_type(f.x)
    |}
    ["Revealed type [-1]: Revealed type for `f.x` is `int`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertTrue(x is not None)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertFalse(x is None)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."]


let test_check_global_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      def foo() -> None:
        a = A(3)
        if a.x:
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `typing.Optional[int]`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          self.assertIsNotNone(a.x)
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `typing.Optional[int]`."];
  assert_type_errors
    {|
      MY_GLOBAL: typing.Optional[int] = 1

      def foo() -> None:
        if MY_GLOBAL:
          reveal_type(MY_GLOBAL)
    |}
    (* TODO(T47870649): Refinement should not work. *)
    [ "Revealed type [-1]: Revealed type for `MY_GLOBAL` is `typing.Optional[int]` (inferred: \
       `int`)." ];

  assert_type_errors
    {|
      x: typing.Optional[int] = 1

      def foo() -> None:
        global x
        x = 1
        if x is not None:
          reveal_type(x)
    |}
    (* TODO(T47870649): Refinement should not work. *)
    [ "Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: \
       `typing_extensions.Literal[1]`)." ]


let test_check_local_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
        if x:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];

  assert_type_errors
    {|
      class FakeTest(unittest.TestCase):
        def foo(self, x: typing.Optional[int]) -> None:
          self.assertIsNotNone(x)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];

  assert_type_errors
    {|
      x: typing.Optional[int] = 1

      def foo(test: bool) -> None:
        if test:
          x = 1
        else:
          x = None
        if x:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[1]`."];

  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Optional[int]
        if x is not None:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."]


let test_check_isinstance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
        if isinstance(x, int):
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];

  assert_type_errors
    {|
      MY_GLOBAL: typing.Union[int, str] = 1

      def foo() -> None:
        if isinstance(MY_GLOBAL, str):
          reveal_type(MY_GLOBAL)
    |}
    (* TODO(T47870649): Refinement should not work. *)
    ["Revealed type [-1]: Revealed type for `MY_GLOBAL` is `str`."];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x: typing.Union[int, str] = 1

      def foo(f: Foo) -> None:
        if isinstance(f.x, str):
          reveal_type(f.x)
    |}
    ["Revealed type [-1]: Revealed type for `f.x` is `typing.Union[int, str]`."]


let () =
  "assert_is_not_none"
  >::: [ "check_assert_is_none" >:: test_assert_is_none;
         "check_global_refinement" >:: test_check_global_refinement;
         "check_local_refinement" >:: test_check_local_refinement;
         "check_isinstance" >:: test_check_isinstance ]
  |> Test.run
