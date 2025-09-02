(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Interprocedural

let test_callables context =
  let assert_callables ?(additional_sources = []) ?(source_filename = "test.py") source ~expected =
    let configuration, pyre_api =
      let project =
        Test.ScratchPyrePysaProject.setup
          ~context
          ~requires_type_of_expressions:false
          ((source_filename, source) :: additional_sources)
      in
      ( Test.ScratchPyrePysaProject.configuration_of project,
        Test.ScratchPyrePysaProject.read_only_api project )
    in
    FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier:(Ast.Reference.create "test")
    |> FetchCallables.get ~definitions:true ~stubs:true
    |> List.sort ~compare:Target.compare
    |> assert_equal
         ~printer:(List.to_string ~f:Target.show_pretty)
         ~cmp:(List.equal Target.equal)
         (List.map ~f:Target.from_regular expected)
  in
  assert_callables
    {|
    class C:
      def foo() -> int:
        ...
    |}
    ~expected:
      [
        Target.Regular.Function { name = "test.$toplevel"; kind = Normal };
        Target.Regular.Method
          { class_name = "test.C"; method_name = "$class_toplevel"; kind = Normal };
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
      ];
  assert_callables
    {|
      import unittest
      class C(unittest.case.TestCase):
        def foo() -> int:
          ...
    |}
    ~expected:[];
  assert_callables
    {|
      import pytest
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:
      [
        Target.Regular.Function { name = "test.$toplevel"; kind = Normal };
        Target.Regular.Method
          { class_name = "test.C"; method_name = "$class_toplevel"; kind = Normal };
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
      ];
  assert_callables
    {|
      import pytest
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:[];
  assert_callables
    {|
      from pytest import raises
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:[];
  assert_callables
    {|
      import pytest.raises as throws
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:[];
  assert_callables
    {|
      import pytest
      class C:
        def foo() -> int:
          ...
        def test_int() -> int:
          return 0
    |}
    ~expected:[];
  assert_callables
    {|
      import foo.pytest
      def test_int() -> int:
        return 0
      class C:
        def foo() -> int:
          ...
    |}
    ~expected:
      [
        Target.Regular.Function { name = "test.$toplevel"; kind = Normal };
        Target.Regular.Function { name = "test.test_int"; kind = Normal };
        Target.Regular.Method
          { class_name = "test.C"; method_name = "$class_toplevel"; kind = Normal };
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
      ];
  assert_callables
    "pass"
    ~additional_sources:
      [
        ( "stub.pyi",
          {|
            class Toplevel:
              some_field: int
              def foo() -> int:
                ...
          |}
        );
      ]
    ~expected:[Target.Regular.Function { name = "test.$toplevel"; kind = Normal }];

  assert_callables
    ~source_filename:"test.pyi"
    {|
      class Toplevel:
        some_field: int
        def foo() -> int:
          ...
    |}
    ~expected:
      [Target.Regular.Method { class_name = "test.Toplevel"; method_name = "foo"; kind = Normal }]


let () = "staticAnalysis" >::: ["callables" >:: test_callables] |> Test.run
