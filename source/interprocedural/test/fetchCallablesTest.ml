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
    let configuration, resolution =
      let scratch_project =
        Test.ScratchProject.setup ~context ((source_filename, source) :: additional_sources)
      in
      ( Test.ScratchProject.configuration_of scratch_project,
        Test.ScratchProject.build_global_resolution scratch_project )
    in
    let source =
      Option.value_exn
        (Analysis.GlobalResolution.ast_environment resolution
        |> fun environment ->
        Analysis.AstEnvironment.ReadOnly.get_processed_source
          environment
          (Ast.Reference.create "test"))
    in
    FetchCallables.from_source ~configuration ~resolution ~include_unit_tests:false ~source
    |> FetchCallables.get_all
    |> List.sort ~compare:Target.compare
    |> assert_equal
         ~printer:(List.to_string ~f:Target.show_pretty)
         ~cmp:(List.equal Target.equal)
         expected
  in
  assert_callables
    {|
    class C:
      def foo() -> int:
        ...
    |}
    ~expected:
      [
        Target.Function { name = "test.$toplevel"; kind = Normal };
        Target.Method { class_name = "test.C"; method_name = "$class_toplevel"; kind = Normal };
        Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
      ];
  assert_callables
    ~additional_sources:["placeholder.py", "# pyre-placeholder-stub"]
    {|
      import placeholder
      class C(placeholder.Base):
        def foo() -> int:
          ...
    |}
    ~expected:
      [
        Target.Function { name = "test.$toplevel"; kind = Normal };
        Target.Method { class_name = "test.C"; method_name = "$class_toplevel"; kind = Normal };
        Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
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
    ~expected:[Target.Function { name = "test.$toplevel"; kind = Normal }];

  assert_callables
    ~source_filename:"test.pyi"
    {|
      class Toplevel:
        some_field: int
        def foo() -> int:
          ...
    |}
    ~expected:[Target.Method { class_name = "test.Toplevel"; method_name = "foo"; kind = Normal }]


let () = "staticAnalysis" >::: ["callables" >:: test_callables] |> Test.run
