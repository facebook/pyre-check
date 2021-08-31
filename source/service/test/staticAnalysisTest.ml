(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core

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
    Service.StaticAnalysis.regular_and_filtered_callables ~configuration ~resolution ~source
    |> fst
    |> List.map ~f:(fun { Service.StaticAnalysis.callable; _ } -> callable)
    |> assert_equal
         ~printer:(List.to_string ~f:Interprocedural.Target.show_callable_t)
         ~cmp:(List.equal Interprocedural.Target.equal_callable_t)
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
        `Function "test.$toplevel";
        `Method { Interprocedural.Target.class_name = "test.C"; method_name = "$class_toplevel" };
        `Method { Interprocedural.Target.class_name = "test.C"; method_name = "foo" };
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
        `Function "test.$toplevel";
        `Method { Interprocedural.Target.class_name = "test.C"; method_name = "$class_toplevel" };
        `Method { Interprocedural.Target.class_name = "test.C"; method_name = "foo" };
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
    ~expected:[`Function "test.$toplevel"];

  assert_callables
    ~source_filename:"test.pyi"
    {|
      class Toplevel:
        some_field: int
        def foo() -> int:
          ...
    |}
    ~expected:[`Method { Interprocedural.Target.class_name = "test.Toplevel"; method_name = "foo" }]


let () = "staticAnalysis" >::: ["callables" >:: test_callables] |> Test.run
