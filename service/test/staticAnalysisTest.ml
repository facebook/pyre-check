(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core

let test_callables context =
  let assert_callables ?(additional_sources = []) source ~expected =
    let resolution =
      Test.ScratchProject.setup ~context (("test.py", source) :: additional_sources)
      |> Test.ScratchProject.build_global_resolution
    in
    let source =
      Option.value_exn
        ( Analysis.GlobalResolution.ast_environment resolution
        |> fun environment ->
        Analysis.AstEnvironment.ReadOnly.get_source environment (Ast.Reference.create "test") )
    in
    Service.StaticAnalysis.callables ~resolution ~source
    |> List.map ~f:fst
    |> assert_equal
         ~printer:(List.to_string ~f:Interprocedural.Callable.show_real_target)
         ~cmp:(List.equal Interprocedural.Callable.equal_real_target)
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
        `Method { Interprocedural.Callable.class_name = "test.C"; method_name = "$class_toplevel" };
        `Method { Interprocedural.Callable.class_name = "test.C"; method_name = "foo" };
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
        `Method { Interprocedural.Callable.class_name = "test.C"; method_name = "$class_toplevel" };
        `Method { Interprocedural.Callable.class_name = "test.C"; method_name = "foo" };
      ]


let () = "staticAnalysis" >::: ["callables" >:: test_callables] |> Test.run
