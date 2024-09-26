(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Test

let test_source_is_unit_test context =
  let assert_is_unit_test ?(expected = true) ?(extra_sources = []) source =
    let project = ScratchProject.setup ~context (["test.py", source] @ extra_sources) in
    let { Test.ScratchProject.BuiltTypeEnvironment.type_environment; _ }, _ =
      Test.ScratchProject.build_type_environment_and_postprocess project
    in
    let global_module_paths_api = Test.ScratchProject.global_module_paths_api project in
    let source =
      SourceCodeApi.source_of_qualifier
        (Test.ScratchProject.get_untracked_source_code_api project)
        (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    let pyre_pysa_read_only_api =
      PyrePysaEnvironment.ReadOnly.create ~type_environment ~global_module_paths_api
    in
    assert_equal
      expected
      (PyrePysaEnvironment.ReadOnly.source_is_unit_test pyre_pysa_read_only_api ~source)
  in
  let assert_not_unit_test = assert_is_unit_test ~expected:false in
  assert_is_unit_test "class C(unittest.case.TestCase): ...";
  assert_not_unit_test {|
    from unittest import TestCase
    class C: pass
  |};
  assert_is_unit_test
    {|
    class C:
      def foo():
        class Nested(unittest.case.TestCase): ...
  |};
  ()


let () = "pyrePysaApi" >::: ["source_is_unit_test" >:: test_source_is_unit_test] |> Test.run
