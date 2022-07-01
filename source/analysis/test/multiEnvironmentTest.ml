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

let instantiate_and_stringify ~lookup errors =
  let to_string error =
    let description = AnalysisError.Instantiated.concise_description error in
    let path = AnalysisError.Instantiated.path error in
    let line = AnalysisError.Instantiated.location error |> Location.WithPath.line in
    Format.sprintf "%s %d: %s" path line description
  in
  List.map errors ~f:(AnalysisError.instantiate ~show_error_traces:false ~lookup)
  |> List.map ~f:to_string


let assert_root_errors ~context ~multi_environment expected =
  let actual =
    MultiEnvironment.root_errors multi_environment
    |> instantiate_and_stringify
         ~lookup:
           (MultiEnvironment.root multi_environment
           |> ErrorsEnvironment.ReadOnly.ast_environment
           |> AstEnvironment.ReadOnly.get_real_path_relative)
  in
  assert_equal ~ctxt:context ~printer:[%show: string list] expected actual


let assert_overlay_errors ~context ~multi_environment ~overlay_identifier expected =
  let actual =
    MultiEnvironment.overlay_errors multi_environment overlay_identifier
    |> instantiate_and_stringify
         ~lookup:
           (MultiEnvironment.root multi_environment
           |> ErrorsEnvironment.ReadOnly.ast_environment
           |> AstEnvironment.ReadOnly.get_real_path_relative)
  in
  assert_equal ~ctxt:context ~printer:[%show: string list] expected actual


let test_update_root context =
  let project =
    ScratchProject.setup
      ~context
      ~in_memory:false
      ["a.py", {|
          def foo(x: int) -> int:
              return "x"
        |}]
  in
  let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
  let errors_environment = ScratchProject.ReadWrite.errors_environment project in
  ErrorsEnvironment.populate_all_errors errors_environment ~scheduler:(Test.mock_scheduler ());
  let multi_environment = MultiEnvironment.create errors_environment in
  assert_root_errors
    ~context
    ~multi_environment
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `str`."];
  let update_code relative new_code =
    ScratchProject.delete_file project ~relative;
    ScratchProject.add_file project ~relative new_code;
    ()
  in
  update_code "a.py" {|
    def foo(x: int) -> int:
        return 1.0 * x
  |};
  MultiEnvironment.update_root
    multi_environment
    ~scheduler:(Test.mock_scheduler ())
    [Test.relative_artifact_path ~root:local_root ~relative:"a.py"]
  |> ignore;
  assert_root_errors
    ~context
    ~multi_environment
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `float`."];
  ()


let test_update_overlays context =
  let foo_has_no_errors, foo_has_str_error, foo_has_float_error =
    ( trim_extra_indentation {|
        def foo(x: int) -> int:
            return x
      |},
      trim_extra_indentation {|
        def foo(x: int) -> int:
            return "x"
      |},
      trim_extra_indentation {|
        def foo(x: int) -> int:
            return 1.0 * x
      |}
    )
  in
  let project = ScratchProject.setup ~context ["a.py", foo_has_no_errors] in
  let artifact_path =
    let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
    Test.relative_artifact_path ~root:local_root ~relative:"a.py"
  in
  let multi_environment =
    ScratchProject.ReadWrite.errors_environment project |> MultiEnvironment.create
  in
  (* Create two overlays *)
  MultiEnvironment.update_overlay_with_code
    multi_environment
    ~code_updates:[artifact_path, foo_has_str_error]
    "overlay_0"
  |> ignore;
  MultiEnvironment.update_overlay_with_code
    multi_environment
    ~code_updates:[artifact_path, foo_has_float_error]
    "overlay_1"
  |> ignore;
  (* Validate that each overlay produces the expected errors, and there is no interference *)
  assert_overlay_errors
    ~context
    ~multi_environment
    ~overlay_identifier:"overlay_0"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `str`."];
  assert_overlay_errors
    ~context
    ~multi_environment
    ~overlay_identifier:"overlay_1"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `float`."];
  (* Update the overlays - swap the code *)
  MultiEnvironment.update_overlay_with_code
    multi_environment
    ~code_updates:[artifact_path, foo_has_float_error]
    "overlay_0"
  |> ignore;
  MultiEnvironment.update_overlay_with_code
    multi_environment
    ~code_updates:[artifact_path, foo_has_str_error]
    "overlay_1"
  |> ignore;
  (* Validate that the errors reflect the update *)
  assert_overlay_errors
    ~context
    ~multi_environment
    ~overlay_identifier:"overlay_0"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `float`."];
  assert_overlay_errors
    ~context
    ~multi_environment
    ~overlay_identifier:"overlay_1"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `str`."];
  ()


let () =
  "environment"
  >::: ["update_root" >:: test_update_root; "update_overlays" >:: test_update_overlays]
  |> Test.run
