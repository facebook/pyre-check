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


let assert_root_errors ~context ~overlaid_environment expected =
  let actual =
    OverlaidEnvironment.root_errors overlaid_environment
    |> instantiate_and_stringify
         ~lookup:
           (OverlaidEnvironment.root overlaid_environment
           |> ErrorsEnvironment.ReadOnly.ast_environment
           |> AstEnvironment.ReadOnly.get_real_path_relative)
  in
  assert_equal ~ctxt:context ~printer:[%show: string list] expected actual


let assert_overlay_errors ~context ~overlaid_environment ~overlay_identifier expected =
  let actual =
    OverlaidEnvironment.overlay_errors overlaid_environment overlay_identifier
    |> instantiate_and_stringify
         ~lookup:
           (OverlaidEnvironment.root overlaid_environment
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
  let overlaid_environment = OverlaidEnvironment.create errors_environment in
  assert_root_errors
    ~context
    ~overlaid_environment
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
  OverlaidEnvironment.update_root
    overlaid_environment
    ~scheduler:(Test.mock_scheduler ())
    [Test.relative_artifact_path ~root:local_root ~relative:"a.py"]
  |> ignore;
  assert_root_errors
    ~context
    ~overlaid_environment
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
  let overlaid_environment =
    ScratchProject.ReadWrite.errors_environment project |> OverlaidEnvironment.create
  in
  (* Create two overlays *)
  OverlaidEnvironment.update_overlay_with_code
    overlaid_environment
    ~code_updates:[artifact_path, ModuleTracker.Overlay.CodeUpdate.NewCode foo_has_str_error]
    "overlay_0"
  |> ignore;
  OverlaidEnvironment.update_overlay_with_code
    overlaid_environment
    ~code_updates:[artifact_path, ModuleTracker.Overlay.CodeUpdate.NewCode foo_has_float_error]
    "overlay_1"
  |> ignore;
  (* Validate that each overlay produces the expected errors, and there is no interference *)
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_0"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `str`."];
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_1"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `float`."];
  (* Update the overlays - swap the code *)
  OverlaidEnvironment.update_overlay_with_code
    overlaid_environment
    ~code_updates:[artifact_path, ModuleTracker.Overlay.CodeUpdate.NewCode foo_has_float_error]
    "overlay_0"
  |> ignore;
  OverlaidEnvironment.update_overlay_with_code
    overlaid_environment
    ~code_updates:[artifact_path, ModuleTracker.Overlay.CodeUpdate.NewCode foo_has_str_error]
    "overlay_1"
  |> ignore;
  (* Validate that the errors reflect the update *)
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_0"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `float`."];
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_1"
    ["a.py 3: Incompatible return type [7]: Expected `int` but got `str`."];
  ()


let test_overlay_propagation context =
  let on_filesystem_code ~x_type ~y_type ~z_type =
    Format.asprintf {|
    x: %s = ...
    y: %s = ...
    z: %s = ...
  |} x_type y_type z_type
    |> trim_extra_indentation
  in
  let in_overlay_code ~varname =
    Format.asprintf {|
    import on_filesystem
    reveal_type(on_filesystem.%s)
  |} varname
    |> trim_extra_indentation
  in
  let project =
    ScratchProject.setup
      ~context
      ~in_memory:false
      [
        "on_filesystem.py", on_filesystem_code ~x_type:"int" ~y_type:"float" ~z_type:"str";
        "in_overlay.py", in_overlay_code ~varname:"x";
      ]
  in
  let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
  let artifact_path = Test.relative_artifact_path ~root:local_root ~relative:"in_overlay.py" in
  let overlaid_environment =
    ScratchProject.ReadWrite.errors_environment project |> OverlaidEnvironment.create
  in
  (* Create two overlays, and check that we detect the right varnames *)
  OverlaidEnvironment.update_overlay_with_code
    overlaid_environment
    ~code_updates:
      [artifact_path, ModuleTracker.Overlay.CodeUpdate.NewCode (in_overlay_code ~varname:"y")]
    "overlay_0"
  |> ignore;
  OverlaidEnvironment.update_overlay_with_code
    overlaid_environment
    ~code_updates:
      [artifact_path, ModuleTracker.Overlay.CodeUpdate.NewCode (in_overlay_code ~varname:"z")]
    "overlay_1"
  |> ignore;
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_0"
    ["in_overlay.py 3: Revealed type [-1]: Revealed type for `on_filesystem.y` is `float`."];
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_1"
    ["in_overlay.py 3: Revealed type [-1]: Revealed type for `on_filesystem.z` is `str`."];
  (* Update the parent environment and make sure the changes propagate *)
  let update_code relative new_code =
    ScratchProject.delete_file project ~relative;
    ScratchProject.add_file project ~relative new_code;
    ()
  in
  update_code "on_filesystem.py" (on_filesystem_code ~x_type:"float" ~y_type:"str" ~z_type:"int");
  OverlaidEnvironment.run_update_root
    overlaid_environment
    ~scheduler:(Test.mock_scheduler ())
    [Test.relative_artifact_path ~root:local_root ~relative:"on_filesystem.py"];
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_0"
    ["in_overlay.py 3: Revealed type [-1]: Revealed type for `on_filesystem.y` is `str`."];
  assert_overlay_errors
    ~context
    ~overlaid_environment
    ~overlay_identifier:"overlay_1"
    ["in_overlay.py 3: Revealed type [-1]: Revealed type for `on_filesystem.z` is `int`."];
  ()


let () =
  "environment"
  >::: [
         "update_root" >:: test_update_root;
         "update_overlays" >:: test_update_overlays;
         "overlay_propagation" >:: test_overlay_propagation;
       ]
  |> Test.run
