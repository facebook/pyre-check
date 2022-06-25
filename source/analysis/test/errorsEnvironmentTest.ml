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


let assert_errors ~context ~project expected =
  let actual =
    ScratchProject.errors_environment project
    |> ErrorsEnvironment.ReadOnly.get_all_errors
    |> instantiate_and_stringify
         ~lookup:
           (ScratchProject.ast_environment project |> AstEnvironment.ReadOnly.get_real_path_relative)
  in
  assert_equal ~ctxt:context ~printer:[%show: string list] expected actual


let test_postprocessing context =
  let code header_comment =
    Format.asprintf
      {|
        %s

        def foo(x):
            return x

        def bar(x: int) -> str:
            return x

        # pyre-ignore[7]
        def ignored_bad_return(x: int) -> str:
            return x

        # pyre-ignore[7]
        def has_unused_ignore(x: int) -> int:
            return x
      |}
      header_comment
  in
  let project =
    ScratchProject.setup
      ~context
      [
        "has_parse_error.py", {|
          def foo(
        |};
        "unsafe.py", code "# pyre-unsafe";
        "strict.py", code "# pyre-strict";
      ]
  in
  ScratchProject.ReadWrite.errors_environment project
  |> ErrorsEnvironment.populate_all_errors ~scheduler:(Test.mock_scheduler ());
  (* Note that the output is always sorted, we can depend on this in tests *)
  assert_errors
    ~context
    ~project
    [
      "has_parse_error.py 2: Parsing failure [404]: '(' was never closed";
      "strict.py 4: Missing return annotation [3]: Return type must be annotated.";
      "strict.py 4: Missing parameter annotation [2]: Parameter must be annotated.";
      "strict.py 8: Incompatible return type [7]: Expected `str` but got `int`.";
      "strict.py 10: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
      "strict.py 12: Incompatible return type [7]: Expected `str` but got `int`.";
      "strict.py 14: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
      (* Note that the strict-mode-only errors on line 4 are not present for unsafe.py *)
      "unsafe.py 8: Incompatible return type [7]: Expected `str` but got `int`.";
      "unsafe.py 10: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
      "unsafe.py 12: Incompatible return type [7]: Expected `str` but got `int`.";
      "unsafe.py 14: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
    ];
  ()


let test_update_ancestor context =
  let project =
    ScratchProject.setup
      ~context
      ~in_memory:false
      [
        "a.py", {|
          class A:
            x: float = 5
        |};
        ( "b.py",
          {|
          from a import A

          class B(A):
            y: str = "y"
        |} );
        ( "c.py",
          {|
          from b import B

          def deconstruct(b: B) -> tuple[int, str]:
            return (b.x, b.y)
        |}
        );
      ]
  in
  ScratchProject.ReadWrite.errors_environment project
  |> ErrorsEnvironment.populate_all_errors ~scheduler:(Test.mock_scheduler ());
  (* validate initial state *)
  assert_errors
    ~context
    ~project
    [
      "c.py 5: Incompatible return type [7]: Expected `Tuple[int, str]` but got `Tuple[float, str]`.";
    ];
  (* update and validate new errors *)
  let update_code relative new_code =
    ScratchProject.delete_file project ~relative;
    ScratchProject.add_file project ~relative new_code;
    ()
  in
  update_code "a.py" {|
    class A:
      x: int = 5
  |};
  let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
  ScratchProject.update_environment
    project
    [Test.relative_artifact_path ~root:local_root ~relative:"a.py"]
  |> ignore;
  assert_errors ~context ~project [];
  ()


let test_update_mode context =
  let project =
    ScratchProject.setup
      ~context
      ~in_memory:false
      [
        ( "changes_from_unsafe_to_strict.py",
          {|
          # pyre-unsafe

          def foo(x):
            return x
        |} );
      ]
  in
  ScratchProject.ReadWrite.errors_environment project
  |> ErrorsEnvironment.populate_all_errors ~scheduler:(Test.mock_scheduler ());
  (* validate initial state *)
  assert_errors ~context ~project [];
  (* update and validate new errors *)
  let update_code relative new_code =
    ScratchProject.delete_file project ~relative;
    ScratchProject.add_file project ~relative new_code;
    ()
  in
  update_code
    "changes_from_unsafe_to_strict.py"
    {|
    # pyre-strict

    def foo(x):
      return x
  |};
  let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
  ScratchProject.update_environment
    project
    [
      Test.relative_artifact_path ~root:local_root ~relative:"a.py";
      Test.relative_artifact_path ~root:local_root ~relative:"changes_from_unsafe_to_strict.py";
    ]
  |> ignore;
  assert_errors
    ~context
    ~project
    [
      "changes_from_unsafe_to_strict.py 4: Missing return annotation [3]: Return type must be \
       annotated.";
      "changes_from_unsafe_to_strict.py 4: Missing parameter annotation [2]: Parameter must be \
       annotated.";
    ];
  ()


let () =
  "environment"
  >::: [
         "postprocessing" >:: test_postprocessing;
         "update_ancestor" >:: test_update_ancestor;
         "update_mode" >:: test_update_mode;
       ]
  |> Test.run
