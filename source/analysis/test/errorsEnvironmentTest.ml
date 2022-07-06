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


let assert_overlay_errors ~context ~project ~overlay qualifier expected =
  let actual =
    ErrorsEnvironment.ReadOnly.get_errors_for_qualifier
      (ErrorsEnvironment.Overlay.read_only overlay)
      qualifier
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


let test_overlay context =
  let project =
    ScratchProject.setup
      ~context
      [
        "code_changes.py", {|
          class Foo:
            x: int = "x"
        |};
        "unsafe_to_strict.py", {|
          # pyre-unsafe
          x = 1 + 2
        |};
      ]
  in
  let parent = ScratchProject.errors_environment project in
  let overlay = ErrorsEnvironment.Overlay.create parent in
  assert_overlay_errors
    ~context
    ~project
    ~overlay
    !&"code_changes"
    ["code_changes.py 3: Incompatible attribute type [8]: Attribute has type `int`; used as `str`."];
  assert_overlay_errors ~context ~project ~overlay !&"unsafe_to_strict" [];
  let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
  ErrorsEnvironment.Overlay.update_overlaid_code
    overlay
    ~code_updates:
      [
        ( Test.relative_artifact_path ~root:local_root ~relative:"code_changes.py",
          ModuleTracker.Overlay.CodeUpdate.NewCode
            (Test.trim_extra_indentation
               {|
               class Foo:
                 x: int = 42.0
               |}) );
        ( Test.relative_artifact_path ~root:local_root ~relative:"unsafe_to_strict.py",
          ModuleTracker.Overlay.CodeUpdate.NewCode
            (Test.trim_extra_indentation
               {|
               # pyre-strict
               x = 1 + 2
               |}) );
      ]
  |> ignore;
  assert_overlay_errors
    ~context
    ~project
    ~overlay
    !&"code_changes"
    [
      "code_changes.py 3: Incompatible attribute type [8]: Attribute has type `int`; used as \
       `float`.";
    ];
  assert_overlay_errors
    ~context
    ~project
    ~overlay
    !&"unsafe_to_strict"
    ["unsafe_to_strict.py 3: Missing global annotation [5]: Global expression must be annotated."];
  ()


let test_error_filtering context =
  (* This test cannot rely on ScratchProject because we want to manually create the configuration in
     order to validate filtering logic that happens in postprocessing *)
  let assert_errors
      ?filter_directories
      ?(ignore_all_errors = [])
      ?(search_paths = [])
      ~root
      ~files
      expected_errors
    =
    let external_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let add_source ~root (relative, content) =
      let content = trim_extra_indentation content in
      let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
      File.write file
    in
    List.iter (typeshed_stubs ()) ~f:(add_source ~root:external_root);
    let ignore_all_errors = external_root :: ignore_all_errors in
    let search_paths = SearchPath.Root external_root :: search_paths in
    let configuration =
      Configuration.Analysis.create
        ?filter_directories
        ~ignore_all_errors
        ~search_paths
        ~project_root:root
        ~local_root:root
        ~source_paths:[SearchPath.Root root]
        ()
    in
    let scheduler = Test.mock_scheduler () in
    List.iter ~f:File.write files;
    let environment =
      let read_write =
        EnvironmentControls.create ~populate_call_graph:true configuration
        |> ErrorsEnvironment.create
      in
      ErrorsEnvironment.check_and_preprocess ~scheduler read_write;
      ErrorsEnvironment.read_only read_write
    in
    let errors =
      Analysis.ErrorsEnvironment.ReadOnly.get_all_errors environment
      |> List.map ~f:(fun error ->
             Analysis.AnalysisError.instantiate
               ~show_error_traces:false
               ~lookup:
                 (Analysis.AstEnvironment.ReadOnly.get_real_path_relative
                    (Analysis.ErrorsEnvironment.ReadOnly.ast_environment environment))
               error
             |> Analysis.AnalysisError.Instantiated.description)
    in
    Memory.reset_shared_memory ();
    assert_equal
      ~printer:(List.to_string ~f:ident)
      ~cmp:(List.equal String.equal)
      expected_errors
      errors
  in
  let content =
    {|
      class C:
        pass
      class D:
        def __init__(self):
          pass
      def foo() -> C:
        return D()
    |}
    |> Test.trim_extra_indentation
  in
  let root = PyrePath.create_absolute ~follow_symbolic_links:true (bracket_tmpdir context) in
  let check_path = PyrePath.create_relative ~root ~relative:"check/a.py" in
  let ignore_path = PyrePath.create_relative ~root ~relative:"ignore/b.py" in
  let files = [File.create ~content check_path; File.create ~content ignore_path] in
  assert_errors
    ~filter_directories:
      [
        PyrePath.create_relative ~root ~relative:"check";
        PyrePath.create_relative ~root ~relative:"ignore";
      ]
    ~root
    ~files
    [
      "Incompatible return type [7]: Expected `C` but got `D`.";
      "Incompatible return type [7]: Expected `C` but got `D`.";
    ];

  let root = PyrePath.create_absolute ~follow_symbolic_links:true (bracket_tmpdir context) in
  let check_path = PyrePath.create_relative ~root ~relative:"check/a.py" in
  let ignore_path = PyrePath.create_relative ~root ~relative:"ignore/b.py" in
  let files = [File.create ~content check_path; File.create ~content ignore_path] in
  assert_errors
    ~root
    ~filter_directories:[PyrePath.create_relative ~root ~relative:"check"]
    ~ignore_all_errors:[PyrePath.create_relative ~root ~relative:"check/search"]
    ~files
    ["Incompatible return type [7]: Expected `C` but got `D`."];

  (* The structure:
   *  /root/check <- pyre is meant to analyze here
   *  /root/check/search <- this is added to the search path, handles are relative to here instead
   *                       of check. The practical case here is resource_cache/typeshed. *)
  let root = PyrePath.create_absolute ~follow_symbolic_links:true (bracket_tmpdir context) in
  assert_errors
    ~root
    ~search_paths:[SearchPath.Root (PyrePath.create_relative ~root ~relative:"check/search")]
    ~filter_directories:[PyrePath.create_relative ~root ~relative:"check"]
    ~ignore_all_errors:[PyrePath.create_relative ~root ~relative:"check/search"]
    ~files:
      [
        File.create ~content (PyrePath.create_relative ~root ~relative:"check/file.py");
        File.create ~content (PyrePath.create_relative ~root ~relative:"check/search/file.py");
      ]
    ["Incompatible return type [7]: Expected `C` but got `D`."];
  let root = PyrePath.create_absolute ~follow_symbolic_links:true (bracket_tmpdir context) in
  assert_errors
    ~root
    ~filter_directories:[PyrePath.create_relative ~root ~relative:"check"]
    ~ignore_all_errors:[PyrePath.create_relative ~root ~relative:"check/ignore"]
    ~files:[File.create ~content (PyrePath.create_relative ~root ~relative:"check/ignore/file.py")]
    []


let () =
  "environment"
  >::: [
         "postprocessing" >:: test_postprocessing;
         "update_ancestor" >:: test_update_ancestor;
         "update_mode" >:: test_update_mode;
         "overlay" >:: test_overlay;
         "error_filtering" >:: test_error_filtering;
       ]
  |> Test.run
