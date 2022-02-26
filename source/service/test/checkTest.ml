(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let assert_errors
    ?filter_directories
    ?(ignore_all_errors = [])
    ?(search_paths = [])
    ~root
    ~files
    ~context
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
  let { Service.Check.environment; errors; _ } =
    Service.Check.check
      ~scheduler
      ~configuration
      ~call_graph_builder:(module Analysis.Callgraph.DefaultBuilder)
  in
  let errors =
    errors
    |> List.map ~f:(fun error ->
           Analysis.AnalysisError.instantiate
             ~show_error_traces:false
             ~lookup:
               (Analysis.AstEnvironment.ReadOnly.get_real_path_relative
                  ~configuration
                  (Analysis.TypeEnvironment.ast_environment environment
                  |> Analysis.AstEnvironment.read_only))
             error
           |> Analysis.AnalysisError.Instantiated.description)
  in
  Memory.reset_shared_memory ();
  assert_equal
    ~printer:(List.to_string ~f:ident)
    ~cmp:(List.equal String.equal)
    expected_errors
    errors


let type_check_sources_list_test context =
  let create_files ~root content =
    let default_content =
      {|
        class object():
          def __sizeof__(self) -> int: pass
        class Sized: ...
        class float():
          pass
        class int(float):
          pass
      |}
      |> trim_extra_indentation
    in
    [
      File.create
        ~content:(default_content ^ "\n" ^ (content |> trim_extra_indentation))
        (PyrePath.create_relative ~root ~relative:"test.py");
    ]
  in
  let check _ =
    let root = PyrePath.current_working_directory () in
    let files = {|
        def foo() -> str:
          return 1
      |} |> create_files ~root in
    assert_errors
      ~filter_directories:[root]
      ~root
      ~files
      ~context
      ["Incompatible return type [7]: Expected `str` but got `int`."]
  in
  with_bracket_chdir context (bracket_tmpdir context) check


let test_filter_directories context =
  let assert_errors = assert_errors ~context in
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
  "typeChecker"
  >::: [
         "type_check_sources_list" >:: type_check_sources_list_test;
         "filter_directories" >:: test_filter_directories;
       ]
  |> Test.run
