(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre
open Test

let assert_errors ?filter_directories ?ignore_all_errors ?search_path ~root ~files errors =
  let configuration =
    Configuration.Analysis.create
      ?filter_directories
      ?ignore_all_errors
      ?search_path
      ~project_root:root
      ~local_root:root
      ()
  in
  let scheduler = Scheduler.mock () in
  List.iter ~f:File.write files;
  let module_tracker = Analysis.ModuleTracker.create configuration in
  let sources, ast_environment =
    Service.Parser.parse_all ~configuration ~scheduler module_tracker
  in
  let qualifiers = List.map sources ~f:(fun { Ast.Source.qualifier; _ } -> qualifier) in
  let all_sources = List.append (typeshed_stubs ~include_helper_builtins:false ()) sources in
  let all_qualifiers = List.map all_sources ~f:(fun { Ast.Source.qualifier; _ } -> qualifier) in
  let environment =
    let ast_environment = Analysis.AstEnvironment.read_only ast_environment in
    Service.Environment.populate_shared_memory
      ~configuration
      ~scheduler
      ~ast_environment
      all_sources
  in
  let actual_errors =
    let ast_environment = Analysis.Environment.ast_environment environment in
    Service.Check.analyze_sources ~scheduler ~configuration ~environment sources
    |> List.map ~f:(fun error ->
           Analysis.Error.instantiate
             ~lookup:(Analysis.AstEnvironment.ReadOnly.get_relative ast_environment)
             error
           |> Analysis.Error.Instantiated.description ~show_error_traces:false)
  in
  Analysis.Environment.purge environment all_qualifiers;
  Analysis.AstEnvironment.remove_sources ast_environment qualifiers;
  assert_equal
    ~printer:(List.to_string ~f:ident)
    ~cmp:(List.equal String.equal)
    errors
    actual_errors


let type_check_sources_list_test context =
  let create_files ~root content =
    let default_content =
      {|
        class object():
          def __sizeof__(self) -> int: pass
        class typing.Sized: ...
        class float():
          pass
        class int(float):
          pass
        class str(typing.Sized):
          pass
      |}
      |> trim_extra_indentation
    in
    [ File.create
        ~content:(default_content ^ "\n" ^ (content |> trim_extra_indentation))
        (Path.create_relative ~root ~relative:"test.py") ]
  in
  let check _ =
    let root = Path.current_working_directory () in
    let files = {|
        def foo() -> str:
          return 1
      |} |> create_files ~root in
    assert_errors
      ~filter_directories:[root]
      ~root
      ~files
      ["Incompatible return type [7]: Expected `str` but got `int`."]
  in
  with_bracket_chdir context (bracket_tmpdir context) check


let test_filter_directories context =
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
  let root = Path.create_absolute (bracket_tmpdir context) in
  let check_path = Path.create_relative ~root ~relative:"check/a.py" in
  let ignore_path = Path.create_relative ~root ~relative:"ignore/b.py" in
  let files = [File.create ~content check_path; File.create ~content ignore_path] in
  assert_errors
    ~filter_directories:
      [Path.create_relative ~root ~relative:"check"; Path.create_relative ~root ~relative:"ignore"]
    ~root
    ~files
    [ "Incompatible return type [7]: Expected `C` but got `D`.";
      "Incompatible return type [7]: Expected `C` but got `D`." ];

  let root = Path.create_absolute (bracket_tmpdir context) in
  let check_path = Path.create_relative ~root ~relative:"check/a.py" in
  let ignore_path = Path.create_relative ~root ~relative:"ignore/b.py" in
  let files = [File.create ~content check_path; File.create ~content ignore_path] in
  assert_errors
    ~root
    ~filter_directories:[Path.create_relative ~root ~relative:"check"]
    ~ignore_all_errors:[Path.create_relative ~root ~relative:"check/search"]
    ~files
    ["Incompatible return type [7]: Expected `C` but got `D`."];

  (* The structure:
   *  /root/check <- pyre is meant to analyze here
   *  /root/check/search <- this is added to the search path, handles are relative to here instead
   *                       of check. The practical case here is resource_cache/typeshed. *)
  let root = Path.create_absolute (bracket_tmpdir context) in
  assert_errors
    ~root
    ~search_path:[SearchPath.Root (Path.create_relative ~root ~relative:"check/search")]
    ~filter_directories:[Path.create_relative ~root ~relative:"check"]
    ~ignore_all_errors:[Path.create_relative ~root ~relative:"check/search"]
    ~files:
      [ File.create ~content (Path.create_relative ~root ~relative:"check/file.py");
        File.create ~content (Path.create_relative ~root ~relative:"check/search/file.py") ]
    ["Incompatible return type [7]: Expected `C` but got `D`."];
  let root = Path.create_absolute (bracket_tmpdir context) in
  assert_errors
    ~root
    ~filter_directories:[Path.create_relative ~root ~relative:"check"]
    ~ignore_all_errors:[Path.create_relative ~root ~relative:"check/ignore"]
    ~files:[File.create ~content (Path.create_relative ~root ~relative:"check/ignore/file.py")]
    []


let () =
  "typeChecker"
  >::: [ "type_check_sources_list" >:: type_check_sources_list_test;
         "filter_directories" >:: test_filter_directories ]
  |> Test.run
