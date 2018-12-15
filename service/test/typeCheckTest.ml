(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Test


let assert_errors ?filter_directories ~root ~files errors =
  let configuration =
    Configuration.Analysis.create ?filter_directories ~project_root:root ~local_root:root ()
  in
  let scheduler = Scheduler.mock () in
  let add_file file =
    let content = Option.value_exn (File.content file) in
    let path = Path.absolute (File.path file) in
    begin
      try
        Unix.mkdir (Filename.dirname path)
      with Unix.Unix_error _ ->
        ()
    end;
    Out_channel.write_all path ~data:content
  in
  List.iter ~f:add_file files;
  let { Service.Parser.parsed = handles; _ } =
    Service.Parser.parse_sources ~configuration ~scheduler ~files
  in
  let ((module Handler: Analysis.Environment.Handler) as environment) =
    (module Service.Environment.SharedHandler: Analysis.Environment.Handler)
  in
  Service.Environment.populate_shared_memory ~configuration ~stubs:[] ~sources:handles;
  let actual_errors =
    Service.Check.analyze_sources ~scheduler ~configuration ~environment ~handles
    |> fst
    |> List.map ~f:(Analysis.Error.description ~detailed:false)
  in
  Handler.purge handles;
  assert_equal
    ~printer:(List.to_string ~f:ident)
    ~cmp:(List.equal ~equal:String.equal)
    errors
    actual_errors


let type_check_sources_list_test context =
  let create_files ~root content =
    let default_content =
      {|
        class object():
          def __sizeof__() -> int: pass
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
    let path, _ = Filename.open_temp_file ~in_dir:(Path.absolute root) "test" ".py" in
    [
      File.create
        ~content:(default_content ^ "\n" ^ (content |> trim_extra_indentation))
        (Path.create_relative ~root ~relative:path);
    ]
  in
  let check _ =
    let root = Path.current_working_directory () in
    let files =
      {|
        def foo() -> str:
          return 1
      |}
      |> create_files ~root
    in
    assert_errors ~root ~files ["Incompatible return type [7]: Expected `str` but got `int`."]
  in
  with_bracket_chdir context (bracket_tmpdir context) check


let test_filter_directories context =
  let root = Path.create_absolute (bracket_tmpdir context) in
  let check_path = Path.create_relative ~root ~relative:"check/a.py" in
  let ignore_path = Path.create_relative ~root ~relative:"ignore/b.py" in
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
  let files = [File.create ~content check_path; File.create ~content ignore_path] in
  assert_errors
    ~root
    ~files
    [
      "Incompatible return type [7]: Expected `C` but got `D`.";
      "Incompatible return type [7]: Expected `C` but got `D`.";
    ];

  assert_errors
    ~root
    ~filter_directories:[Path.create_relative ~root ~relative:"check"]
    ~files
    ["Incompatible return type [7]: Expected `C` but got `D`."]


let () =
  "typeChecker">:::[
    "type_check_sources_list">::type_check_sources_list_test;
    "filter_directories">::test_filter_directories;
  ]
  |> Test.run
