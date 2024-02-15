(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Base
open Buck_commands.Testing

let test_manifest_loading =
  let assert_failure_with_message ~expected actual =
    let message =
      Stdlib.Format.asprintf
        "Expected %s but got %a"
        expected
        Sexp.pp_hum
        ([%sexp_of: (Manifest.t, Manifest.Error.t) Result.t] actual)
    in
    assert_failure message
  in
  let assert_manifest ~context ~expected actual =
    match actual with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: Manifest.t]
          ~printer:(fun r -> [%sexp_of: Manifest.t] r |> Sexp.to_string_hum)
          expected
          actual
    | Result.Error _ -> assert_failure_with_message ~expected:"parsed manifest" actual
  in
  let assert_json_format_error = function
    | Result.Error (Manifest.Error.JsonFormatError _) -> ()
    | _ as actual -> assert_failure_with_message ~expected:"json format error" actual
  in
  let assert_json_parse_error = function
    | Result.Error (Manifest.Error.JsonParseError _) -> ()
    | _ as actual -> assert_failure_with_message ~expected:"json parse error" actual
  in
  let assert_file_read_error = function
    | Result.Error (Manifest.Error.FileReadError _) -> ()
    | _ as actual -> assert_failure_with_message ~expected:"file read error" actual
  in

  [
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context -> assert_manifest ~context (Manifest.load_from_string "[]") ~expected:[]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    assert_manifest
      ~context
      (Manifest.load_from_string {|[["foo.py", "src/foo.py", "//src:test derp"]]|})
      ~expected:[{ Manifest.Item.artifact_path = "foo.py"; source_path = "src/foo.py" }]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let content =
      {|[["foo.py", "src/foo.py", "//src:test"], ["bar.py", "src/bar.py", "//src:test"]]|}
    in
    assert_manifest
      ~context
      (Manifest.load_from_string content)
      ~expected:
        [
          { Manifest.Item.artifact_path = "foo.py"; source_path = "src/foo.py" };
          { Manifest.Item.artifact_path = "bar.py"; source_path = "src/bar.py" };
        ]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun _ -> assert_json_format_error (Manifest.load_from_string {|["foo.py", "src/foo.py"]|}));
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun _ -> assert_json_parse_error (Manifest.load_from_string "[derp]"));
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let relative = "test.manifest" in
    let root =
      TestHelper.setup_scratch_directory ~context [relative, {|[["foo.py", "src/foo.py", "derp"]]|}]
    in
    assert_manifest
      ~context
      (Manifest.load_from_file (PyrePath.create_relative ~root ~relative))
      ~expected:[{ Manifest.Item.artifact_path = "foo.py"; source_path = "src/foo.py" }]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root = TestHelper.setup_scratch_directory ~context [] in
    let path = PyrePath.create_relative ~root ~relative:"nonexistent" in
    assert_file_read_error (Manifest.load_from_file path));
  ]


let assert_equal_string_option ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: string option]
    ~printer:(fun value -> [%sexp_of: string option] value |> Sexp.to_string_hum)
    expected
    actual


let assert_equal_string_list ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: string list]
    ~printer:(fun value -> [%sexp_of: string list] value |> Sexp.to_string_hum)
    expected
    actual


let assert_lookup_source ~context ~expected Sourcedb.{ lookup = { Lookup.get_source; _ }; _ } key =
  let actual = get_source key in
  assert_equal_string_option ~context ~expected actual


let assert_lookup_dependency
    ~context
    ~expected
    Sourcedb.{ lookup = { Lookup.get_dependency; _ }; _ }
    key
  =
  let actual = get_dependency key in
  assert_equal_string_option ~context ~expected actual


let assert_all_sources ~context ~expected Sourcedb.{ listing = { Listing.all_sources; _ }; _ } =
  let actual = all_sources () in
  assert_equal_string_list ~context ~expected actual


let assert_all_dependencies
    ~context
    ~expected
    Sourcedb.{ listing = { Listing.all_dependencies; _ }; _ }
  =
  let actual = all_dependencies () in
  assert_equal_string_list ~context ~expected actual


let test_sourcedb_from_manifest =
  [
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let sourcedb =
      Sourcedb.create_from_manifests
        ~source_manifests:[]
        ~dependency_manifests:[]
        ~typeshed_manifests:[]
        ()
    in
    assert_lookup_source ~context sourcedb "foo.py" ~expected:None;
    assert_lookup_dependency ~context sourcedb "foo.py" ~expected:None;
    assert_all_sources ~context sourcedb ~expected:[];
    assert_all_dependencies ~context sourcedb ~expected:[]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let sourcedb =
      Sourcedb.create_from_manifests
        ~source_manifests:[[{ Manifest.Item.artifact_path = "foo.py"; source_path = "src/foo.py" }]]
        ~dependency_manifests:
          [[{ Manifest.Item.artifact_path = "bar.py"; source_path = "src/bar.py" }]]
        ~typeshed_manifests:
          [[{ Manifest.Item.artifact_path = "baz.pyi"; source_path = "src/baz.pyi" }]]
        ()
    in
    assert_lookup_source ~context sourcedb "foo.py" ~expected:(Some "src/foo.py");
    assert_lookup_source ~context sourcedb "bar.py" ~expected:None;
    assert_lookup_dependency ~context sourcedb "foo.py" ~expected:None;
    assert_lookup_dependency ~context sourcedb "bar.py" ~expected:(Some "src/bar.py");
    assert_lookup_dependency ~context sourcedb "baz.pyi" ~expected:(Some "src/baz.pyi");
    assert_all_sources ~context sourcedb ~expected:["foo.py"];
    assert_all_dependencies ~context sourcedb ~expected:["bar.py"; "baz.pyi"]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let sourcedb =
      Sourcedb.create_from_manifests
        ~source_manifests:
          [
            [{ Manifest.Item.artifact_path = "foo.py"; source_path = "src/d0/foo.py" }];
            [{ Manifest.Item.artifact_path = "foo.py"; source_path = "src/d1/foo.py" }];
          ]
        ~dependency_manifests:
          [
            [{ Manifest.Item.artifact_path = "bar.py"; source_path = "src/d0/bar.py" }];
            [{ Manifest.Item.artifact_path = "bar.py"; source_path = "src/d1/bar.py" }];
          ]
        ~typeshed_manifests:[]
        ()
    in
    assert_lookup_source ~context sourcedb "foo.py" ~expected:(Some "src/d1/foo.py");
    assert_lookup_dependency ~context sourcedb "bar.py" ~expected:(Some "src/d1/bar.py");
    assert_all_sources ~context sourcedb ~expected:["foo.py"];
    assert_all_dependencies ~context sourcedb ~expected:["bar.py"]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let sourcedb =
      Sourcedb.create_from_manifests
        ~source_manifests:[]
        ~dependency_manifests:
          [[{ Manifest.Item.artifact_path = "bar.pyi"; source_path = "dep/bar.pyi" }]]
        ~typeshed_manifests:
          [[{ Manifest.Item.artifact_path = "bar.pyi"; source_path = "typeshed/bar.pyi" }]]
        ()
    in
    assert_lookup_source ~context sourcedb "bar.pyi" ~expected:None;
    assert_lookup_dependency ~context sourcedb "bar.pyi" ~expected:(Some "dep/bar.pyi");
    assert_all_dependencies ~context sourcedb ~expected:["bar.pyi"]);
  ]


let assert_python_version ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: Configuration.PythonVersion.t]
    ~printer:(fun v -> [%sexp_of: Configuration.PythonVersion.t] v |> Sexp.to_string_hum)
    expected
    actual


let test_sourcedb_from_argfile =
  let assert_json_format_error = function
    | Result.Error (CheckCommandInput.Error.JsonFormatError _) -> ()
    | _ -> assert_failure "expected json format error"
  in
  let assert_json_parse_error = function
    | Result.Error (CheckCommandInput.Error.JsonParseError _) -> ()
    | _ -> assert_failure "expected json parse error"
  in
  let assert_file_read_error = function
    | Result.Error (CheckCommandInput.Error.FileReadError _) -> ()
    | _ -> assert_failure "expect file read error"
  in
  [
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root = TestHelper.setup_scratch_directory ~context [] in
    PyrePath.create_relative ~root ~relative:"nonexistent"
    |> CheckCommandInput.create_from_argument_file
    |> assert_file_read_error);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let relative = "test.json" in
    let root = TestHelper.setup_scratch_directory ~context [relative, "derp"] in
    PyrePath.create_relative ~root ~relative
    |> CheckCommandInput.create_from_argument_file
    |> assert_json_parse_error);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let relative = "test.json" in
    let root = TestHelper.setup_scratch_directory ~context [relative, "[]"] in
    PyrePath.create_relative ~root ~relative
    |> CheckCommandInput.create_from_argument_file
    |> assert_json_format_error);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root = bracket_tmpdir context |> PyrePath.create_absolute in
    let source_manifest_path = PyrePath.create_relative ~root ~relative:"source.manifest" in
    File.create source_manifest_path ~content:{|[["foo.py", "src/foo.py", "derp"]]|} |> File.write;
    let dependency_manifest_path = PyrePath.create_relative ~root ~relative:"dependency.manifest" in
    File.create dependency_manifest_path ~content:{|[["bar.py", "src/bar.py", "derp"]]|}
    |> File.write;
    let argfile_path = PyrePath.create_relative ~root ~relative:"args.json" in
    File.create
      argfile_path
      ~content:
        (Stdlib.Format.asprintf
           {|{ "sources": ["%a"], "dependencies": ["%a"], "py_version": "3.10.0" }|}
           PyrePath.pp
           source_manifest_path
           PyrePath.pp
           dependency_manifest_path)
    |> File.write;
    let { CheckCommandInput.get_source_db; get_python_version; _ } =
      match CheckCommandInput.create_from_argument_file argfile_path with
      | Result.Error error ->
          let message =
            Stdlib.Format.asprintf
              "Error loading check input argfile: %a"
              Sexp.pp_hum
              (CheckCommandInput.Error.sexp_of_t error)
          in
          assert_failure message
      | Result.Ok loaded -> loaded
    in
    let source_db = get_source_db () in
    assert_lookup_source ~context source_db "foo.py" ~expected:(Some "src/foo.py");
    assert_lookup_dependency ~context source_db "bar.py" ~expected:(Some "src/bar.py");
    assert_all_sources ~context source_db ~expected:["foo.py"];
    assert_all_dependencies ~context source_db ~expected:["bar.py"];
    let version = get_python_version () in
    assert_python_version
      ~context
      version
      ~expected:(Configuration.PythonVersion.create ~major:3 ~minor:10 ~micro:0 ()));
  ]


let test_parse_py_version =
  let assert_parsed ~expected input context =
    match CheckCommandInput.parse_py_version input with
    | Result.Error error ->
        let message =
          Stdlib.Format.asprintf
            "Error parsing py_version: %a"
            Sexp.pp_hum
            (CheckCommandInput.Error.sexp_of_t error)
        in
        assert_failure message
    | Result.Ok actual -> assert_python_version ~context ~expected actual
  in
  let assert_not_parsed input _ =
    match CheckCommandInput.parse_py_version input with
    | Result.Ok _ ->
        let message = Stdlib.Format.sprintf "Unexpected py_version parsing success on: %s" input in
        assert_failure message
    | Result.Error _ -> ()
  in
  [
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ assert_parsed "3" ~expected:(Configuration.PythonVersion.create ~major:3 ());
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ assert_parsed "3.8" ~expected:(Configuration.PythonVersion.create ~major:3 ~minor:8 ());
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ assert_parsed
         "3.8.6"
         ~expected:(Configuration.PythonVersion.create ~major:3 ~minor:8 ~micro:6 ());
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ @@ assert_not_parsed "";
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ @@ assert_not_parsed "abc";
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ @@ assert_not_parsed "python3.10";
    Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__ @@ assert_not_parsed "3.10python";
  ]


let () =
  "check_options"
  >::: [
         test_list test_manifest_loading;
         test_list test_sourcedb_from_manifest;
         test_list test_sourcedb_from_argfile;
         test_list test_parse_py_version;
       ]
  |> Test.run
