(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Base
open Buck_commands.Testing

let setup_scratch_directory ~context relatives_and_contents =
  let root = bracket_tmpdir context |> PyrePath.create_absolute in
  List.iter relatives_and_contents ~f:(fun (relative, content) ->
      let file = PyrePath.create_relative ~root ~relative in
      File.create file ~content |> File.write);
  root


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
      setup_scratch_directory ~context [relative, {|[["foo.py", "src/foo.py", "derp"]]|}]
    in
    assert_manifest
      ~context
      (Manifest.load_from_file (PyrePath.create_relative ~root ~relative))
      ~expected:[{ Manifest.Item.artifact_path = "foo.py"; source_path = "src/foo.py" }]);
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root = setup_scratch_directory ~context [] in
    let path = PyrePath.create_relative ~root ~relative:"nonexistent" in
    assert_file_read_error (Manifest.load_from_file path));
  ]


let () = "check_options" >::: [test_list test_manifest_loading] |> Test.run
