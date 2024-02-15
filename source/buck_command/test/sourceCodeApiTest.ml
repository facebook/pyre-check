(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Base
open Buck_commands.Testing

let test_file_loader =
  let assert_loaded ~context ~expected ~loader:{ FileLoader.load } path =
    match load path with
    | Result.Error message -> assert_failure message
    | Result.Ok actual ->
        assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected actual
  in
  let assert_not_loaded ~loader:{ FileLoader.load } path =
    match load path with
    | Result.Ok content ->
        let message =
          Stdlib.Format.sprintf "Unexpected load success on path `%s`: %s" path content
        in
        assert_failure message
    | Result.Error _ -> ()
  in
  [
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root =
      TestHelper.setup_scratch_directory ~context ["src/foo.py", "x = 42"; "src/bar.py", "y = 43"]
    in
    let lookup =
      Sourcedb.Lookup.create_for_testing
        ~sources:["foo.py", "src/foo.py"]
        ~dependencies:["bar.py", "src/bar.py"]
        ()
    in
    let loader = FileLoader.create_from_sourcedb_lookup ~root lookup in
    assert_loaded ~context ~loader "foo.py" ~expected:"x = 42";
    assert_loaded ~context ~loader "bar.py" ~expected:"y = 43";
    assert_not_loaded ~loader "baz.py");
    (Test.labeled_test_case Stdlib.__FUNCTION__ Stdlib.__LINE__
    @@ fun context ->
    let root =
      TestHelper.setup_scratch_directory ~context ["src/test.py", "0"; "dep/test.py", "1"]
    in
    let lookup =
      Sourcedb.Lookup.create_for_testing
        ~sources:["test.py", "src/test.py"]
        ~dependencies:["test.py", "dep/test.py"; "not_on_filesystem.py", "src/not_on_filesystem.py"]
        ()
    in
    let loader = FileLoader.create_from_sourcedb_lookup ~root lookup in
    assert_loaded ~context ~loader "test.py" ~expected:"0";
    assert_not_loaded ~loader "not_on_filesystem.py";
    assert_not_loaded ~loader "not_in_lookup.py");
  ]


let () = "source_code_api" >::: [test_list test_file_loader] |> Test.run
