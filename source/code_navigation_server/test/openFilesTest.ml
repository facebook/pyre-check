(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open CodeNavigationServer

let ( ! ) path = PyrePath.create_absolute path |> SourcePath.create

let test_open_files _ =
  let open_files = OpenFiles.create () in
  let assert_open_files open_files ~expected =
    assert_equal
      ~cmp:(List.equal String.equal)
      (OpenFiles.open_files open_files
      |> List.map ~f:(fun source_path -> SourcePath.raw source_path |> PyrePath.absolute)
      |> List.sort ~compare:String.compare)
      expected
  in
  assert_open_files open_files ~expected:[];

  OpenFiles.open_file open_files ~source_path:!"/a/b.py" ~overlay_id:None;
  assert_open_files open_files ~expected:["/a/b.py"];
  let response = OpenFiles.close_file open_files ~source_path:!"/a/b.py" ~overlay_id:None in
  assert_equal response (Result.Ok ());
  assert_open_files open_files ~expected:[];

  OpenFiles.open_file open_files ~source_path:!"/a/b.py" ~overlay_id:(Some "1");
  assert_open_files open_files ~expected:["/a/b.py"];

  OpenFiles.open_file open_files ~source_path:!"/a/b.py" ~overlay_id:(Some "2");
  assert_open_files open_files ~expected:["/a/b.py"];

  OpenFiles.open_file open_files ~source_path:!"/b/c.py" ~overlay_id:(Some "3");
  assert_open_files open_files ~expected:["/a/b.py"; "/b/c.py"];

  let response = OpenFiles.close_file open_files ~source_path:!"/b/c.py" ~overlay_id:(Some "3") in
  assert_equal response (Result.Ok ());
  assert_open_files open_files ~expected:["/a/b.py"];

  let response = OpenFiles.close_file open_files ~source_path:!"/a/b.py" ~overlay_id:(Some "2") in
  assert_equal response (Result.Ok ());
  assert_open_files open_files ~expected:["/a/b.py"];

  let response = OpenFiles.close_file open_files ~source_path:!"/a/b.py" ~overlay_id:(Some "1") in
  assert_equal response (Result.Ok ());
  assert_open_files open_files ~expected:[];

  let response = OpenFiles.close_file open_files ~source_path:!"/a/b.py" ~overlay_id:(Some "1") in
  assert_bool "Expected an error response" (response != Result.Ok ());
  ()


let () = "open_files_test" >::: ["test_open_files" >:: test_open_files] |> Test.run
