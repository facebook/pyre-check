(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Pyre

let ( ! ) = Path.create_absolute

let root context =
  let path = bracket_tmpdir context in
  let root = !path in
  path, root


let test_create_search_path context =
  let _, root = root context in
  (* Create root/subdirectory. *)
  let subdirectory = Path.create_relative ~root ~relative:"subdirectory" in
  subdirectory |> Path.show |> Sys_utils.mkdir_no_fail;
  assert_equal ~cmp:SearchPath.equal (SearchPath.Root root) (SearchPath.create (Path.show root));
  assert_equal
    ~cmp:SearchPath.equal
    (SearchPath.Root subdirectory)
    (SearchPath.create (Path.show subdirectory));
  assert_equal
    ~cmp:SearchPath.equal
    (SearchPath.Subdirectory { root; subdirectory = "subdirectory" })
    (SearchPath.create (Path.show root ^ "$subdirectory"));
  assert_raises (Failure "Unable to create search path from too$many$levels") (fun () ->
      SearchPath.create "too$many$levels")


let test_search_for_path context =
  let root = OUnit2.bracket_tmpdir context |> Path.create_absolute in
  let assert_path ~search_path ~path ~expected =
    assert_equal
      (Some expected)
      ( SearchPath.search_for_path ~search_path path
      >>| fun SearchPath.{ relative_path; _ } -> Path.RelativePath.relative relative_path )
  in
  let search_path =
    [ SearchPath.Subdirectory { root; subdirectory = "a" };
      SearchPath.Subdirectory
        { root = Path.create_relative ~root ~relative:"b"; subdirectory = "c" };
      SearchPath.Subdirectory { root; subdirectory = "b" } ]
  in
  assert_path
    ~search_path
    ~path:(Path.create_relative ~root ~relative:"a/file.py")
    ~expected:"a/file.py";
  assert_path
    ~search_path
    ~path:(Path.create_relative ~root ~relative:"b/c/file.py")
    ~expected:"c/file.py";
  assert_path
    ~search_path
    ~path:(Path.create_relative ~root ~relative:"b/other/file.py")
    ~expected:"b/other/file.py"


let () =
  "searchPath"
  >::: [ "create_search_path" >:: test_create_search_path;
         "search_for_path" >:: test_search_for_path ]
  |> Test.run
