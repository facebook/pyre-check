(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Pyre
open Test

let ( ! ) = Path.create_absolute

let root context =
  let path = bracket_tmpdir context in
  let root = !path in
  path, root


let touch path = Stdio.Out_channel.write_all (Path.absolute path) ~data:""

let test_create context =
  let path, root = root context in
  (* Create absolute paths. *)
  assert_equal (root |> Path.show) path;

  (* Create relative paths. *)
  assert_equal (Path.create_relative ~root ~relative:"some/path" |> Path.show) (path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:(path ^/ "some/path") |> Path.show)
    (path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:"/other/root/some/path" |> Path.show)
    (path ^/ "other/root/some/path");

  (* Current directory. *)
  assert_equal (Path.current_working_directory () |> Path.show) (Sys.getcwd ())


let test_absolute context =
  let path, root = root context in
  assert_equal
    (Path.create_relative ~root ~relative:"some/path" |> Path.absolute)
    (path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:(path ^/ "some/path") |> Path.absolute)
    (path ^/ "some/path")


let test_get_relative_to_root context =
  let _, root = root context in
  let some = Path.create_relative ~root ~relative:"some/" in
  let relative = Path.create_relative ~root:some ~relative:"path" in
  let unrelated = Path.create_relative ~root ~relative:"other" in
  assert_equal (Path.get_relative_to_root ~root ~path:relative) (Some "some/path");
  assert_equal (Path.get_relative_to_root ~root:some ~path:relative) (Some "path");
  assert_equal (Path.get_relative_to_root ~root:some ~path:unrelated) None;
  assert_equal (Path.get_relative_to_root ~root ~path:unrelated) (Some "other")


let test_is_directory context =
  let path, _ = bracket_tmpfile context in
  assert_false (!path |> Path.is_directory);
  let path = bracket_tmpdir context in
  assert_true (!path |> Path.is_directory)


let test_is_python_file _ =
  let assert_stub ~path expected =
    let actual = Path.is_path_python_stub path in
    if expected then assert_true actual else assert_false actual
  in
  let assert_init ~path expected =
    let actual = Path.is_path_python_init path in
    if expected then assert_true actual else assert_false actual
  in
  assert_stub ~path:"test.py" false;
  assert_stub ~path:"test.pyi" true;
  assert_stub ~path:"durp/test.pyi" true;
  assert_init ~path:"test.py" false;
  assert_init ~path:"test.pyi" false;
  assert_init ~path:"__init__.py" true;
  assert_init ~path:"__init__.pyi" true;
  assert_init ~path:"durp/__init__.py" true;
  assert_init ~path:"durp/__init__.pyi" true;

  assert_stub ~path:"root/test" false;
  assert_stub ~path:"root/test.py" false;
  assert_stub ~path:"root/test.pyi" true;
  assert_stub ~path:"root/durp/test.pyi" true;
  assert_init ~path:"root/test.py" false;
  assert_init ~path:"root/test.pyi" false;
  assert_init ~path:"root/__init__.py" true;
  assert_init ~path:"root/__init__.pyi" true;
  assert_init ~path:"root/durp/__init__.py" true;
  assert_init ~path:"root/durp/__init__.pyi" true


let test_file_exists context =
  let path, _ = bracket_tmpfile context in
  assert_true (!path |> Path.file_exists);
  assert_false (Path.create_relative ~root:!path ~relative:"durp" |> Path.file_exists)


let test_last context =
  let _, root = root context in
  assert_equal (Path.last (Path.create_relative ~root ~relative:"some")) "some";
  assert_equal (Path.last (Path.create_relative ~root ~relative:"some/path")) "path"


let test_get_directory context =
  let _, root = root context in
  let assert_get_directory ~expected path =
    let actual = Path.get_directory path in
    assert_equal ~printer:Path.show ~cmp:Path.equal expected actual
  in
  let create_absolute = Path.create_absolute in
  assert_get_directory (create_absolute "/") ~expected:(create_absolute "/");
  assert_get_directory (create_absolute "/foo") ~expected:(create_absolute "/");
  assert_get_directory (create_absolute "/foo/bar") ~expected:(create_absolute "/foo");
  assert_get_directory (create_absolute "/foo/bar/baz") ~expected:(create_absolute "/foo/bar");
  assert_get_directory (Path.create_relative ~root ~relative:"foo") ~expected:root;
  assert_get_directory
    (Path.create_relative ~root ~relative:"foo/bar")
    ~expected:(Path.create_relative ~root ~relative:"foo");
  assert_get_directory
    (Path.create_relative ~root ~relative:"foo/bar/baz")
    ~expected:(Path.create_relative ~root ~relative:"foo/bar")


let test_directory_contains context =
  let _, root = root context in
  assert_equal
    (Path.directory_contains
       ~directory:root
       (Path.create_relative ~root ~relative:"nonexistent.py"))
    true;
  assert_equal
    (Path.directory_contains
       ~directory:(Path.create_relative ~root ~relative:"non")
       (Path.create_relative ~root ~relative:"nonexistent.py"))
    false


let test_create_directory_recursively context =
  let _, root = root context in
  let first_level = Path.create_relative ~root ~relative:"a" in
  let second_level = Path.create_relative ~root:first_level ~relative:"b" in
  let third_level = Path.create_relative ~root:second_level ~relative:"c" in
  Path.create_directory_recursively third_level |> Result.ok_or_failwith;

  assert_true (Path.is_directory first_level);
  assert_true (Path.is_directory second_level);
  assert_true (Path.is_directory third_level);
  ()


let test_remove context =
  let path, _ = bracket_tmpfile context in
  let path = !path in
  assert_true (Path.file_exists path);
  Path.remove path;
  assert_false (Path.file_exists path);
  Path.remove path


let test_remove_contents_of_directory context =
  let assert_success path =
    Path.remove_contents_of_directory path |> Result.ok_or_failwith;
    let elements = Sys.readdir (Path.absolute path) in
    assert_true (Array.is_empty elements)
  in
  let assert_failure path =
    match Path.remove_contents_of_directory path with
    | Result.Ok () -> assert_failure "Unexpected success on `ensure_parent_directory`"
    | _ -> ()
  in

  let _, root = root context in
  (* Empty directory *)
  let root0 = Path.create_relative ~root ~relative:"test0" in
  Unix.mkdir (Path.absolute root0);
  assert_success root0;

  (* Files *)
  let root1 = Path.create_relative ~root ~relative:"test1" in
  Unix.mkdir (Path.absolute root1);
  touch (Path.create_relative ~root:root1 ~relative:"file");
  assert_success root1;

  (* Subdirectory *)
  let root2 = Path.create_relative ~root ~relative:"test2" in
  let subdirectory = Path.create_relative ~root:root2 ~relative:"subdirectory" in
  Unix.mkdir (Path.absolute root2);
  Unix.mkdir (Path.absolute subdirectory);
  touch (Path.create_relative ~root:subdirectory ~relative:"file");
  assert_success root2;

  (* Mixed *)
  let root3 = Path.create_relative ~root ~relative:"test3" in
  let subdirectory = Path.create_relative ~root:root3 ~relative:"subdirectory" in
  Unix.mkdir (Path.absolute root3);
  touch (Path.create_relative ~root:root3 ~relative:"file0");
  Unix.mkdir (Path.absolute subdirectory);
  touch (Path.create_relative ~root:subdirectory ~relative:"file1");
  assert_success root3;

  (* Not a directory *)
  let not_a_directory = Path.create_relative ~root ~relative:"not_a_directory" in
  touch not_a_directory;
  assert_failure not_a_directory;

  (* Directory does not exist *)
  let does_not_exist = Path.create_relative ~root ~relative:"does_not_exist" in
  assert_failure does_not_exist;
  ()


let () =
  "path"
  >::: [
         "create" >:: test_create;
         "absolute" >:: test_absolute;
         "directory_contains" >:: test_directory_contains;
         "get_relative_to_root" >:: test_get_relative_to_root;
         "is_directory" >:: test_is_directory;
         "is_python_file" >:: test_is_python_file;
         "file_exists" >:: test_file_exists;
         "last" >:: test_last;
         "get_directory" >:: test_get_directory;
         "create_directory_recursively" >:: test_create_directory_recursively;
         "remove" >:: test_remove;
         "remove_contents_of_directory" >:: test_remove_contents_of_directory;
       ]
  |> Test.run
