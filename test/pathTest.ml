(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre
open Test

let ( ! ) = Path.create_absolute

let root context =
  let path = bracket_tmpdir context in
  let root = !path in
  path, root


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


let test_relative context =
  let _, root = root context in
  assert_is_none (root |> Path.relative);
  assert_equal
    (Path.create_relative ~root ~relative:"some/path" |> Path.relative)
    (Some "some/path")


let test_absolute context =
  let path, root = root context in
  assert_equal
    (Path.create_relative ~root ~relative:"some/path" |> Path.absolute)
    (path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:(path ^/ "some/path") |> Path.absolute)
    (path ^/ "some/path")


let test_uri context =
  let path, root = root context in
  assert_equal
    (Path.create_relative ~root ~relative:"some/path" |> Path.uri)
    ("file://" ^ path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:(path ^/ "some/path") |> Path.uri)
    ("file://" ^ path ^/ "some/path")


let test_from_uri context =
  let path, root = root context in
  let uri = "file://" ^ path in
  ( match Path.from_uri uri with
  | Some path_from_uri -> assert_equal root path_from_uri
  | None -> assert_unreached () );
  let invalid_schema = "invalid-schema://" ^ path in
  Path.from_uri invalid_schema |> Option.is_none |> assert_true;
  let invalid_uri = "file://" ^ path ^/ "some/invalid/uri" in
  try
    Path.from_uri invalid_uri |> ignore;
    assert_unreached ()
  with
  | Unix.Unix_error (error, _name, parameter) ->
      assert_equal ~printer:ident "No such file or directory" (Unix.Error.message error);
      assert_equal ~printer:ident (path ^/ "some/invalid/uri") parameter


let test_get_relative_to_root context =
  let _, root = root context in
  let some = Path.create_relative ~root ~relative:"some/" in
  let relative = Path.create_relative ~root:some ~relative:"path" in
  let unrelated = Path.create_relative ~root ~relative:"other" in
  assert_equal (Path.get_relative_to_root ~root ~path:relative) (Some "some/path");
  assert_equal (Path.get_relative_to_root ~root:some ~path:relative) (Some "path");
  assert_equal (Path.get_relative_to_root ~root:some ~path:unrelated) None;
  assert_equal (Path.get_relative_to_root ~root ~path:unrelated) (Some "other")


let test_append context =
  let path, root = root context in
  assert_equal ~printer:Fn.id (root |> Path.append ~element:"durp" |> Path.show) (path ^/ "durp");
  assert_equal
    (Path.create_relative ~root ~relative:"path" |> Path.append ~element:"durp" |> Path.show)
    (path ^/ "path/durp");
  let open Path.AppendOperator in
  assert_equal (root ^| "durp" |> Path.show) (path ^/ "durp");
  assert_equal ((root ^| "path") ^| "durp" |> Path.show) (path ^/ "path/durp");
  assert_equal ((root ^| "") ^| "durp" |> Path.show) (path ^/ "durp")


let test_is_directory context =
  let path, _ = bracket_tmpfile context in
  assert_false (!path |> Path.is_directory);
  let path = bracket_tmpdir context in
  assert_true (!path |> Path.is_directory)


let test_is_python_file _ =
  let assert_stub ~path expected =
    let actual = Path.is_python_stub path in
    if expected then assert_true actual else assert_false actual
  in
  let assert_init ~path expected =
    let actual = Path.is_python_init path in
    if expected then assert_true actual else assert_false actual
  in
  assert_stub ~path:(Path.create_absolute ~follow_symbolic_links:false "test.py") false;
  assert_stub ~path:(Path.create_absolute ~follow_symbolic_links:false "test.pyi") true;
  assert_stub ~path:(Path.create_absolute ~follow_symbolic_links:false "durp/test.pyi") true;
  assert_init ~path:(Path.create_absolute ~follow_symbolic_links:false "test.py") false;
  assert_init ~path:(Path.create_absolute ~follow_symbolic_links:false "test.pyi") false;
  assert_init ~path:(Path.create_absolute ~follow_symbolic_links:false "__init__.py") true;
  assert_init ~path:(Path.create_absolute ~follow_symbolic_links:false "__init__.pyi") true;
  assert_init ~path:(Path.create_absolute ~follow_symbolic_links:false "durp/__init__.py") true;
  assert_init ~path:(Path.create_absolute ~follow_symbolic_links:false "durp/__init__.pyi") true;

  let root = Path.create_absolute ~follow_symbolic_links:false "root" in
  assert_stub ~path:(Path.create_relative ~root ~relative:"test") false;
  assert_stub ~path:(Path.create_relative ~root ~relative:"test.py") false;
  assert_stub ~path:(Path.create_relative ~root ~relative:"test.pyi") true;
  assert_stub ~path:(Path.create_relative ~root ~relative:"durp/test.pyi") true;
  assert_init ~path:(Path.create_relative ~root ~relative:"test.py") false;
  assert_init ~path:(Path.create_relative ~root ~relative:"test.pyi") false;
  assert_init ~path:(Path.create_relative ~root ~relative:"__init__.py") true;
  assert_init ~path:(Path.create_relative ~root ~relative:"__init__.pyi") true;
  assert_init ~path:(Path.create_relative ~root ~relative:"durp/__init__.py") true;
  assert_init ~path:(Path.create_relative ~root ~relative:"durp/__init__.pyi") true


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
  let create_absolute = Path.create_absolute ~follow_symbolic_links:false in
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
    true


let test_link context =
  let path, root = root context in
  let link = path ^ "-link" in
  let linklink = link ^ "-link" in
  Unix.symlink ~target:path ~link_name:link;
  Unix.symlink ~target:link ~link_name:linklink;
  let symbolic = Path.create_absolute ~follow_symbolic_links:false link in
  let link = Path.create_absolute link in
  let linklink = Path.create_absolute linklink in
  assert_equal root (Path.real_path root);
  assert_equal root (Path.real_path link);
  assert_equal root (Path.real_path linklink);
  assert_equal (path ^ "-link") (Path.absolute symbolic);
  Unix.remove (Path.absolute link);
  assert_equal link (Path.real_path link);
  assert_equal link (Path.real_path linklink)


let test_remove context =
  let path, _ = bracket_tmpfile context in
  let path = !path in
  assert_true (Path.file_exists path);
  Path.remove path;
  assert_false (Path.file_exists path);
  Path.remove path


(* Yolo *)

let test_build_symlink_map context =
  let root = bracket_tmpdir context |> Path.create_absolute in
  let path relative = Path.create_relative ~root ~relative in
  let create_file path = Out_channel.write_all ~data:"" (Path.absolute path) in
  let link = path "link.py" in
  let target = path "original.py" in
  create_file target;
  Unix.symlink ~target:(Path.absolute target) ~link_name:(Path.absolute link);
  let assert_keys ~links expected =
    let expected_map = Path.Map.of_alist_exn expected in
    let map = Path.build_symlink_map ~links in
    assert_equal ~cmp:(Path.Map.equal Path.equal) expected_map map
  in
  assert_keys ~links:[link] [target, link];
  let broken = path "broken.py" in
  create_file broken;
  let broken_link = path "broken_link.py" in
  Unix.symlink ~target:(Path.absolute broken) ~link_name:(Path.absolute broken_link);
  Unix.remove (Path.absolute broken);
  assert_keys ~links:[link; broken_link] [target, link];
  let nonexistent = path "nonexistent.py" in
  assert_keys ~links:[link; broken_link; nonexistent] [target, link]


let () =
  "path"
  >::: [ "create" >:: test_create;
         "relative" >:: test_relative;
         "absolute" >:: test_absolute;
         "directory_contains" >:: test_directory_contains;
         "uri" >:: test_uri;
         "from_uri" >:: test_from_uri;
         "get_relative_to_root" >:: test_get_relative_to_root;
         "append" >:: test_append;
         "is_directory" >:: test_is_directory;
         "is_python_file" >:: test_is_python_file;
         "file_exists" >:: test_file_exists;
         "last" >:: test_last;
         "get_directory" >:: test_get_directory;
         "link" >:: test_link;
         "remove" >:: test_remove;
         "build_symlink_map" >:: test_build_symlink_map ]
  |> Test.run
