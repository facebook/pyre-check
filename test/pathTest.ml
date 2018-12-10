(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Test


let (!) =
  Path.create_absolute


let root context =
  let path = bracket_tmpdir context in
  let root = !path in
  path, root


let test_create context =
  let path, root = root context in

  (* Create absolute paths. *)
  assert_equal (root |> Path.show) path;

  (* Create relative paths. *)
  assert_equal
    (Path.create_relative ~root ~relative:"some/path" |> Path.show)
    (path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:(path ^/ "some/path") |> Path.show)
    (path ^/ "some/path");
  assert_equal
    (Path.create_relative ~root ~relative:"/other/root/some/path" |> Path.show)
    (path ^/ "other/root/some/path");

  (* Current directory. *)
  assert_equal (Path.current_working_directory () |> Path.show) (Sys.getcwd ())


let test_create_search_path context =
  let _, root = root context in
  (* Create root/subdirectory. *)
  let subdirectory = Path.create_relative ~root ~relative:"subdirectory" in
  subdirectory
  |> Path.show
  |> Sys_utils.mkdir_no_fail;

  assert_equal
    ~cmp:Path.SearchPath.equal
    (Path.SearchPath.Root root)
    (Path.SearchPath.create (Path.show root));
  assert_equal
    ~cmp:Path.SearchPath.equal
    (Path.SearchPath.Root subdirectory)
    (Path.SearchPath.create (Path.show subdirectory));
  assert_equal
    ~cmp:Path.SearchPath.equal
    (Path.SearchPath.Subdirectory { root; subdirectory = "subdirectory" })
    (Path.SearchPath.create (Path.show root ^ "$subdirectory"));

  assert_raises
    (Failure "Unable to create search path from too$many$levels")
    (fun () -> Path.SearchPath.create "too$many$levels")

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
  begin
    match Path.from_uri uri with
    | Some path_from_uri -> assert_equal root path_from_uri
    | None -> assert_unreached ()
  end;

  let invalid_schema = "invalid-schema://" ^ path in
  Path.from_uri invalid_schema
  |> Option.is_none
  |> assert_true;

  let invalid_uri = "file://" ^ path ^/ "some/invalid/uri" in
  try
    Path.from_uri invalid_uri |> ignore;
    assert_unreached ()
  with
  | Unix.Unix_error (error, _name, parameter) ->
      begin
        assert_equal
          ~printer:ident
          "No such file or directory"
          (Unix.Error.message error);
        assert_equal
          ~printer:ident
          (path ^/ "some/invalid/uri")
          parameter
      end


let test_get_relative_to_root context =
  let _, root = root context in
  let some = Path.create_relative ~root ~relative:"some/" in
  let relative = Path.create_relative ~root:some ~relative:"path" in
  let unrelated = Path.create_relative ~root ~relative:"other" in
  assert_equal
    (Path.get_relative_to_root ~root ~path:relative)
    (Some "some/path");
  assert_equal
    (Path.get_relative_to_root ~root:some ~path:relative)
    (Some "path");
  assert_equal
    (Path.get_relative_to_root ~root:some ~path:unrelated)
    None;
  assert_equal
    (Path.get_relative_to_root ~root ~path:unrelated)
    (Some "other")


let test_append context =
  let path, root = root context in
  assert_equal ~printer:Fn.id (root |> Path.append ~element:"durp" |> Path.show) (path ^/ "durp");
  assert_equal
    (Path.create_relative ~root ~relative:"path"
     |> Path.append ~element:"durp"
     |> Path.show)
    (path ^/ "path/durp");

  let open Path.AppendOperator in
  assert_equal (root ^| "durp" |> Path.show) (path ^/ "durp");
  assert_equal ((root ^| "path") ^| "durp" |> Path.show) (path ^/ "path/durp")


let test_is_directory context =
  let path, _ = bracket_tmpfile context in
  assert_false (!path |> Path.is_directory);

  let path = bracket_tmpdir context in
  assert_true (!path |> Path.is_directory)


let test_file_exists context =
  let path, _ = bracket_tmpfile context in
  assert_true (!path |> Path.file_exists);

  assert_false (Path.create_relative ~root:!path ~relative:"durp" |> Path.file_exists)


let test_last context =
  let _, root = root context in
  assert_equal
    (Path.last (Path.create_relative ~root ~relative:"some"))
    "some";
  assert_equal
    (Path.last (Path.create_relative ~root ~relative:"some/path"))
    "path"


let test_directory_contains context =
  let _, root = root context in
  assert_equal
    (Path.directory_contains
       ~follow_symlinks:true
       ~directory:root
       (Path.create_relative ~root ~relative:"nonexistent.py"))
    false


let test_link context =
  let path, root = root context in
  let link = path ^ "-link" in
  let linklink = link ^ "-link" in
  Unix.symlink ~src:path ~dst:link;
  Unix.symlink ~src:link ~dst:linklink;
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
  Path.remove path  (* Yolo *)


let () =
  "path">:::[
    "create">::test_create;
    "relative">::test_relative;
    "absolute">::test_absolute;
    "directory_contains">::test_directory_contains;
    "uri">::test_uri;
    "from_uri">::test_from_uri;
    "get_relative_to_root">::test_get_relative_to_root;
    "append">::test_append;
    "is_directory">::test_is_directory;
    "file_exists">::test_file_exists;
    "last">::test_last;
    "link">::test_link;
    "remove">::test_remove;
    "create_search_path">::test_create_search_path;
  ]
  |> Test.run
