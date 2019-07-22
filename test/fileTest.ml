(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre
open Test

let test_content context =
  let data = "file" in
  let path, _ = bracket_tmpfile context in
  Out_channel.write_all ~data path;
  let path = Path.create_absolute path in
  assert_equal (File.create path |> File.content) (Some data);
  assert_equal (File.create ~content:"content" path |> File.content) (Some "content");
  assert_is_none (File.create (Path.create_relative ~root:path ~relative:"derp") |> File.content)


let test_lines context =
  let path, _ = bracket_tmpfile context in
  assert_equal
    ~cmp:(List.equal String.equal)
    ( File.create ~content:"foo\nbar" (Path.create_absolute path)
    |> File.lines
    |> fun lines -> Option.value_exn lines )
    ["foo"; "bar"]


let test_is_stub _ =
  assert_true (File.Handle.is_stub (File.Handle.create_for_testing "a.pyi"));
  assert_true (File.Handle.is_stub (File.Handle.create_for_testing "pyi.pyi"));
  assert_false (File.Handle.is_stub (File.Handle.create_for_testing "pyi.py"));
  assert_false (File.Handle.is_stub (File.Handle.create_for_testing "a.py"))


let test_handle _ =
  let assert_handle ~absolute ~handle =
    let path = Path.create_absolute ~follow_symbolic_links:false in
    let configuration =
      Configuration.Analysis.create
        ~local_root:(path "/root")
        ~search_path:
          [ SearchPath.Root (path "/root/stubs");
            SearchPath.Root (path "/external");
            SearchPath.Subdirectory { root = path "/virtualenv"; subdirectory = "importMe" };
            SearchPath.Root (path "/typeshed/stdlib/3");
            SearchPath.Root (path "/typeshed/third_party/3") ]
        ()
    in
    match handle with
    | None ->
        let message =
          let roots =
            List.to_string
              [ "/root/stubs";
                "/external";
                "/virtualenv/importMe";
                "/typeshed/stdlib/3";
                "/typeshed/third_party/3";
                "/root" ]
              ~f:ident
          in
          Format.sprintf "Unable to construct handle for %s. Possible roots: %s" absolute roots
        in
        assert_raises (File.NonexistentHandle message) (fun () ->
            File.handle ~configuration (File.create (path absolute)))
    | Some handle ->
        let expected =
          File.handle ~configuration (File.create (path absolute)) |> File.Handle.show
        in
        assert_equal expected handle
  in
  assert_handle ~absolute:"/root/a.py" ~handle:(Some "a.py");
  assert_handle ~absolute:"/external/b/c.py" ~handle:(Some "b/c.py");
  assert_handle ~absolute:"/root/stubs/stub.pyi" ~handle:(Some "stub.pyi");
  assert_handle ~absolute:"/typeshed/stdlib/3/builtins.pyi" ~handle:(Some "builtins.pyi");
  assert_handle ~absolute:"/typeshed/third_party/3/django.pyi" ~handle:(Some "django.pyi");
  assert_handle ~absolute:"/typeshed/3/whoops.pyi" ~handle:None;
  assert_handle ~absolute:"/untracked/a.py" ~handle:None;
  assert_handle ~absolute:"/virtualenv/importMe/a.py" ~handle:(Some "importMe/a.py")


let test_handle_to_path context =
  (* Set up a directory structure that looks like this:
   * /local/a.py
   * /local/matching.py
   * /other/b.py
   * /other/matching.py
   * /virtualEnv/importMe/a.py
   *)
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let other_root = bracket_tmpdir context |> Path.create_absolute in
  let virtualenv = bracket_tmpdir context in
  Sys_utils.mkdir_no_fail (virtualenv ^ "/importMe");
  Sys_utils.mkdir_no_fail (virtualenv ^ "/doNotImport");
  let import_me = virtualenv ^ "/importMe" |> Path.create_absolute in
  let do_not_import = virtualenv ^ "/doNotImport" |> Path.create_absolute in
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~search_path:
        [ SearchPath.Root other_root;
          SearchPath.Subdirectory
            { root = Path.create_absolute virtualenv; subdirectory = "importMe" } ]
      ()
  in
  let touch root relative =
    Path.create_relative ~root ~relative |> File.create ~content:"" |> File.write
  in
  touch local_root "a.py";
  touch local_root "matching.py";
  touch other_root "b.py";
  touch other_root "matching.py";
  touch import_me "a.py";
  touch do_not_import "a.py";

  (* Check that we can recover paths from handles. *)
  let assert_path ~handle ~path =
    match File.Handle.to_path ~configuration (File.Handle.create_for_testing handle) with
    | None -> assert_unreached ()
    | Some actual -> assert_equal ~printer:Path.show ~cmp:Path.equal path actual
  in
  let assert_not_path ~handle =
    assert_is_none (File.Handle.to_path ~configuration (File.Handle.create_for_testing handle))
  in
  assert_path ~handle:"a.py" ~path:(Path.create_relative ~root:local_root ~relative:"a.py");
  assert_path ~handle:"b.py" ~path:(Path.create_relative ~root:other_root ~relative:"b.py");
  assert_path
    ~handle:"matching.py"
    ~path:(Path.create_relative ~root:other_root ~relative:"matching.py");
  assert_not_path ~handle:"nonexistent.py";
  assert_path ~handle:"importMe/a.py" ~path:(Path.create_relative ~root:import_me ~relative:"a.py");
  assert_not_path ~handle:"doNotImport/a.py"


let () =
  "file"
  >::: [ "content" >:: test_content;
         "lines" >:: test_lines;
         "handle" >:: test_handle;
         "is_stub" >:: test_is_stub;
         "handle_to_path" >:: test_handle_to_path ]
  |> Test.run
