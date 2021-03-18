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

let test_restore_symbolic_links context =
  let project_root = bracket_tmpdir context |> Path.create_absolute ~follow_symbolic_links:true in
  let local_root = bracket_tmpdir context |> Path.create_absolute ~follow_symbolic_links:true in
  let path name = Path.create_relative ~root:project_root ~relative:name in
  let create_file name = path name |> File.create ~content:"" |> File.write in
  let link name =
    let actual = Path.absolute (path name) in
    let link = Path.create_relative ~root:local_root ~relative:name |> Path.absolute in
    Unix.symlink ~target:actual ~link_name:link
  in
  create_file "a.py";
  create_file "b.py";
  link "a.py";
  link "b.py";
  Path.create_relative ~root:local_root ~relative:"unlinked.py"
  |> File.create ~content:""
  |> File.write;
  let assert_restored ~names ~expected =
    let get_old_link_path =
      let map =
        Path.Map.of_alist_exn
          [
            path "a.py", Path.create_relative ~root:local_root ~relative:"a.py";
            path "b.py", Path.create_relative ~root:local_root ~relative:"unused_link.py";
            path "removed.py", Path.create_relative ~root:local_root ~relative:"removed.py";
          ]
      in
      Map.find map
    in
    assert_equal
      ~printer:(List.to_string ~f:Path.show)
      ~cmp:(List.equal Path.equal)
      expected
      (Server.SavedState.restore_symbolic_links
         ~changed_paths:names
         ~source_path:[local_root]
         ~get_old_link_path)
  in
  (* For changed files, the new set of links is prioritized. *)
  assert_restored
    ~names:[path "a.py"; path "b.py"]
    ~expected:
      [
        Path.create_relative ~root:local_root ~relative:"a.py";
        Path.create_relative ~root:local_root ~relative:"b.py";
      ];

  (* We only get paths that are passed in. *)
  assert_restored
    ~names:[path "a.py"]
    ~expected:[Path.create_relative ~root:local_root ~relative:"a.py"];

  (* We relativize files not present in the new set of files. *)
  assert_restored
    ~names:[path "a.py"; path "removed.py"]
    ~expected:
      [
        Path.create_relative ~root:local_root ~relative:"a.py";
        Path.create_relative ~root:local_root ~relative:"removed.py";
      ];
  assert_restored
    ~names:[Path.create_relative ~root:local_root ~relative:"unlinked.py"]
    ~expected:[Path.create_relative ~root:local_root ~relative:"unlinked.py"];

  (* Do not drop paths even if they might not correspond to an old link. *)
  assert_restored ~names:[path "nonexist.py"] ~expected:[path "nonexist.py"];
  ()


let () = "saved_state" >::: ["restore_symbolic_links" >:: test_restore_symbolic_links] |> Test.run
