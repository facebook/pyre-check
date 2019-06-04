(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre

let test_restore_symbolic_links context =
  let project_root = bracket_tmpdir context |> Path.create_absolute in
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let path name = Path.create_relative ~root:project_root ~relative:name in
  let create_file name = path name |> File.create ~content:"" |> File.write in
  let link name =
    let actual = Path.absolute (path name) in
    let link = Path.create_relative ~root:local_root ~relative:name |> Path.absolute in
    Unix.symlink ~src:actual ~dst:link
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
          [ path "a.py", Path.create_relative ~root:local_root ~relative:"a.py";
            path "b.py", Path.create_relative ~root:local_root ~relative:"unused_link.py";
            path "removed.py", Path.create_relative ~root:local_root ~relative:"removed.py" ]
      in
      Map.find map
    in
    assert_equal
      ~printer:(List.to_string ~f:Path.show)
      ~cmp:(List.equal ~equal:Path.equal)
      expected
      (Server.SavedState.restore_symbolic_links ~changed_paths:names ~local_root ~get_old_link_path)
  in
  (* For changed files, the new set of links is prioritized. *)
  assert_restored
    ~names:[path "a.py"; path "b.py"]
    ~expected:
      [ Path.create_relative ~root:local_root ~relative:"a.py";
        Path.create_relative ~root:local_root ~relative:"b.py" ];
  (* We only get paths that are passed in. *)
  assert_restored
    ~names:[path "a.py"]
    ~expected:[Path.create_relative ~root:local_root ~relative:"a.py"];
  (* We relativize files not present in the new set of files. *)
  assert_restored
    ~names:[path "a.py"; path "removed.py"]
    ~expected:
      [ Path.create_relative ~root:local_root ~relative:"a.py";
        Path.create_relative ~root:local_root ~relative:"removed.py" ];
  assert_restored
    ~names:[Path.create_relative ~root:local_root ~relative:"unlinked.py"]
    ~expected:[Path.create_relative ~root:local_root ~relative:"unlinked.py"]


type locally_changed_file = {
  relative: string;
  old_content: string option;
  (* Won't be written if content is None. *)
  new_content: string option
}

let test_compute_locally_changed_files context =
  let assert_changed_files ~files ~expected =
    (* Set up the HandleKeys as the parser would. *)
    Ast.SharedMemory.HandleKeys.clear ();
    let root = Path.create_absolute (bracket_tmpdir context) in
    let add_file { relative; old_content; new_content } =
      (* Register old content in shared memory. *)
      ( match old_content with
      | Some content ->
          let handle = File.Handle.create relative in
          Test.parse ~handle:relative content |> Ast.SharedMemory.Sources.add handle;
          Ast.SharedMemory.HandleKeys.add ~handles:(File.Handle.Set.Tree.singleton handle)
      | None -> () );
      (* Write new content to the file system if necessary. *)
      match new_content with
      | Some content -> File.write (File.create ~content (Path.create_relative ~root ~relative))
      | None -> ()
    in
    List.iter files ~f:add_file;
    let actual =
      Server.SavedState.compute_locally_changed_files
        ~scheduler:(Scheduler.mock ())
        ~configuration:(Configuration.Analysis.create ~local_root:root ())
      |> List.map ~f:File.path
      |> List.filter_map ~f:(fun path -> Path.get_relative_to_root ~root ~path)
    in
    (* Ensure sources are cleaned up afterwards. *)
    List.map files ~f:(fun { relative; _ } -> File.Handle.create relative)
    |> fun handles ->
    Ast.SharedMemory.Sources.remove ~handles;
    assert_equal
      ~printer:(List.to_string ~f:ident)
      (List.sort ~compare:String.compare expected)
      (List.sort ~compare:String.compare actual)
  in
  assert_changed_files
    ~files:[{ relative = "a.py"; old_content = Some "a = 1"; new_content = Some "a = 2" }]
    ~expected:["a.py"];
  assert_changed_files
    ~files:[{ relative = "a.py"; old_content = None; new_content = Some "a = 2" }]
    ~expected:["a.py"];
  assert_changed_files
    ~files:[{ relative = "a.py"; old_content = Some "a = 2"; new_content = Some "a = 2" }]
    ~expected:[];
  assert_changed_files
    ~files:[{ relative = "a.py"; old_content = Some "'I used to exist'"; new_content = None }]
    ~expected:["a.py"];
  (* If a stub shadows a `.py` file that existed in the initial saved state generation, the server
     will be passed both files (as `a.py` used to exist, but was now removed in the eyes of the
     algorithm). *)
  assert_changed_files
    ~files:
      [ { relative = "a.py"; old_content = Some "a = 1"; new_content = Some "new" };
        { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 2" } ]
    ~expected:["a.py"];
  assert_changed_files
    ~files:
      [ { relative = "a.py"; old_content = Some "a = 1"; new_content = Some "new" };
        { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 3" } ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [ { relative = "a.py"; old_content = Some "a = 1"; new_content = None };
        { relative = "a.pyi"; old_content = None; new_content = Some "a = 2" } ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [ { relative = "a.py"; old_content = None; new_content = Some "a = 1" };
        { relative = "a.pyi"; old_content = Some "a: int"; new_content = None } ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [ { relative = "b.py"; old_content = Some "a = 1"; new_content = Some "new" };
        { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 2" } ]
    ~expected:["b.py"]


let () =
  "saved_state"
  >::: [ "restore_symbolic_links" >:: test_restore_symbolic_links;
         "compute_locally_changed_files" >:: test_compute_locally_changed_files ]
  |> Test.run
