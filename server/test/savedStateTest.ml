(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre
open Test

let test_restore_symbolic_links context =
  let project_root = bracket_tmpdir context |> Path.create_absolute in
  let local_root = bracket_tmpdir context |> Path.create_absolute in
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
          [ path "a.py", Path.create_relative ~root:local_root ~relative:"a.py";
            path "b.py", Path.create_relative ~root:local_root ~relative:"unused_link.py";
            path "removed.py", Path.create_relative ~root:local_root ~relative:"removed.py" ]
      in
      Map.find map
    in
    assert_equal
      ~printer:(List.to_string ~f:Path.show)
      ~cmp:(List.equal Path.equal)
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
  new_content: string option;
}

let test_compute_locally_changed_files context =
  let assert_changed_files ~files ~expected =
    let { ScratchProject.configuration; module_tracker; _ }, ast_environment =
      let sources =
        List.filter_map files ~f:(fun { relative; old_content; _ } ->
            old_content >>| fun content -> relative, content)
      in
      let project = ScratchProject.setup ~context sources in
      let _, ast_environment = ScratchProject.parse_sources project in
      project, ast_environment
    in
    let { Configuration.Analysis.local_root; _ } = configuration in
    let write_new_file { relative; new_content; _ } =
      (* Write new content to the file system if necessary. *)
      let path = Path.create_relative ~root:local_root ~relative in
      match new_content with
      | Some content -> File.write (File.create ~content path)
      | None -> Sys.remove (Path.absolute path)
    in
    List.iter files ~f:write_new_file;
    let actual =
      Server.SavedState.compute_locally_changed_paths
        ~scheduler:(Scheduler.mock ())
        ~configuration
        ~module_tracker
        ~ast_environment:(Analysis.AstEnvironment.read_only ast_environment)
      |> List.filter_map ~f:(fun path -> Path.get_relative_to_root ~root:local_root ~path)
    in
    (* Ensure sources are cleaned up afterwards. *)
    List.map files ~f:(fun { relative; _ } -> Ast.SourcePath.qualifier_of_relative relative)
    |> Analysis.AstEnvironment.remove_sources ast_environment;
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

  assert_changed_files
    ~files:
      [ { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 2" };
        { relative = "a.py"; old_content = Some "a = 1"; new_content = Some "new" } ]
    ~expected:[];
  assert_changed_files
    ~files:
      [ { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 3" };
        { relative = "a.py"; old_content = Some "a = 1"; new_content = Some "new" } ]
    ~expected:["a.pyi"];
  assert_changed_files
    ~files:
      [ { relative = "a.pyi"; old_content = None; new_content = Some "a = 2" };
        { relative = "a.py"; old_content = Some "a = 1"; new_content = None } ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [ { relative = "a.pyi"; old_content = Some "a: int"; new_content = None };
        { relative = "a.py"; old_content = None; new_content = Some "a = 1" } ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [ { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 2" };
        { relative = "b.py"; old_content = Some "a = 1"; new_content = Some "new" } ]
    ~expected:["b.py"]


let () =
  "saved_state"
  >::: [ "restore_symbolic_links" >:: test_restore_symbolic_links;
         "compute_locally_changed_files" >:: test_compute_locally_changed_files ]
  |> Test.run
