(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Pyre
open Test

type locally_changed_file = {
  relative: string;
  old_content: string option;
  (* Won't be written if content is None. *)
  new_content: string option;
}

let test_compute_locally_changed_files context =
  let assert_changed_files ~files ~expected =
    let scheduler = Test.mock_scheduler () in
    let configuration, old_module_tracker =
      let sources =
        List.filter_map files ~f:(fun { relative; old_content; _ } ->
            old_content >>| fun content -> relative, content)
      in
      let project = ScratchProject.setup ~context ~in_memory:false sources in
      let configuration = ScratchProject.configuration_of project in
      let module_tracker = ScratchProject.ReadWrite.module_tracker project in
      let () =
        Interprocedural.ChangedPaths.save_current_paths ~scheduler ~configuration ~module_tracker
      in
      configuration, module_tracker
    in
    let { Configuration.Analysis.local_root; _ } = configuration in
    let write_new_file { relative; new_content; _ } =
      (* Write new content to the file system if necessary. *)
      let path = PyrePath.create_relative ~root:local_root ~relative in
      match new_content with
      | Some content -> File.write (File.create ~content path)
      | None -> Sys.remove (PyrePath.absolute path)
    in
    List.iter files ~f:write_new_file;
    let new_module_tracker =
      Analysis.EnvironmentControls.create configuration |> Analysis.ModuleTracker.create
    in
    let actual =
      Interprocedural.ChangedPaths.compute_locally_changed_paths
        ~scheduler
        ~configuration
        ~old_module_tracker
        ~new_module_tracker
      |> List.filter_map ~f:(fun path ->
             PyrePath.get_relative_to_root ~root:local_root ~path:(ArtifactPath.raw path))
    in
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
      [
        { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 2" };
        { relative = "a.py"; old_content = Some "a = 1"; new_content = Some "new" };
      ]
    ~expected:[];
  assert_changed_files
    ~files:
      [
        { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 3" };
        { relative = "a.py"; old_content = Some "a = 1"; new_content = Some "new" };
      ]
    ~expected:["a.pyi"];
  assert_changed_files
    ~files:
      [
        { relative = "a.pyi"; old_content = None; new_content = Some "a = 2" };
        { relative = "a.py"; old_content = Some "a = 1"; new_content = None };
      ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [
        { relative = "a.pyi"; old_content = Some "a: int"; new_content = None };
        { relative = "a.py"; old_content = None; new_content = Some "a = 1" };
      ]
    ~expected:["a.py"; "a.pyi"];
  assert_changed_files
    ~files:
      [
        { relative = "a.pyi"; old_content = Some "a = 2"; new_content = Some "a = 2" };
        { relative = "b.py"; old_content = Some "a = 1"; new_content = Some "new" };
      ]
    ~expected:["b.py"];
  (* Test with a parsing error. *)
  assert_changed_files
    ~files:[{ relative = "a.py"; old_content = Some "+"; new_content = Some "+" }]
    ~expected:[];
  ()


let () =
  "saved_state"
  >::: ["compute_locally_changed_files" >:: test_compute_locally_changed_files]
  |> Test.run
