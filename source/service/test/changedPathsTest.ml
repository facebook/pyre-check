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
      let ast_environment, _ = ScratchProject.parse_sources project in
      project, ast_environment
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
    let actual =
      Service.ChangedPaths.compute_locally_changed_paths
        ~scheduler:(Test.mock_scheduler ())
        ~configuration
        ~module_tracker
        ~ast_environment:(Analysis.AstEnvironment.read_only ast_environment)
      |> List.filter_map ~f:(fun path -> PyrePath.get_relative_to_root ~root:local_root ~path)
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
    ~expected:["b.py"]


let () =
  "saved_state"
  >::: ["compute_locally_changed_files" >:: test_compute_locally_changed_files]
  |> Test.run
