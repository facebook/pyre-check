(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
module BuildSystem = CodeNavigationServer.Testing.BuildSystem

let assert_artifact_events_equal ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: ArtifactPath.Event.t list]
    ~printer:(fun event -> Sexp.to_string_hum ([%sexp_of: ArtifactPath.Event.t list] event))
    (List.sort ~compare:ArtifactPath.Event.compare expected)
    (List.sort ~compare:ArtifactPath.Event.compare actual)


let create_buck_build_system_for_testing ~source_root ~artifact_root ~construct_build_map () =
  Buck.Interface.Lazy.create_for_testing ~construct_build_map ()
  |> Buck.Builder.Lazy.create ~source_root ~artifact_root
  |> BuildSystem.Initializer.buck ~artifact_root
  |> BuildSystem.Initializer.initialize


let test_buck_update_working_set context =
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let raw_source_path0 = PyrePath.create_relative ~root:source_root ~relative:"source0.py" in
  let raw_source_path1 = PyrePath.create_relative ~root:source_root ~relative:"source1.py" in
  File.create raw_source_path0 ~content:"" |> File.write;
  File.create raw_source_path1 ~content:"" |> File.write;
  let source_path0 = SourcePath.create raw_source_path0 in
  let source_path1 = SourcePath.create raw_source_path1 in
  let artifact_path0 =
    PyrePath.create_relative ~root:artifact_root ~relative:"artifact0.py" |> ArtifactPath.create
  in
  let artifact_path1 =
    PyrePath.create_relative ~root:artifact_root ~relative:"artifact1.py" |> ArtifactPath.create
  in

  let build_system =
    let construct_build_map working_set =
      let mappings = [] in
      let mappings =
        if List.exists working_set ~f:(String.equal "source0.py") then
          ("artifact0.py", "source0.py") :: mappings
        else
          mappings
      in
      let mappings =
        if List.exists working_set ~f:(String.equal "source1.py") then
          ("artifact1.py", "source1.py") :: mappings
        else
          mappings
      in
      Buck.BuildMap.(Partial.of_alist_exn mappings |> create) |> Lwt.return
    in
    create_buck_build_system_for_testing ~source_root ~artifact_root ~construct_build_map ()
  in

  let%lwt result = BuildSystem.update_working_set build_system [source_path0] in
  assert_artifact_events_equal
    ~context
    ~expected:[ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged artifact_path0)]
    result;

  let%lwt result = BuildSystem.update_working_set build_system [source_path0; source_path1] in
  assert_artifact_events_equal
    ~context
    ~expected:[ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged artifact_path1)]
    result;

  let%lwt result = BuildSystem.update_working_set build_system [] in
  assert_artifact_events_equal
    ~context
    ~expected:
      [
        ArtifactPath.Event.(create ~kind:Kind.Deleted artifact_path0);
        ArtifactPath.Event.(create ~kind:Kind.Deleted artifact_path1);
      ]
    result;
  Lwt.return_unit


let test_buck_update_sources context =
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let raw_source_path0 = PyrePath.create_relative ~root:source_root ~relative:"source0.py" in
  let raw_source_path1 = PyrePath.create_relative ~root:source_root ~relative:"source1.py" in
  File.create raw_source_path0 ~content:"" |> File.write;
  File.create raw_source_path1 ~content:"" |> File.write;
  let source_path0 = SourcePath.create raw_source_path0 in
  let source_path1 = SourcePath.create raw_source_path1 in

  let update_build_map_flag = ref false in
  let build_system =
    let construct_build_map _ =
      (* Note how the build map does not vary with working set. This means working set values do not
         matter in this test. *)
      if not !update_build_map_flag then
        Buck.BuildMap.(Partial.of_alist_exn ["artifact0.py", "source0.py"] |> create) |> Lwt.return
      else
        Buck.BuildMap.(Partial.of_alist_exn ["artifact1.py", "source1.py"] |> create) |> Lwt.return
    in
    create_buck_build_system_for_testing ~source_root ~artifact_root ~construct_build_map ()
  in
  let%lwt _ =
    (* Force the lazy builder to populate the initial build map. *)
    BuildSystem.update_working_set build_system []
  in

  let%lwt result =
    BuildSystem.update_sources
      build_system
      ~working_set:[]
      [SourcePath.Event.(create ~kind:Kind.CreatedOrChanged source_path0)]
  in
  assert_artifact_events_equal ~context ~expected:[] result;

  let%lwt result =
    BuildSystem.update_sources
      build_system
      ~working_set:[]
      [SourcePath.Event.(create ~kind:Kind.CreatedOrChanged source_path1)]
  in
  assert_artifact_events_equal ~context ~expected:[] result;

  let%lwt result =
    BuildSystem.update_sources
      build_system
      ~working_set:[]
      [
        SourcePath.Event.(create ~kind:Kind.CreatedOrChanged source_path0);
        SourcePath.Event.(create ~kind:Kind.CreatedOrChanged source_path1);
      ]
  in
  assert_artifact_events_equal ~context ~expected:[] result;

  let source_path2 =
    PyrePath.create_relative ~root:source_root ~relative:"BUCK" |> SourcePath.create
  in
  let%lwt result =
    BuildSystem.update_sources
      build_system
      ~working_set:[]
      [SourcePath.Event.(create ~kind:Kind.CreatedOrChanged source_path2)]
  in
  assert_artifact_events_equal ~context ~expected:[] result;

  update_build_map_flag := true;
  PyrePath.remove raw_source_path0;
  let artifact_path0 =
    PyrePath.create_relative ~root:artifact_root ~relative:"artifact0.py" |> ArtifactPath.create
  in
  let artifact_path1 =
    PyrePath.create_relative ~root:artifact_root ~relative:"artifact1.py" |> ArtifactPath.create
  in
  let%lwt result =
    BuildSystem.update_sources
      build_system
      ~working_set:[]
      [SourcePath.Event.(create ~kind:Kind.Deleted source_path0)]
  in
  assert_artifact_events_equal
    ~context
    ~expected:
      [
        ArtifactPath.Event.(create ~kind:Kind.Deleted artifact_path0);
        ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged artifact_path1);
      ]
    result;
  Lwt.return_unit


let () =
  "build_system_test"
  >::: [
         "test_buck_updaet_working_set" >:: OUnitLwt.lwt_wrapper test_buck_update_working_set;
         "test_buck_update_sources" >:: OUnitLwt.lwt_wrapper test_buck_update_sources;
       ]
  |> Test.run
