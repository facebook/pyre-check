(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server
open ServerTest

let create_relative_source_path ~root ~relative =
  PyrePath.create_relative ~root ~relative |> SourcePath.create


let test_initialize context =
  let internal_state = ref "uninitiailzed" in
  let build_system_initializer =
    let initialize () =
      internal_state := "initialized";
      Lwt.return (BuildSystem.create_for_testing ())
    in
    let load () = failwith "saved state loading is not supported" in
    let cleanup () = Lwt.return_unit in
    BuildSystem.Initializer.create_for_testing ~initialize ~load ~cleanup ()
  in
  let test_initialize _ =
    (* Verify that the build system has indeed been initiailzed. *)
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id "initialized" !internal_state;
    Lwt.return_unit
  in
  ScratchProject.setup
    ~context
    ~include_typeshed_stubs:false
    ~include_helper_builtins:false
    ~build_system_initializer
    []
  |> ScratchProject.test_server_with ~f:test_initialize


let test_cleanup context =
  let internal_state = ref "uncleaned" in
  let build_system_initializer =
    let initialize () = Lwt.return (BuildSystem.create_for_testing ()) in
    let load () = failwith "saved state loading is not supported" in
    let cleanup () =
      internal_state := "cleaned";
      Lwt.return_unit
    in
    BuildSystem.Initializer.create_for_testing ~initialize ~load ~cleanup ()
  in
  let open Lwt.Infix in
  let configuration, start_options =
    let project =
      ScratchProject.setup ~context ~include_typeshed_stubs:false ~include_helper_builtins:false []
    in
    ScratchProject.configuration_of project, ScratchProject.start_options_of project
  in

  Start.start_server
    start_options
    ~configuration
    ~build_system_initializer
    ~on_exception:(fun exn -> raise exn)
    ~on_started:(fun _ _ ->
      (* Shutdown the server immediately after it is started. *)
      Lwt.return Start.ExitStatus.Ok)
  >>= fun _ ->
  (* Verify that the build system has indeed been cleaned up. *)
  assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id "cleaned" !internal_state;
  Lwt.return_unit


let test_type_errors context =
  let test_source_path = PyrePath.create_absolute "/foo/test.py" |> SourcePath.create in
  let test_artifact_path =
    (* The real value will be deterimend once the server starts. *)
    ref (PyrePath.create_absolute "uninitialized" |> ArtifactPath.create)
  in
  let build_system_initializer =
    let initialize () =
      let lookup_source path =
        if ArtifactPath.equal path !test_artifact_path then
          Some test_source_path
        else
          None
      in
      let lookup_artifact path =
        if SourcePath.equal path test_source_path then
          [!test_artifact_path]
        else
          []
      in
      Lwt.return (BuildSystem.create_for_testing ~lookup_source ~lookup_artifact ())
    in
    let load () = failwith "saved state loading is not supported" in
    let cleanup () = Lwt.return_unit in
    BuildSystem.Initializer.create_for_testing ~initialize ~load ~cleanup ()
  in
  let test_type_errors client =
    let open Lwt.Infix in
    let global_root =
      Client.get_server_properties client
      |> fun { ServerProperties.configuration = { Configuration.Analysis.project_root; _ }; _ } ->
      project_root
    in
    test_artifact_path := Test.relative_artifact_path ~root:global_root ~relative:"test.py";
    let test_error =
      Analysis.AnalysisError.Instantiated.of_yojson
        (`Assoc
          [
            "line", `Int 1;
            "column", `Int 0;
            "stop_line", `Int 1;
            "stop_column", `Int 11;
            "path", `String "/foo/test.py";
            "code", `Int (-1);
            "name", `String "Revealed type";
            ( "description",
              `String
                "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`." );
            ( "long_description",
              `String
                "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`." );
            ( "concise_description",
              `String
                "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`." );
            "define", `String "test.$toplevel";
          ])
      |> Result.ok_or_failwith
    in
    let test2_error =
      (* `test2.py` is intentionally not tracked by the build system. The expected behavior here is
         to show its original path. *)
      let test2_artifact_path =
        create_relative_source_path ~root:global_root ~relative:"test2.py"
      in
      Analysis.AnalysisError.Instantiated.of_yojson
        (`Assoc
          [
            "line", `Int 1;
            "column", `Int 0;
            "stop_line", `Int 1;
            "stop_column", `Int 11;
            "path", `String (SourcePath.raw test2_artifact_path |> PyrePath.absolute);
            "code", `Int (-1);
            "name", `String "Revealed type";
            ( "description",
              `String
                "Revealed type [-1]: Revealed type for `43` is `typing_extensions.Literal[43]`." );
            ( "long_description",
              `String
                "Revealed type [-1]: Revealed type for `43` is `typing_extensions.Literal[43]`." );
            ( "concise_description",
              `String
                "Revealed type [-1]: Revealed type for `43` is `typing_extensions.Literal[43]`." );
            "define", `String "test2.$toplevel";
          ])
      |> Result.ok_or_failwith
    in
    Client.assert_response
      client
      ~request:(Request.DisplayTypeError [])
      ~expected:(Response.TypeErrors [test_error; test2_error])
    >>= fun () ->
    Client.assert_response
      client
      ~request:(Request.DisplayTypeError ["/foo/test.py"])
      ~expected:(Response.TypeErrors [test_error])
  in
  ScratchProject.setup
    ~context
    ~include_typeshed_stubs:false
    ~include_helper_builtins:false
    ~build_system_initializer
    ["test.py", "reveal_type(42)"; "test2.py", "reveal_type(43)"]
  |> ScratchProject.test_server_with ~f:test_type_errors


let test_update context =
  let internal_state = ref "unupdated" in
  let test_source_path = PyrePath.create_absolute "/foo/test.py" |> SourcePath.create in
  let test_artifact_path =
    (* The real value will be deterimend once the server starts. *)
    ref (PyrePath.create_absolute "uninitialized" |> ArtifactPath.create)
  in
  let build_system_initializer =
    let initialize () =
      let lookup_source path =
        if ArtifactPath.equal path !test_artifact_path then
          Some test_source_path
        else
          None
      in
      let lookup_artifact path =
        if SourcePath.equal path test_source_path then
          [!test_artifact_path]
        else
          []
      in
      let update actual_paths =
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: SourcePath.t list]
          ~printer:(fun paths -> List.map paths ~f:SourcePath.show |> String.concat ~sep:", ")
          [test_source_path]
          actual_paths;
        internal_state := "updated";
        Lwt.return []
      in
      Lwt.return (BuildSystem.create_for_testing ~update ~lookup_source ~lookup_artifact ())
    in
    let load () = failwith "saved state loading is not supported" in
    let cleanup () = Lwt.return_unit in
    BuildSystem.Initializer.create_for_testing ~initialize ~load ~cleanup ()
  in
  let test_update client =
    let open Lwt.Infix in
    let root =
      Client.get_server_properties client
      |> fun { ServerProperties.configuration = { Configuration.Analysis.project_root; _ }; _ } ->
      project_root
    in
    test_artifact_path := Test.relative_artifact_path ~root ~relative:"test.py";

    ArtifactPath.raw !test_artifact_path |> File.create ~content:"reveal_type(42)" |> File.write;
    Client.send_request
      client
      (Request.IncrementalUpdate [SourcePath.raw test_source_path |> PyrePath.absolute])
    >>= fun _ ->
    (* Verify that the build system has indeed been updated. *)
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id "updated" !internal_state;
    (* Verify that recheck has indeed happened. *)
    let expected_error =
      Analysis.AnalysisError.Instantiated.of_yojson
        (`Assoc
          [
            "line", `Int 1;
            "column", `Int 0;
            "stop_line", `Int 1;
            "stop_column", `Int 11;
            "path", `String "/foo/test.py";
            "code", `Int (-1);
            "name", `String "Revealed type";
            ( "description",
              `String
                "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`." );
            ( "long_description",
              `String
                "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`." );
            ( "concise_description",
              `String
                "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`." );
            "define", `String "test.$toplevel";
          ])
      |> Result.ok_or_failwith
    in
    Client.assert_response
      client
      ~request:(Request.DisplayTypeError [])
      ~expected:(Response.TypeErrors [expected_error])
    >>= fun () -> Lwt.return_unit
  in
  ScratchProject.setup
    ~context
    ~include_typeshed_stubs:false
    ~include_helper_builtins:false
    ~build_system_initializer
    ["test.py", "reveal_type(True)"]
  |> ScratchProject.test_server_with ~f:test_update


let test_buck_renormalize context =
  (* Count how many times target renormalization has happened. *)
  let normalize_counter = ref 0 in
  let assert_normalize_counter expected =
    assert_equal ~ctxt:context ~cmp:Int.equal ~printer:Int.to_string expected !normalize_counter
  in

  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let foo_source = create_relative_source_path ~root:source_root ~relative:"foo.py" in
  File.create (SourcePath.raw foo_source) ~content:"" |> File.write;
  let get_buck_build_system () =
    let interface =
      let normalize_targets targets =
        incr normalize_counter;
        Lwt.return (List.map targets ~f:Buck.Target.of_string)
      in
      let construct_build_map targets =
        Lwt.return
          {
            Buck.Interface.BuildResult.targets;
            build_map = Buck.(BuildMap.(create (Partial.of_alist_exn ["foo.py", "foo.py"])));
          }
      in
      Buck.Interface.create_for_testing ~normalize_targets ~construct_build_map ()
    in
    let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in
    let builder = Buck.Builder.create ~source_root ~artifact_root interface in
    BuildSystem.Initializer.buck ~builder ~artifact_root ~targets:["//foo:target"] ()
    |> BuildSystem.Initializer.run
  in
  let open Lwt.Infix in
  get_buck_build_system ()
  >>= fun buck_build_system ->
  (* Normalization will happen once upon initialization. *)
  assert_normalize_counter 1;

  (* Normalization won't happen if no target file changes. *)
  BuildSystem.update buck_build_system []
  >>= fun _ ->
  assert_normalize_counter 1;
  BuildSystem.update buck_build_system [foo_source]
  >>= fun _ ->
  assert_normalize_counter 1;

  (* Normalization will happen if target file has changes. *)
  BuildSystem.update
    buck_build_system
    [create_relative_source_path ~root:source_root ~relative:"bar/TARGETS"]
  >>= fun _ ->
  assert_normalize_counter 2;
  BuildSystem.update
    buck_build_system
    [create_relative_source_path ~root:source_root ~relative:"BUCK"]
  >>= fun _ ->
  assert_normalize_counter 3;
  Lwt.return_unit


let test_buck_update context =
  let assert_optional_path ~expected actual =
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: SourcePath.t option]
      ~printer:(Option.value_map ~default:"NONE" ~f:SourcePath.show)
      expected
      actual
  in
  let assert_optional_analysis_path ~expected actual =
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: ArtifactPath.t option]
      ~printer:(Option.value_map ~default:"NONE" ~f:ArtifactPath.show)
      expected
      actual
  in
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in

  let get_buck_build_system () =
    let interface =
      (* Here's the set up: we have 2 files, `foo/bar.py` and `foo/baz.py`. If `is_rebuild` is
         false, we'll only include `foo/bar.py` in the target. If `is_rebuild` is true, we'll
         include both files. The `is_rebuild` flag is initially false but will be set to true after
         the first build. This setup emulates an incremental Buck update where the user edits the
         TARGET file to include another source in the target. *)
      let is_rebuild = ref false in
      let normalize_targets targets = Lwt.return (List.map targets ~f:Buck.Target.of_string) in
      let construct_build_map targets =
        let build_mappings =
          if !is_rebuild then
            ["bar.py", "foo/bar.py"; "baz.py", "foo/baz.py"]
          else (
            is_rebuild := true;
            ["bar.py", "foo/bar.py"])
        in
        Lwt.return
          {
            Buck.Interface.BuildResult.targets;
            build_map = Buck.(BuildMap.(create (Partial.of_alist_exn build_mappings)));
          }
      in
      Buck.Interface.create_for_testing ~normalize_targets ~construct_build_map ()
    in
    let builder = Buck.Builder.create ~source_root ~artifact_root interface in
    BuildSystem.Initializer.buck ~builder ~artifact_root ~targets:["//foo:target"] ()
    |> BuildSystem.Initializer.run
  in
  let open Lwt.Infix in
  get_buck_build_system ()
  >>= fun buck_build_system ->
  let bar_source = create_relative_source_path ~root:source_root ~relative:"foo/bar.py" in
  let bar_artifact = Test.relative_artifact_path ~root:artifact_root ~relative:"bar.py" in
  let baz_source = create_relative_source_path ~root:source_root ~relative:"foo/baz.py" in
  let baz_artifact = Test.relative_artifact_path ~root:artifact_root ~relative:"baz.py" in

  (* Initially, we build bar.py but not baz.py. *)
  assert_optional_path
    ~expected:(Some bar_source)
    (BuildSystem.lookup_source buck_build_system bar_artifact);
  assert_optional_path ~expected:None (BuildSystem.lookup_source buck_build_system baz_artifact);
  assert_optional_analysis_path
    ~expected:(Some bar_artifact)
    (BuildSystem.lookup_artifact buck_build_system bar_source |> List.hd);
  assert_optional_analysis_path
    ~expected:None
    (BuildSystem.lookup_artifact buck_build_system baz_source |> List.hd);

  (* Rebuild the project. The fake TARGET file is needed to force a full rebuild. *)
  let fake_target_file = create_relative_source_path ~root:source_root ~relative:"TARGETS" in
  BuildSystem.update buck_build_system [bar_source; baz_source; fake_target_file]
  >>= fun _ ->
  (* After the rebuild, both bar.py and baz.py should be included in build map. *)
  assert_optional_path
    ~expected:(Some bar_source)
    (BuildSystem.lookup_source buck_build_system bar_artifact);
  assert_optional_path
    ~expected:(Some baz_source)
    (BuildSystem.lookup_source buck_build_system baz_artifact);
  assert_optional_analysis_path
    ~expected:(Some bar_artifact)
    (BuildSystem.lookup_artifact buck_build_system bar_source |> List.hd);
  assert_optional_analysis_path
    ~expected:(Some baz_artifact)
    (BuildSystem.lookup_artifact buck_build_system baz_source |> List.hd);

  Lwt.return_unit


let assert_paths_no_order ~context ~expected actual =
  let compare = [%compare: ArtifactPath.t] in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: ArtifactPath.t list]
    ~printer:(fun paths -> List.map paths ~f:ArtifactPath.show |> String.concat ~sep:" ")
    (List.sort ~compare expected)
    (List.sort ~compare actual)


let test_buck_update_without_rebuild context =
  let assert_paths_no_order = assert_paths_no_order ~context in
  let source_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let artifact_root = bracket_tmpdir context |> PyrePath.create_absolute in

  let get_buck_build_system () =
    let interface =
      let is_rebuild = ref false in
      let normalize_targets targets = Lwt.return (List.map targets ~f:Buck.Target.of_string) in
      let construct_build_map targets =
        let build_mappings =
          if not !is_rebuild then (
            is_rebuild := true;
            ["bar.py", "foo/bar.py"; "baz.py", "foo/baz.py"])
          else
            assert_failure
              "Build map construction is not expected to be invoked again after the initial build"
        in
        Lwt.return
          {
            Buck.Interface.BuildResult.targets;
            build_map = Buck.(BuildMap.(create (Partial.of_alist_exn build_mappings)));
          }
      in
      Buck.Interface.create_for_testing ~normalize_targets ~construct_build_map ()
    in
    let builder = Buck.Builder.create ~source_root ~artifact_root interface in
    BuildSystem.Initializer.buck ~builder ~artifact_root ~targets:["//foo:target"] ()
    |> BuildSystem.Initializer.run
  in
  let open Lwt.Infix in
  get_buck_build_system ()
  >>= fun buck_build_system ->
  let bar_source = create_relative_source_path ~root:source_root ~relative:"foo/bar.py" in
  let baz_source = create_relative_source_path ~root:source_root ~relative:"foo/baz.py" in
  File.create (SourcePath.raw bar_source) ~content:"" |> File.write;
  File.create (SourcePath.raw baz_source) ~content:"" |> File.write;
  BuildSystem.update buck_build_system [bar_source; baz_source]
  >>= fun changed_artifacts ->
  (* After the rebuild, both bar.py and baz.py should be included in build map. *)
  let bar_artifact = Test.relative_artifact_path ~root:artifact_root ~relative:"bar.py" in
  let baz_artifact = Test.relative_artifact_path ~root:artifact_root ~relative:"baz.py" in
  assert_paths_no_order changed_artifacts ~expected:[bar_artifact; baz_artifact];
  Lwt.return_unit


let test_unwatched_dependency_no_failure_on_initialize context =
  let bucket_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let bucket = "BUCKET" in
  let wheel_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let checksum_path = "CHECKSUM" in
  let open Lwt.Infix in
  let test_initializer =
    BuildSystem.Initializer.track_unwatched_dependency
      {
        Configuration.UnwatchedDependency.change_indicator =
          { Configuration.ChangeIndicator.root = bucket_root; relative = bucket };
        files = { Configuration.UnwatchedFiles.root = wheel_root; checksum_path };
      }
  in
  BuildSystem.Initializer.run test_initializer
  >>= fun build_system ->
  (* Initialization should not crash, even when the checksum path does not exist *)
  let bucket_path = create_relative_source_path ~root:bucket_root ~relative:bucket in
  Lwt.catch
    (fun () ->
      (* Update should crash, if the checksum path does not exist *)
      BuildSystem.update build_system [bucket_path]
      >>= fun _ -> assert_failure "should not reach here")
    (function
      | ChecksumMap.LoadError _ -> Lwt.return_unit
      | _ -> assert_failure "wrong exception raised")


let test_unwatched_dependency_update context =
  let assert_paths_no_order = assert_paths_no_order ~context in
  let bucket_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let bucket = "BUCKET" in
  let bucket_path = create_relative_source_path ~root:bucket_root ~relative:bucket in
  let wheel_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let checksum_path = "CHECKSUM" in
  let checksum_full_path = PyrePath.create_relative ~root:wheel_root ~relative:checksum_path in
  let content =
    {|
      {
        "a.py": "checksum0",
        "b/c.py": "checksum1",
        "d/e/f.pyi": "checksum2"
      }
    |}
  in
  File.create checksum_full_path ~content |> File.write;
  let open Lwt.Infix in
  let instagram_initializer =
    BuildSystem.Initializer.track_unwatched_dependency
      {
        Configuration.UnwatchedDependency.change_indicator =
          { Configuration.ChangeIndicator.root = bucket_root; relative = bucket };
        files = { Configuration.UnwatchedFiles.root = wheel_root; checksum_path };
      }
  in
  BuildSystem.Initializer.run instagram_initializer
  >>= fun build_system ->
  let test_path = PyrePath.create_absolute "/some/source/file.py" |> SourcePath.create in
  (* Normal update does not yield additional changed paths. *)
  BuildSystem.update build_system [test_path]
  >>= fun updated ->
  assert_paths_no_order updated ~expected:[];

  (* Touching the change indicator but not the checksum file does not yield additional changed
     paths. *)
  BuildSystem.update build_system [bucket_path]
  >>= fun updated ->
  assert_paths_no_order updated ~expected:[];

  (* Checksum file update will lead to additional changed paths. *)
  let content =
    {|
      {
        "a.py": "checksum3",
        "d/e/f.pyi": "checksum2",
        "g.py": "checksum4"
      }
    |}
  in
  File.create checksum_full_path ~content |> File.write;
  BuildSystem.update build_system [test_path; bucket_path]
  >>= fun updated ->
  assert_paths_no_order
    updated
    ~expected:
      [
        Test.relative_artifact_path ~root:wheel_root ~relative:"a.py";
        Test.relative_artifact_path ~root:wheel_root ~relative:"b/c.py";
        Test.relative_artifact_path ~root:wheel_root ~relative:"g.py";
      ];

  Lwt.return_unit


let () =
  "build_system_test"
  >::: [
         "initialize" >:: OUnitLwt.lwt_wrapper test_initialize;
         "cleanup" >:: OUnitLwt.lwt_wrapper test_cleanup;
         "type_errors" >:: OUnitLwt.lwt_wrapper test_type_errors;
         "update" >:: OUnitLwt.lwt_wrapper test_update;
         "buck_renormalize" >:: OUnitLwt.lwt_wrapper test_buck_renormalize;
         "buck_update" >:: OUnitLwt.lwt_wrapper test_buck_update;
         "buck_update_without_rebuild" >:: OUnitLwt.lwt_wrapper test_buck_update_without_rebuild;
         "unwatched_dependency_no_failure_on_initialize"
         >:: OUnitLwt.lwt_wrapper test_unwatched_dependency_no_failure_on_initialize;
         "unwatched_dependency_update" >:: OUnitLwt.lwt_wrapper test_unwatched_dependency_update;
       ]
  |> Test.run
