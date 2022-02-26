(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Lwt.Infix
open Server

let test_configuration_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match SavedStateAction.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: SavedStateAction.t]
          ~printer:(fun value -> Sexp.to_string_hum ([%sexp_of: SavedStateAction.t] value))
          expected
          actual
  in
  let assert_not_parsed json_string =
    let json = Yojson.Safe.from_string json_string in
    match SavedStateAction.of_yojson json with
    | Result.Ok _ -> assert_failure "Unexpected JSON parsing success"
    | Result.Error _ -> ()
  in

  assert_not_parsed {|"derp"|};
  assert_not_parsed "42";
  assert_not_parsed "[]";

  assert_parsed
    {|
      [
        "load_from_file",
        {
          "shared_memory_path": "/some/path"
        }
      ]
    |}
    ~expected:
      (SavedStateAction.LoadFromFile
         { shared_memory_path = PyrePath.create_absolute "/some/path"; changed_files_path = None });
  assert_parsed
    {|
      [
        "load_from_file",
        {
          "shared_memory_path": "/some/path",
          "changed_files_path": "/some/other/path"
        }
      ]
    |}
    ~expected:
      (SavedStateAction.LoadFromFile
         {
           shared_memory_path = PyrePath.create_absolute "/some/path";
           changed_files_path = Some (PyrePath.create_absolute "/some/other/path");
         });
  assert_parsed
    {|
      [
        "save_to_file",
        {
          "shared_memory_path": "/some/path"
        }
      ]
    |}
    ~expected:
      (SavedStateAction.SaveToFile { shared_memory_path = PyrePath.create_absolute "/some/path" });
  assert_parsed
    {|
      [
        "load_from_project",
        {
          "project_name": "my_project"
        }
      ]
    |}
    ~expected:
      (SavedStateAction.LoadFromProject { project_name = "my_project"; project_metadata = None });
  assert_parsed
    {|
      [
        "load_from_project",
        {
          "project_name": "my_project",
          "project_metadata": "my_metadata"
        }
      ]
    |}
    ~expected:
      (SavedStateAction.LoadFromProject
         { project_name = "my_project"; project_metadata = Some "my_metadata" });
  ()


let test_query context =
  let watchman_root = PyrePath.create_absolute "/fake/root" in
  let target = PyrePath.create_absolute "/fake/target" in
  let critical_files = [CriticalFile.BaseName ".pyre_configuration"] in
  let watchman_filter =
    { Watchman.Filter.base_names = [".pyre_configuration"]; whole_names = []; suffixes = [".py"] }
  in
  let assert_request ~expected ~project_name ~project_metadata () =
    let request_mailbox = Lwt_mvar.create_empty () in
    let mock_raw =
      let send request =
        (* Avoid making assertions inside this function since the raised exception will be swallowed
           by `SavedState.query`. Instead, buffer the request until `SavedState.query` finishes. *)
        Lwt_mvar.put request_mailbox request
      in
      let receive () = Lwt.return_none in
      Watchman.Raw.create_for_testing ~send ~receive ()
    in
    Watchman.Raw.with_connection mock_raw ~f:(fun watchman_connection ->
        let savedstate_setting =
          {
            SavedState.Setting.watchman_root;
            watchman_filter;
            watchman_connection;
            project_name;
            project_metadata;
            critical_files;
            target;
          }
        in
        SavedState.query savedstate_setting)
    >>= fun _ ->
    Lwt_mvar.take request_mailbox
    >>= fun actual ->
    assert_equal
      ~ctxt:context
      ~cmp:Yojson.Safe.equal
      ~printer:Yojson.Safe.pretty_to_string
      expected
      actual;
    Lwt.return_unit
  in
  let assert_queried ~expected ~response () =
    let mock_raw =
      let send _ = Lwt.return_unit in
      let receive () = Lwt.return_some response in
      Watchman.Raw.create_for_testing ~send ~receive ()
    in
    Watchman.Raw.with_connection mock_raw ~f:(fun watchman_connection ->
        let savedstate_setting =
          {
            SavedState.Setting.watchman_root;
            watchman_filter;
            watchman_connection;
            project_name = "fake_name";
            project_metadata = None;
            critical_files;
            target;
          }
        in
        SavedState.query savedstate_setting)
    >>= fun actual ->
    let actual = Result.ok actual in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: SavedState.Queried.t option]
      ~printer:(fun result -> [%sexp_of: SavedState.Queried.t option] result |> Sexp.to_string_hum)
      expected
      actual;
    Lwt.return_unit
  in

  assert_request
    ~project_name:"my_project"
    ~project_metadata:None
    ~expected:
      (`List
        [
          `String "query";
          `String (PyrePath.absolute watchman_root);
          `Assoc
            [
              "expression", Watchman.Filter.watchman_expression_of watchman_filter;
              "fields", `List [`String "name"];
              ( "since",
                `Assoc
                  [
                    ( "scm",
                      `Assoc
                        [
                          "mergebase-with", `String "master";
                          ( "saved-state",
                            `Assoc
                              [
                                "storage", `String "manifold";
                                "config", `Assoc ["project", `String "my_project"];
                              ] );
                        ] );
                  ] );
            ];
        ])
    ()
  >>= fun () ->
  assert_queried
    ~response:
      (`Assoc
        [
          ( "saved-state-info",
            `Assoc ["manifold-bucket", `String "my_bucket"; "manifold-path", `String "my_path"] );
          "files", `List [`String "a.py"; `String "subdirectory/b.py"];
        ])
    ~expected:
      (Some
         {
           SavedState.Queried.bucket = "my_bucket";
           path = "my_path";
           target;
           changed_files =
             [
               PyrePath.create_relative ~root:watchman_root ~relative:"a.py";
               PyrePath.create_relative ~root:watchman_root ~relative:"subdirectory/b.py";
             ];
           commit_id = None;
         })
    ()
  >>= fun () ->
  assert_queried
    ~response:
      (`Assoc
        [
          ( "saved-state-info",
            `Assoc ["manifold-bucket", `String "my_bucket"; "manifold-path", `String "my_path"] );
          "files", `List [`String "a.py"; `String ".pyre_configuration"];
        ])
    ~expected:None
    ()
  >>= fun () ->
  assert_queried ~response:(`Assoc ["error", `String "Fake watchman error"]) ~expected:None ()


let () =
  "savedstate_test"
  >::: [
         "configuration_parsing" >:: test_configuration_parsing;
         "query" >:: OUnitLwt.lwt_wrapper test_query;
       ]
  |> Test.run
