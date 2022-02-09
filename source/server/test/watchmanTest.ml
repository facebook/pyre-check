(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server

let test_low_level_apis _ =
  let open Lwt.Infix in
  let open Watchman.Raw in
  (* A `.watchmanconfig` file is required at watchman root directory. *)
  let test_connection connection =
    let assert_member_exists ~key json =
      match Yojson.Safe.Util.member key json with
      | `Null ->
          let message = Format.sprintf "Failed to find required JSON member: %s" key in
          assert_failure message
      | _ -> ()
    in
    (* Send a simple `watchman version` request. *)
    let version_request = `List [`String "version"] in
    Connection.send connection version_request
    >>= fun () ->
    Connection.receive connection ()
    >>= function
    | Response.EndOfStream -> assert_failure "Unexpected end-of-stream response from watchman"
    | Response.Error message ->
        let message = Format.sprintf "Unexpected failure response from watchman: %s" message in
        assert_failure message
    | Response.Ok response ->
        assert_member_exists response ~key:"version";
        Lwt.return_unit
  in
  Lwt.catch
    (fun () -> create_exn () >>= with_connection ~f:test_connection)
    (function
      | OUnitTest.OUnit_failure _ as exn ->
          (* We need to re-raise OUnit test failures since OUnit relies on it for error reporting. *)
          raise exn
      | _ as exn ->
          Format.printf
            "Skipping low-level watchman API test due to exception: %s\n"
            (Exn.to_string exn);
          Lwt.return_unit)


let test_subscription _ =
  let open Lwt.Infix in
  let root = PyrePath.create_absolute "/fake/root" in
  let version_name = "fake_watchman_version" in
  let subscription_name = "my_subscription" in
  let initial_clock = "fake:clock:0" in
  let initial_success_response =
    `Assoc
      [
        "version", `String version_name;
        "subscribe", `String subscription_name;
        "clock", `String initial_clock;
      ]
  in
  let initial_fail_response =
    `Assoc
      ["version", `String version_name; "error", `String "RootResolveError: unable to resolve root"]
  in
  let watch_success_response =
    `Assoc
      [
        "version", `String version_name;
        "watch", `String (PyrePath.absolute root);
        "watcher", `String "fake_watcher";
      ]
  in
  let watch_fail_response =
    `Assoc ["version", `String version_name; "error", `String "watchman::CommandValidationError"]
  in
  let update_response ?(is_fresh_instance = false) ?(clock = "fake:clock:default") file_names =
    `Assoc
      [
        "is_fresh_instance", `Bool is_fresh_instance;
        "files", `List (List.map file_names ~f:(fun name -> `String name));
        "root", `String (PyrePath.absolute root);
        "version", `String version_name;
        "clock", `String clock;
        (* This can be mocked better once we start to utilize `since` queries. *)
        "since", `String initial_clock;
      ]
  in
  let assert_updates ?(should_raise = false) ~expected responses =
    let mock_raw =
      (* Our simple mock server doesn't really care what get sent to it. *)
      let send _ = Lwt.return_unit in
      (* Our simple mock server simply forward the mocked responses in order. *)
      let remaining_responses = ref responses in
      let receive () =
        match !remaining_responses with
        | [] -> Lwt.return_none
        | next_response :: rest ->
            remaining_responses := rest;
            Lwt.return_some next_response
      in
      Watchman.Raw.create_for_testing ~send ~receive ()
    in
    let remaining_updates = ref expected in
    let assert_update actual =
      match !remaining_updates with
      | [] -> assert_failure "Watchman subscriber receives more updates than expected."
      | next_update :: rest ->
          remaining_updates := rest;
          assert_equal
            ~cmp:[%compare.equal: PyrePath.t list]
            ~printer:(fun paths -> [%sexp_of: PyrePath.t list] paths |> Sexp.to_string_hum)
            next_update
            actual;
          Lwt.return_unit
    in
    let setting =
      {
        Watchman.Subscriber.Setting.raw = mock_raw;
        root;
        (* We are not going to test these settings so they can be anything. *)
        filter = { Watchman.Filter.base_names = []; whole_names = []; suffixes = [] };
      }
    in
    Lwt.catch
      (fun () ->
        Watchman.Subscriber.with_subscription setting ~f:assert_update
        >>= fun () ->
        if should_raise then
          assert_failure "Expected an exception to raise but did not get one";
        Lwt.return_unit)
      (fun exn ->
        (if not should_raise then
           let message = Format.sprintf "Unexpected exception: %s" (Exn.to_string exn) in
           assert_failure message);
        Lwt.return_unit)
  in

  (* Lack of watch-project response would raise. *)
  assert_updates ~should_raise:true ~expected:[] []
  >>= fun () ->
  (* Failing watch-project response would raise. *)
  assert_updates ~should_raise:true ~expected:[] [watch_fail_response]
  >>= fun () ->
  (* Lack of initial response would raise. *)
  assert_updates ~should_raise:true ~expected:[] [watch_success_response]
  >>= fun () ->
  (* Failing initial response would raise. *)
  assert_updates ~should_raise:true ~expected:[] [watch_success_response; initial_fail_response]
  >>= fun () ->
  (* Missing initial clock would raise. *)
  assert_updates
    ~should_raise:true
    ~expected:[]
    [
      watch_success_response;
      `Assoc ["version", `String version_name; "subscribe", `String subscription_name];
    ]
  >>= fun () ->
  (* Non-files update is ok. *)
  assert_updates
    ~expected:[]
    [
      watch_success_response;
      initial_success_response;
      `Assoc
        [
          "unilateral", `Bool true;
          "root", `String (PyrePath.absolute root);
          "subscription", `String subscription_name;
          "version", `String version_name;
          "clock", `String initial_clock;
          "state-enter", `String "hg.transaction";
        ];
    ]
  >>= fun () ->
  (* Single file in single update. *)
  assert_updates
    ~expected:[[PyrePath.create_relative ~root ~relative:"foo.py"]]
    [watch_success_response; initial_success_response; update_response ["foo.py"]]
  >>= fun () ->
  (* Multiple files in single update. *)
  assert_updates
    ~expected:
      [
        [
          PyrePath.create_relative ~root ~relative:"foo.py";
          PyrePath.create_relative ~root ~relative:"bar/baz.py";
        ];
      ]
    [watch_success_response; initial_success_response; update_response ["foo.py"; "bar/baz.py"]]
  >>= fun () ->
  (* Single file in multiple updates. *)
  assert_updates
    ~expected:
      [
        [PyrePath.create_relative ~root ~relative:"foo.py"];
        [PyrePath.create_relative ~root ~relative:"bar/baz.py"];
      ]
    [
      watch_success_response;
      initial_success_response;
      update_response ["foo.py"];
      update_response ["bar/baz.py"];
    ]
  >>= fun () ->
  (* Multiple files in multiple updates. *)
  assert_updates
    ~expected:
      [
        [
          PyrePath.create_relative ~root ~relative:"foo.py";
          PyrePath.create_relative ~root ~relative:"bar/baz.py";
        ];
        [
          PyrePath.create_relative ~root ~relative:"my/cat.py";
          PyrePath.create_relative ~root ~relative:"dog.py";
        ];
      ]
    [
      watch_success_response;
      initial_success_response;
      update_response ["foo.py"; "bar/baz.py"];
      update_response ["my/cat.py"; "dog.py"];
    ]
  >>= fun () ->
  (* Initial fresh instance update is ok *)
  assert_updates
    ~expected:[[PyrePath.create_relative ~root ~relative:"foo.py"]]
    [
      watch_success_response;
      initial_success_response;
      update_response ~clock:initial_clock ~is_fresh_instance:true [];
      update_response ~clock:"fake:clock:1" ["foo.py"];
    ]
  >>= fun () ->
  (* Non-initial fresh instance update is not ok *)
  assert_updates
    ~should_raise:true
    ~expected:[]
    [
      watch_success_response;
      initial_success_response;
      update_response ~clock:"fake:clock:1" ~is_fresh_instance:true [];
    ]
  >>= fun () ->
  assert_updates
    ~should_raise:true
    ~expected:[]
    [
      watch_success_response;
      initial_success_response;
      update_response ~clock:initial_clock ~is_fresh_instance:true [];
      update_response ~clock:"fake:clock:1" ~is_fresh_instance:true [];
    ]
  >>= fun () ->
  assert_updates
    ~should_raise:true
    ~expected:[[PyrePath.create_relative ~root ~relative:"foo.py"]]
    [
      watch_success_response;
      initial_success_response;
      update_response ["foo.py"];
      update_response ~clock:"fake:clock:1" ~is_fresh_instance:true ["bar.py"];
    ]
  >>= fun () ->
  (* Correctly identify cancellation response. *)
  assert_updates
    ~should_raise:true
    ~expected:[]
    [
      watch_success_response;
      initial_success_response;
      Yojson.Safe.Util.combine (update_response ["foo.py"]) (`Assoc ["canceled", `Bool true]);
    ]
  >>= fun () -> Lwt.return_unit


let test_filter_expression context =
  let assert_expression ~expected filter =
    let actual = Watchman.Filter.watchman_expression_of filter in
    assert_equal
      ~ctxt:context
      ~cmp:Yojson.Safe.equal
      ~printer:Yojson.Safe.pretty_to_string
      expected
      actual
  in
  assert_expression
    {
      Watchman.Filter.base_names = ["foo.txt"; "TARGETS"];
      whole_names = ["bar.json"];
      suffixes = ["cc"; "cpp"];
    }
    ~expected:
      (`List
        [
          `String "allof";
          `List [`String "type"; `String "f"];
          `List
            [
              `String "anyof";
              `List [`String "suffix"; `String "cc"];
              `List [`String "suffix"; `String "cpp"];
              `List [`String "match"; `String "foo.txt"; `String "basename"];
              `List [`String "match"; `String "TARGETS"; `String "basename"];
              `List [`String "match"; `String "bar.json"; `String "wholename"];
            ];
        ]);
  ()


let test_filter_creation context =
  let assert_filter
      ~expected:
        {
          Watchman.Filter.base_names = expected_base_names;
          whole_names = expected_whole_names;
          suffixes = expected_suffixes;
        }
      {
        Watchman.Filter.base_names = actual_base_names;
        whole_names = actual_whole_names;
        suffixes = actual_suffixes;
      }
    =
    let cmp = [%compare.equal: string list] in
    let printer contents = Base.Sexp.to_string_hum ([%sexp_of: string list] contents) in
    let sorted = List.sort ~compare:String.compare in
    assert_equal ~ctxt:context ~cmp ~printer (sorted expected_base_names) (sorted actual_base_names);
    assert_equal
      ~ctxt:context
      ~cmp
      ~printer
      (sorted expected_whole_names)
      (sorted actual_whole_names);
    assert_equal ~ctxt:context ~cmp ~printer (sorted expected_suffixes) (sorted actual_suffixes)
  in

  let open Configuration in
  let open Watchman.Filter in
  assert_filter
    (from_server_configurations
       ~critical_files:[]
       ~extensions:[]
       ~source_paths:(SourcePaths.Simple [])
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"];
        whole_names = [];
        suffixes = ["py"; "pyi"];
      };
  assert_filter
    (from_server_configurations
       ~critical_files:[]
       ~extensions:[Extension.create_extension ".foo"]
       ~source_paths:(SourcePaths.Simple [])
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"];
        whole_names = [];
        suffixes = ["py"; "pyi"; "foo"];
      };
  assert_filter
    (from_server_configurations
       ~critical_files:
         [
           CriticalFile.BaseName "foo.txt";
           CriticalFile.FullPath (PyrePath.create_absolute "/derp/bar.txt");
           CriticalFile.Extension "bar";
         ]
       ~extensions:[]
       ~source_paths:(SourcePaths.Simple [])
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"; "foo.txt"; "bar.txt"];
        whole_names = [];
        suffixes = ["py"; "pyi"; "bar"];
      };
  assert_filter
    (from_server_configurations
       ~critical_files:[CriticalFile.Extension "py"; CriticalFile.Extension "pyi"]
       ~extensions:[]
       ~source_paths:(SourcePaths.Simple [])
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"];
        whole_names = [];
        suffixes = ["py"; "pyi"];
      };
  assert_filter
    (from_server_configurations
       ~critical_files:
         [
           CriticalFile.BaseName ".pyre_configuration";
           CriticalFile.BaseName ".pyre_configuration.local";
           CriticalFile.FullPath (PyrePath.create_absolute "/foo/bar.txt");
           CriticalFile.BaseName "bar.txt";
         ]
       ~extensions:[]
       ~source_paths:(SourcePaths.Simple [])
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"; "bar.txt"];
        whole_names = [];
        suffixes = ["py"; "pyi"];
      };
  assert_filter
    (from_server_configurations
       ~critical_files:[]
       ~extensions:[]
       ~source_paths:
         (SourcePaths.WithUnwatchedDependency
            {
              sources = [];
              unwatched_dependency =
                {
                  UnwatchedDependency.change_indicator =
                    {
                      ChangeIndicator.root = PyrePath.create_absolute "/foo";
                      relative = "bar/baz.txt";
                    };
                  files =
                    {
                      UnwatchedFiles.root = PyrePath.create_absolute "/derp";
                      checksum_path = "anything";
                    };
                };
            })
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"];
        whole_names = ["bar/baz.txt"];
        suffixes = ["py"; "pyi"];
      };
  assert_filter
    (from_server_configurations
       ~critical_files:[]
       ~extensions:[]
       ~source_paths:
         (SourcePaths.Buck
            {
              Buck.mode = None;
              isolation_prefix = None;
              use_buck2 = false;
              targets = [];
              source_root = PyrePath.create_absolute "/source";
              artifact_root = PyrePath.create_absolute "/artifact";
            })
       ())
    ~expected:
      {
        base_names = [".pyre_configuration"; ".pyre_configuration.local"; "TARGETS"; "BUCK"];
        whole_names = [];
        suffixes = ["py"; "pyi"];
      };
  ()


let test_since_query_request context =
  let open Watchman.SinceQuery in
  let root = PyrePath.create_absolute "/fake/root" in
  let filter =
    { Watchman.Filter.base_names = [".pyre_configuration"]; whole_names = []; suffixes = [".py"] }
  in
  let assert_request ~expected request =
    let actual = watchman_request_of request in
    assert_equal
      ~ctxt:context
      ~cmp:Yojson.Safe.equal
      ~printer:Yojson.Safe.pretty_to_string
      expected
      actual
  in
  assert_request
    { root; filter; since = Since.Clock "fake:clock" }
    ~expected:
      (Yojson.Safe.from_string
         {|
           [
             "query",
             "/fake/root",
             {
               "fields": [ "name" ],
               "expression": [
                 "allof",
                 [ "type", "f" ],
                 [ "anyof", [ "suffix", ".py" ], [ "match", ".pyre_configuration", "basename" ] ]
               ],
               "since": "fake:clock"
             }
           ]
         |});
  assert_request
    {
      root;
      filter;
      since = Since.SourceControlAware { mergebase_with = "master"; saved_state = None };
    }
    ~expected:
      (Yojson.Safe.from_string
         {|
           [
             "query",
             "/fake/root",
             {
               "fields": [ "name" ],
               "expression": [
                 "allof",
                 [ "type", "f" ],
                 [ "anyof", [ "suffix", ".py" ], [ "match", ".pyre_configuration", "basename" ] ]
               ],
               "since": { "scm": { "mergebase-with": "master" } }
             }
           ]
         |});
  assert_request
    {
      root;
      filter;
      since =
        Since.SourceControlAware
          {
            mergebase_with = "master";
            saved_state =
              Some
                {
                  Since.SavedState.storage = "my_storage";
                  project_name = "my_project";
                  project_metadata = None;
                };
          };
    }
    ~expected:
      (Yojson.Safe.from_string
         {|
           [
             "query",
             "/fake/root",
             {
               "fields": [ "name" ],
               "expression": [
                 "allof",
                 [ "type", "f" ],
                 [ "anyof", [ "suffix", ".py" ], [ "match", ".pyre_configuration", "basename" ] ]
               ],
               "since": {
                 "scm": {
                   "mergebase-with": "master",
                   "saved-state": {
                     "storage": "my_storage",
                     "config": { "project": "my_project" }
                   }
                 }
               }
             }
           ]
         |});
  assert_request
    {
      root;
      filter;
      since =
        Since.SourceControlAware
          {
            mergebase_with = "master";
            saved_state =
              Some
                {
                  Since.SavedState.storage = "my_storage";
                  project_name = "my_project";
                  project_metadata = Some "my_metadata";
                };
          };
    }
    ~expected:
      (Yojson.Safe.from_string
         {|
           [
             "query",
             "/fake/root",
             {
               "fields": [ "name" ],
               "expression": [
                 "allof",
                 [ "type", "f" ],
                 [ "anyof", [ "suffix", ".py" ], [ "match", ".pyre_configuration", "basename" ] ]
               ],
               "since": {
                 "scm": {
                   "mergebase-with": "master",
                   "saved-state": {
                     "storage": "my_storage",
                     "config": {
                       "project": "my_project",
                       "project-metadata": "my_metadata"
                     }
                   }
                 }
               }
             }
           ]
         |});
  ()


let test_since_query_response context =
  let open Watchman.SinceQuery in
  let assert_response ~expected response =
    let actual = Response.of_watchman_response response in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Response.t option]
      ~printer:(fun response -> [%sexp_of: Response.t option] response |> Sexp.to_string_hum)
      expected
      actual
  in
  assert_response
    (`Assoc ["files", `List [`String "a.py"; `String "subdirectory/b.py"]])
    ~expected:(Some { Response.relative_paths = ["a.py"; "subdirectory/b.py"]; saved_state = None });
  assert_response
    (`Assoc
      [
        ( "saved-state-info",
          `Assoc ["manifold-bucket", `String "my_bucket"; "manifold-path", `String "my_path"] );
        "files", `List [`String "a.py"; `String "subdirectory/b.py"];
      ])
    ~expected:
      (Some
         {
           Response.relative_paths = ["a.py"; "subdirectory/b.py"];
           saved_state =
             Some { Response.SavedState.bucket = "my_bucket"; path = "my_path"; commit_id = None };
         });
  assert_response
    (`Assoc
      [
        ( "saved-state-info",
          `Assoc
            [
              "manifold-bucket", `String "my_bucket";
              "manifold-path", `String "my_path";
              "commit-id", `String "my_commit";
            ] );
        "files", `List [`String "a.py"; `String "subdirectory/b.py"];
      ])
    ~expected:
      (Some
         {
           Response.relative_paths = ["a.py"; "subdirectory/b.py"];
           saved_state =
             Some
               {
                 Response.SavedState.bucket = "my_bucket";
                 path = "my_path";
                 commit_id = Some "my_commit";
               };
         });
  ()


let test_since_query _ =
  let open Lwt.Infix in
  (* Test what happens when watchman sends back no response. *)
  let mock_raw =
    let send _ = Lwt.return_unit in
    let receive () = Lwt.return_none in
    Watchman.Raw.create_for_testing ~send ~receive ()
  in
  Lwt.catch
    (fun () ->
      Watchman.Raw.with_connection mock_raw ~f:(fun connection ->
          Watchman.SinceQuery.(
            query_exn
              ~connection
              {
                root = PyrePath.create_absolute "/fake/root";
                filter = { Watchman.Filter.base_names = []; whole_names = []; suffixes = [] };
                since = Since.Clock "fake:clock";
              }))
      >>= fun _ -> assert_failure "Unexpected success")
    (function
      | Watchman.QueryError _ -> Lwt.return_unit
      | _ as exn ->
          let message = Format.sprintf "Unexpected failure: %s" (Exn.to_string exn) in
          assert_failure message)


let () =
  "watchman_test"
  >::: [
         "low_level" >:: OUnitLwt.lwt_wrapper test_low_level_apis;
         "subscription" >:: OUnitLwt.lwt_wrapper test_subscription;
         "filter_expression" >:: test_filter_expression;
         "filter_creation" >:: test_filter_creation;
         "since_query_request" >:: test_since_query_request;
         "since_query_response" >:: test_since_query_response;
         "since_query" >:: OUnitLwt.lwt_wrapper test_since_query;
       ]
  |> Test.run
