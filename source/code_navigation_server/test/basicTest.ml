(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Request = CodeNavigationServer.Testing.Request
module Response = CodeNavigationServer.Testing.Response

let test_no_op_server context =
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with_one_connection ~f:(fun _ -> Lwt.return_unit)


let test_invalid_request context =
  let test_invalid_request client =
    let%lwt raw_response = ScratchProject.ClientConnection.send_raw_request client "derp" in
    match Yojson.Safe.from_string raw_response with
    | `List [`String "Error"; `List (`String "InvalidRequest" :: _)] -> Lwt.return_unit
    | _ as json ->
        let message =
          Format.sprintf "Expected error response but got: `%s`" (Yojson.Safe.to_string json)
        in
        assert_failure message
  in
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with_one_connection ~f:test_invalid_request


let test_stop_request context =
  let test_stop_request client =
    let%lwt _ =
      ScratchProject.ClientConnection.send_request client (Request.Command Request.Command.Stop)
    in
    (* Consuming the stop request would make the server terminate itself immeidately, bypassing this
       eternal-wait. *)
    let wait_forever, _ = Lwt.wait () in
    wait_forever
  in
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with_one_connection ~f:test_stop_request


let test_get_type_errors_request context =
  let project =
    ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", "reveal_type(42)"]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" in
  let expected_error =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 1;
          "column", `Int 0;
          "stop_line", `Int 1;
          "stop_column", `Int 11;
          "path", `String (PyrePath.absolute test_path);
          "code", `Int (-1);
          "name", `String "Revealed type";
          ( "description",
            `String "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`."
          );
          ( "long_description",
            `String "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`."
          );
          ( "concise_description",
            `String "Revealed type [-1]: Revealed type for `42` is `typing_extensions.Literal[42]`."
          );
          "define", `String "test.$toplevel";
        ])
    |> Result.ok_or_failwith
  in
  let open TestHelper in
  let client_id = "foo" in
  let assert_type_errors ~path client =
    ScratchProject.ClientConnection.assert_response
      client
      ~request:Request.(Query (Query.GetTypeErrors { client_id; path }))
      ~expected:(Response.TypeErrors [expected_error])
  in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        open_file ~client_id ~path:(PyrePath.absolute test_path);
        assert_type_errors ~path:(PyrePath.absolute test_path);
        ScratchProject.ClientConnection.assert_error_response
          ~request:Request.(Query (Query.GetTypeErrors { client_id; path = "/doesnotexist.py" }))
          ~kind:"FileNotOpened";
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(
              Query
                (Query.GetTypeErrors
                   { client_id = "doesnotexist"; path = PyrePath.absolute test_path }))
          ~kind:"ClientNotRegistered";
        close_file ~client_id ~path:(PyrePath.absolute test_path);
        dispose_client ~client_id;
      ]


let test_file_opened_and_closed_request context =
  let project =
    ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", "reveal_type(42)"]
  in
  let test_path =
    let source_root = ScratchProject.source_root_of project in
    PyrePath.append source_root ~element:"test.py" |> PyrePath.absolute
  in
  let open TestHelper in
  let client_id = "foo" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        assert_type_error_count_for_path ~path:test_path ~client_id ~expected:1;
        open_file ~path:test_path ~content:"reveal_type(43)\nreveal_type(44)" ~client_id;
        assert_type_error_count_for_path ~path:test_path ~client_id ~expected:2;
        ScratchProject.ClientConnection.assert_response
          ~request:Request.(Command (Command.FileClosed { path = "/untracked/file.py"; client_id }))
          ~expected:
            (Response.Error (Response.ErrorKind.FileNotOpened { path = "/untracked/file.py" }));
        assert_type_error_count_for_path ~path:test_path ~client_id ~expected:2;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command (Command.FileClosed { path = test_path; client_id = "nonexistent_client_id" }))
          ~expected:(Response.Error (Response.ErrorKind.FileNotOpened { path = test_path }));
        assert_type_error_count_for_path ~path:test_path ~client_id ~expected:2;
        close_file ~path:test_path ~client_id;
        assert_type_error_count_for_path ~path:test_path ~client_id ~expected:1;
        (* Now that foo is no longer tracked as an open file, this should error. *)
        ScratchProject.ClientConnection.assert_response
          ~request:Request.(Command (Command.FileClosed { path = test_path; client_id }))
          ~expected:(Response.Error (Response.ErrorKind.FileNotOpened { path = test_path }));
        dispose_client ~client_id;
      ]


let test_local_update_request context =
  let project =
    ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", "reveal_type(42)"]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" |> PyrePath.absolute in
  let open TestHelper in
  let client_foo = "foo" in
  let client_bar = "bar" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id:client_foo;
        register_client ~client_id:client_bar;
        open_file ~path:test_path ~client_id:client_foo;
        open_file ~path:test_path ~client_id:client_bar;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate
                   {
                     path = test_path;
                     content = Some "reveal_type(43)\nreveal_type(44)";
                     client_id = client_foo;
                   }))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_bar ~expected:1;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_foo ~expected:2;
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate
                   { path = "/doesnotexist"; content = Some ""; client_id = client_foo }))
          ~kind:"FileNotOpened";
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate
                   {
                     path = test_path;
                     content = Some "reveal_type(43)\nreveal_type(44)\nreveal_type(45)";
                     client_id = client_foo;
                   }))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_bar ~expected:1;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_foo ~expected:3;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate
                   {
                     path = test_path;
                     content = Some "reveal_type(43)\nreveal_type(44)";
                     client_id = client_bar;
                   }))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_bar ~expected:2;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_foo ~expected:3;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate { path = test_path; content = None; client_id = client_foo }))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_foo ~expected:1;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate { path = test_path; content = None; client_id = client_bar }))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:test_path ~client_id:client_bar ~expected:1;
        close_file ~path:test_path ~client_id:client_foo;
        close_file ~path:test_path ~client_id:client_bar;
        dispose_client ~client_id:client_foo;
        dispose_client ~client_id:client_bar;
      ]


let test_file_update_request context =
  let project =
    ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", "reveal_type(42)"]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" in
  let test2_path = PyrePath.create_relative ~root ~relative:"test2.py" in
  let open TestHelper in
  let client_id = "foo" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(Query (Query.GetTypeErrors { client_id; path = PyrePath.absolute test_path }))
          ~kind:"FileNotOpened";
        open_file ~client_id ~path:(PyrePath.absolute test_path);
        assert_type_error_count_for_path ~path:(PyrePath.absolute test_path) ~client_id ~expected:1;
        (fun _ ->
          PyrePath.remove test_path;
          Lwt.return_unit);
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.FileUpdate
                   [FileUpdateEvent.{ kind = Kind.Deleted; path = PyrePath.absolute test_path }]))
          ~expected:Response.Ok;
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(Query (Query.GetTypeErrors { client_id; path = PyrePath.absolute test_path }))
          ~kind:"ModuleNotTracked";
        close_file ~client_id ~path:(PyrePath.absolute test_path);
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(Query (Query.GetTypeErrors { client_id; path = PyrePath.absolute test_path }))
          ~kind:"FileNotOpened";
        (fun _ ->
          File.create test2_path ~content:"reveal_type(43)\nreveal_type(44)" |> File.write;
          Lwt.return_unit);
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.FileUpdate
                   [
                     FileUpdateEvent.
                       { kind = Kind.CreatedOrChanged; path = PyrePath.absolute test2_path };
                   ]))
          ~expected:Response.Ok;
        open_file ~client_id ~path:(PyrePath.absolute test2_path);
        assert_type_error_count_for_path ~path:(PyrePath.absolute test2_path) ~client_id ~expected:2;
        close_file ~client_id ~path:(PyrePath.absolute test2_path);
        dispose_client ~client_id;
      ]


let test_file_and_local_update context =
  let project =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs:false
      ["test.py", "reveal_type(42)\nreveal_type(43)"; "test2.py", ""]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" in
  let test2_path = PyrePath.create_relative ~root ~relative:"test2.py" in
  let open TestHelper in
  let client_id = "foo" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        open_file ~path:(PyrePath.absolute test_path) ~client_id;
        open_file ~path:(PyrePath.absolute test2_path) ~client_id;
        assert_type_error_count_for_path ~path:(PyrePath.absolute test_path) ~client_id ~expected:2;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.LocalUpdate
                   {
                     path = PyrePath.absolute test_path;
                     content = Some "from test2 import x";
                     client_id;
                   }))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:(PyrePath.absolute test_path) ~client_id ~expected:1;
        (fun _ ->
          File.create test2_path ~content:"x: int = 42" |> File.write;
          Lwt.return_unit);
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Command
                (Command.FileUpdate
                   [
                     FileUpdateEvent.
                       { kind = Kind.CreatedOrChanged; path = PyrePath.absolute test2_path };
                   ]))
          ~expected:Response.Ok;
        assert_type_error_count_for_path ~path:(PyrePath.absolute test_path) ~client_id ~expected:0;
        close_file ~path:(PyrePath.absolute test_path) ~client_id;
        close_file ~path:(PyrePath.absolute test2_path) ~client_id;
        dispose_client ~client_id;
      ]


let test_hover_request context =
  let project =
    ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", "x: float = 4.2"]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" |> PyrePath.absolute in
  let open TestHelper in
  let client_id = "foo" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        open_file ~client_id ~path:test_path;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              (* This location point to empty space. *)
              Query (Query.Hover { client_id; path = test_path; position = position 1 2 }))
          ~expected:
            Response.(Hover { contents = HoverContent.[{ value = None; docstring = None }] });
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(Query (Query.Hover { client_id; path = test_path; position = position 1 0 }))
          ~expected:
            Response.(
              Hover { contents = HoverContent.[{ value = Some "float"; docstring = None }] });
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(Query (Query.Hover { client_id; path = test_path; position = position 1 0 }))
          ~expected:
            Response.(
              Hover { contents = HoverContent.[{ value = Some "float"; docstring = None }] });
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(
              Query (Query.Hover { client_id; path = "/doesnotexist.py"; position = position 1 0 }))
          ~kind:"FileNotOpened";
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(
              Query
                (Query.Hover
                   { client_id = "doesnotexist"; path = test_path; position = position 1 0 }))
          ~kind:"ClientNotRegistered";
        close_file ~client_id ~path:test_path;
        dispose_client ~client_id;
      ]


let test_location_of_definition_request context =
  let project =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs:false
      ["test.py", "x: int = 42"; "test2.py", "import test\ny: int = test.x"]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" |> PyrePath.absolute in
  let test2_path = PyrePath.create_relative ~root ~relative:"test2.py" |> PyrePath.absolute in
  let open TestHelper in
  let client_id = "foo" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        open_file ~client_id ~path:test_path;
        open_file ~client_id ~path:test2_path;
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              (* This location points to an empty space *)
              Query
                (Query.LocationOfDefinition
                   { client_id; path = test2_path; position = position 2 8 }))
          ~expected:Response.(LocationOfDefinition { definitions = [] });
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              (* This location points to `test.x` *)
              Query
                (Query.LocationOfDefinition
                   { client_id; path = test2_path; position = position 2 14 }))
          ~expected:
            Response.(
              LocationOfDefinition
                { definitions = [{ DefinitionLocation.path = test_path; range = range 1 0 1 1 }] });
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.LocationOfDefinition
                   { client_id; path = test2_path; position = position 2 14 }))
          ~expected:
            Response.(
              LocationOfDefinition
                { definitions = [{ DefinitionLocation.path = test_path; range = range 1 0 1 1 }] });
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(
              Query
                (Query.LocationOfDefinition
                   { client_id; path = "/doesnotexist.py"; position = position 1 0 }))
          ~kind:"FileNotOpened";
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(
              Query
                (Query.LocationOfDefinition
                   { client_id = "doesnotexist"; path = test_path; position = position 1 0 }))
          ~kind:"ClientNotRegistered";
        close_file ~client_id ~path:test_path;
        close_file ~client_id ~path:test2_path;
        dispose_client ~client_id;
      ]


let test_get_info_request context =
  let project = ScratchProject.setup ~context ~include_typeshed_stubs:false [] in
  let root = ScratchProject.source_root_of project in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        ScratchProject.ClientConnection.assert_response
          ~request:Request.((* This location points to an empty space *)
                            Query Query.GetInfo)
          ~expected:
            Response.(
              Info
                {
                  socket = PyrePath.show project.start_options.socket_path;
                  pid = Core_unix.getpid () |> Pid.to_int;
                  version = Version.version ();
                  global_root = PyrePath.show root;
                  relative_local_root = None;
                });
      ]


let test_superclasses context =
  let project =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs:false
      ["test.py", "class C: pass\nclass D(C): pass\nclass E(D): pass"]
  in
  let test_path =
    let source_root = ScratchProject.source_root_of project in
    PyrePath.append source_root ~element:"test.py" |> PyrePath.absolute
  in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.Superclasses
                   { class_ = { ClassExpression.module_ = "test"; qualified_name = "C" } }))
          ~expected:
            Response.(
              Superclasses
                {
                  superclasses =
                    [Request.{ ClassExpression.module_ = ""; qualified_name = "object" }];
                });
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.Superclasses
                   { class_ = { ClassExpression.module_ = "test"; qualified_name = "D" } }))
          ~expected:
            Response.(
              Superclasses
                {
                  superclasses =
                    [
                      Request.{ ClassExpression.module_ = "test"; qualified_name = "C" };
                      Request.{ ClassExpression.module_ = ""; qualified_name = "object" };
                    ];
                });
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.Superclasses
                   { class_ = { ClassExpression.module_ = "test"; qualified_name = "E" } }))
          ~expected:
            Response.(
              Superclasses
                {
                  superclasses =
                    [
                      Request.{ ClassExpression.module_ = "test"; qualified_name = "D" };
                      Request.{ ClassExpression.module_ = "test"; qualified_name = "C" };
                      Request.{ ClassExpression.module_ = ""; qualified_name = "object" };
                    ];
                });
        (* Non-existent module. *)
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.Superclasses
                   { class_ = { ClassExpression.module_ = "missing_module"; qualified_name = "C" } }))
          ~expected:
            Response.(
              Error (ErrorKind.InvalidRequest "Cannot find module with name `missing_module`"));
        (* Non-existent class. *)
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.Superclasses
                   {
                     class_ = { ClassExpression.module_ = "test"; qualified_name = "CDoesNotExist" };
                   }))
          ~expected:
            Response.(
              Error (ErrorKind.InvalidRequest "Cannot find class `CDoesNotExist` in module `test`."));
        (* Local changes *)
        TestHelper.register_client ~client_id:"foo";
        TestHelper.open_file ~path:test_path ~content:"class OnlyInOverlay: ..." ~client_id:"foo";
        ScratchProject.ClientConnection.assert_response
          ~request:
            Request.(
              Query
                (Query.Superclasses
                   {
                     class_ = { ClassExpression.module_ = "test"; qualified_name = "OnlyInOverlay" };
                   }))
          ~expected:
            Response.(
              Error (ErrorKind.InvalidRequest "Cannot find class `OnlyInOverlay` in module `test`."));
        TestHelper.close_file ~path:test_path ~client_id:"foo";
        TestHelper.dispose_client ~client_id:"foo";
      ]


let watchman_version = "fake_watchman_version"

let watchman_initial_clock = "fake:clock:0"

(* We use a mailbox variable in this test to get a more precise control of when a given watchman
   response can reach the Pyre serer. *)
let create_mock_watchman mailbox =
  let initialize_stage = ref 0 in
  let send _ = Lwt.return_unit in
  let receive () =
    match !initialize_stage with
    | 0 ->
        let watchman_watch_response =
          `Assoc ["version", `String watchman_version; "watcher", `String "fake_watcher"]
        in
        initialize_stage := 1;
        Lwt.return_some watchman_watch_response
    | 1 ->
        let watchman_subscribe_response =
          `Assoc ["version", `String watchman_version; "clock", `String watchman_initial_clock]
        in
        initialize_stage := 2;
        Lwt.return_some watchman_subscribe_response
    | _ ->
        let%lwt message = Lwt_mvar.take mailbox in
        Lwt.return_some message
  in
  Server.Watchman.Raw.create_for_testing ~send ~receive ()


let test_watchman_integration context =
  let watchman_mailbox = Lwt_mvar.create_empty () in
  let mock_watchman = create_mock_watchman watchman_mailbox in
  let project =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs:false
      ~watchman:mock_watchman
      ["test.py", "from test2 import x"; "test2.py", "x: int = 42"]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" in
  let test2_path = PyrePath.create_relative ~root ~relative:"test2.py" in
  let watchman_update_response file_names =
    `Assoc
      [
        "is_fresh_instance", `Bool false;
        "files", `List (List.map file_names ~f:(fun name -> `String name));
        "root", `String (PyrePath.absolute root);
        "version", `String watchman_version;
        "clock", `String "fake:clock:update";
        "since", `String watchman_initial_clock;
      ]
  in
  let open TestHelper in
  let client_id = "foo" in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Sequential
    ~clients:
      [
        register_client ~client_id;
        open_file ~client_id ~path:(PyrePath.absolute test_path);
        assert_type_error_count_for_path ~path:(PyrePath.absolute test_path) ~client_id ~expected:0;
        (fun _ ->
          File.create ~content:"" test2_path |> File.write;
          Lwt_mvar.put watchman_mailbox (watchman_update_response ["test2.py"]));
        assert_type_error_count_for_path ~path:(PyrePath.absolute test_path) ~client_id ~expected:1;
        (fun _ ->
          PyrePath.remove test_path;
          Lwt_mvar.put watchman_mailbox (watchman_update_response ["test.py"]));
        ScratchProject.ClientConnection.assert_error_response
          ~request:
            Request.(Query (Query.GetTypeErrors { client_id; path = PyrePath.absolute test_path }))
          ~kind:"ModuleNotTracked";
        close_file ~client_id ~path:(PyrePath.absolute test_path);
        dispose_client ~client_id;
      ]


let test_watchman_failure context =
  let watchman_mailbox = Lwt_mvar.create_empty () in
  let mock_watchman = create_mock_watchman watchman_mailbox in
  let test_watchman_failure _ =
    let cancel_response = `Assoc ["canceled", `Bool true] in
    let%lwt () = Lwt_mvar.put watchman_mailbox cancel_response in
    (* Consuming the watchman cancel notification would make the server terminate itself
       immeidately, bypassing this eternal-wait. *)
    let wait_forever, _ = Lwt.wait () in
    wait_forever
  in
  let project =
    ScratchProject.setup ~context ~include_typeshed_stubs:false ~watchman:mock_watchman []
  in
  try%lwt
    let%lwt () = ScratchProject.test_server_with_one_connection ~f:test_watchman_failure project in
    assert_failure "Server is expected to crash but it returned normally instead"
  with
  | Server.Watchman.SubscriptionError _ -> Lwt.return_unit


let ( >:: ) name test = name >:: OUnitLwt.lwt_wrapper test

let () =
  "basic_test"
  >::: [
         "no_op_server" >:: test_no_op_server;
         "invalid_request" >:: test_invalid_request;
         "stop_request" >:: test_stop_request;
         "get_type_errors_request" >:: test_get_type_errors_request;
         "get_info_request" >:: test_get_info_request;
         "local_update_request" >:: test_local_update_request;
         "file_update_request" >:: test_file_update_request;
         "file_and_local_update" >:: test_file_and_local_update;
         "hover_request" >:: test_hover_request;
         "location_of_definition_request" >:: test_location_of_definition_request;
         "superclasses" >:: test_superclasses;
         "watchman_integration" >:: test_watchman_integration;
         "watchman_failure" >:: test_watchman_failure;
       ]
  |> Test.run
