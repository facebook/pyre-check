(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Pyre
open Server
open Test


let mock_server_state ?(sources = []) ?(errors = File.Handle.Table.create ()) () =
  let environment =
    let configuration = Test.mock_configuration in
    let environment =
      let environment = Analysis.Environment.Builder.create () in
      Service.Environment.populate
        (Analysis.Environment.handler ~configuration environment)
        (typeshed_stubs @ sources);
      environment
    in
    Analysis.Environment.handler ~configuration environment
  in
  add_defaults_to_environment environment;
  {
    State.deferred_requests = [];
    environment;
    errors;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    lookups = String.Table.create ();
    lock = Mutex.create ();
    connections = ref {
        State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
        persistent_clients = Unix.File_descr.Table.create ();
        file_notifiers = [];
        watchman_pid = None;
      };
    scheduler = Scheduler.mock ();
  }


let initialize sources =
  let configuration =
    Configuration.Analysis.create
      ~project_root:(Path.current_working_directory ())
      ()
  in
  let state =
    let files = List.map sources ~f:write_file in
    let scheduler = Scheduler.mock () in

    (* Clear and re-populate ASTs in shared memory. *)
    let handles = List.map files ~f:(File.handle ~configuration) in
    Ast.SharedMemory.Sources.remove ~handles;
    Service.Parser.parse_sources ~configuration ~scheduler ~files
    |> ignore;

    (* Initialize dependency map. *)
    let source (path, content) =
      let handle = File.Handle.create path in
      parse ~qualifier:(Source.qualifier ~handle) ~handle:path content
      |> Analysis.Preprocessing.qualify
    in
    mock_server_state ~sources:(List.map sources ~f:source) ()
  in
  configuration, state


let test_process_client_shutdown_request _ =
  let assert_response id expected_response =
    let state = mock_server_state () in
    let actual_response =
      Request.process_client_shutdown_request ~state ~id
      |> function
      | { Request.response = Some (Protocol.LanguageServerProtocolResponse response); _ } ->
          response
      | _ ->
          failwith "Unexpected response."
    in
    let expected_response =
      expected_response
      |> Yojson.Safe.from_string
      |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  assert_response 0 {|{"jsonrpc":"2.0","id":0,"result":null}|};
  assert_response 2 {|{"jsonrpc":"2.0","id":2,"result":null}|}


let test_process_type_query_request _ =
  let assert_response request expected_response =
    let state = mock_server_state () in
    let actual_response =
      Request.process_type_query_request ~state ~request
      |> function
      | { Request.response = Some (Protocol.TypeQueryResponse response); _ } ->
          Protocol.TypeQuery.response_to_yojson response
          |> Yojson.Safe.to_string
      | _ ->
          failwith "Unexpected response."
    in
    let expected_response =
      expected_response
      |> Yojson.Safe.from_string
      |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in

  assert_response
    (Protocol.TypeQuery.Join (parse_single_access "int", parse_single_access "float"))
    {|{"response":{"type":"float"}}|};
  assert_response
    (Protocol.TypeQuery.NormalizeType (parse_single_access "yerp"))
    {|{"error":"Type `yerp` was not found in the type order."}|}


let assert_errors_equal ~actual_errors ~expected_errors =
  let actual_errors =
    let errors (handle, errors) =
      File.Handle.show handle,
      List.map errors ~f:(Analysis.Error.description ~detailed:false)
    in
    List.map actual_errors ~f:errors
  in
  let equal =
    let equal left right =
      String.equal (fst left) (fst right) &&
      List.equal
        (snd left |> List.sort ~compare:String.compare)
        (snd right |> List.sort ~compare:String.compare)
        ~equal:String.equal
    in
    List.equal ~equal
  in
  let printer errors =
    let show (path, errors) = Format.asprintf "%s: [%s]" path (String.concat errors ~sep:", ") in
    List.map errors ~f:show
    |> String.concat ~sep:"\n"
  in
  assert_equal ~cmp:equal ~printer expected_errors actual_errors


let test_process_display_type_errors_request _ =
  let assert_response ~paths ~errors ~expected_errors =
    let actual_errors =
      let state =
        let serialized_errors errors =
          let entry (path, errors) =
            let errors =
              let error undefined =
                let location = { Location.Instantiated.any with Location.path } in
                {
                  Analysis.Error.location;
                  kind = Analysis.Error.UndefinedName (Expression.Access.create undefined);
                  define = +mock_define;
                }
              in
              List.map errors ~f:error
            in
            File.Handle.create path, errors
          in
          List.map errors ~f:entry
        in
        mock_server_state
          ~errors:(serialized_errors errors |> File.Handle.Table.of_alist_exn)
          ()
      in
      let configuration =
        Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ()
      in
      let files = List.map paths ~f:(fun path -> mock_path path |> File.create) in
      Request.process_display_type_errors_request ~state ~configuration ~files
      |> function
      | { Request.response = Some (Protocol.TypeCheckResponse response); _ } -> response
      | _ -> failwith "Unexpected response."
    in
    let expected_errors =
      let expected_error (path, undefined_globals) =
        let undefined_global global =
          Format.asprintf
            "Undefined name [18]: Global name `%s` is undefined."
            global
        in
        path,
        List.map undefined_globals ~f:undefined_global
      in
      List.map expected_errors ~f:expected_error
    in
    assert_errors_equal ~actual_errors ~expected_errors
  in

  assert_response ~paths:[] ~errors:[] ~expected_errors:[];
  (* Empty request returns all errors. *)
  assert_response
    ~paths:[]
    ~errors:["one.py", ["one"]; "two.py", ["two"]]
    ~expected_errors:["one.py", ["one"]; "two.py", ["two"]];
  assert_response
    ~paths:["one.py"]
    ~errors:["one.py", ["one"]; "two.py", ["two"]]
    ~expected_errors:["one.py", ["one"]; "two.py", []]


let test_process_type_check_request context =
  let assert_response
      ?(sources = [])
      ~check
      ~expected_errors
      ?(expected_deferred_requests = [])
      () =
    let assert_response _ =
      let actual_errors, actual_deferred_requests =
        let configuration, state = initialize sources in
        let check = List.map check ~f:write_file in
        let request = { Protocol.TypeCheckRequest.update_environment_with = check; check } in
        Request.process_type_check_request ~state ~configuration ~request
        |> function
        | {
          Request.response = Some (Protocol.TypeCheckResponse response);
          state = { State.deferred_requests; _ };
        } ->
            let actual_deferred_requests =
              let deferred_request request =
                let open Protocol in
                match request with
                | Request.TypeCheckRequest {
                    TypeCheckRequest.check;
                    update_environment_with = [];
                  } ->
                    let path file =
                      File.handle file ~configuration
                      |> File.Handle.show
                    in
                    List.map check ~f:path
                | _ ->
                    failwith "Unable to extract deferred type-check request"
              in
              List.map deferred_requests ~f:deferred_request
              |> List.concat
            in
            response, actual_deferred_requests
        | _ ->
            failwith "Unexpected response."
      in
      assert_errors_equal ~actual_errors ~expected_errors;
      assert_equal
        ~cmp:(List.equal ~equal:String.equal)
        ~printer:(String.concat ~sep:"\n")
        expected_deferred_requests
        actual_deferred_requests
    in
    OUnit2.with_bracket_chdir context (OUnit2.bracket_tmpdir context) assert_response
  in

  assert_response ~check:[] ~expected_errors:[] ();
  assert_response
    ~check:[
      "test.py",
      {|
        def foo() -> int:
          return 'asdf'
      |};
    ]
    ~expected_errors:["test.py", ["Incompatible return type [7]: Expected `int` but got `str`."]]
    ();

  (* Check deferred requests for dependencies. *)
  assert_response
    ~sources:[
      "library.py", "def function() -> int: ...";
      "client.py", "from library import function";
    ]
    ~check:["library.py", "def function() -> int: ..."]  (* Unchanged. *)
    ~expected_errors:["library.py", []]
    ~expected_deferred_requests:[]
    ();
  (* Single dependency. *)
  assert_response
    ~sources:[
      "library.py", "def function() -> int: ...";
      "client.py", "from library import function";
    ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:["library.py", []]
    ~expected_deferred_requests:["client.py"]
    ();
  (* Multiple depedencies. *)
  assert_response
    ~sources:[
      "library.py", "def function() -> int: ...";
      "client.py", "from library import function";
      "other.py", "from library import function";
    ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:["library.py", []]
    ~expected_deferred_requests:["client.py"; "other.py"]
    ();
  (* Indirect dependency. *)
  assert_response
    ~sources:[
      "library.py", "def function() -> int: ...";
      "client.py",
      {|
        from library import function
        def function() -> int: ...
      |};
      "indirect.py", "from client import function"
    ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:["library.py", []]
    ~expected_deferred_requests:["client.py"]
    ()


let test_process_get_definition_request context =
  let temporary_directory = OUnit2.bracket_tmpdir context in
  let run_test _ =
    let assert_response ~sources ?filename ~line ~column response =
      let configuration, state = initialize sources in

      let position = { Location.line; column } in
      let request =
        let file =
          match filename with
          | Some valid_filename ->
              Path.create_relative
                ~relative:valid_filename
                ~root:(Path.current_working_directory ())
              |> File.create
          | _ ->
              (* Create a bogus filename entry. *)
              Path.create_relative
                ~relative:"bogusfile.py"
                ~root:(Path.create_absolute ~follow_symbolic_links:false "/bogus/dir")
              |> File.create
        in
        { Protocol.DefinitionRequest.id = 0; file; position }
      in
      let actual_response =
        let actual_response =
          Request.process_get_definition_request
            ~state
            ~configuration
            ~request
        in
        match actual_response with
        | { Request.response = Some (Protocol.LanguageServerProtocolResponse response); _ } ->
            begin
              Yojson.Safe.from_string response
            end
        | _ -> failwith "Unexpected response."
      in
      let expected_response =
        let open LanguageServer.Types in
        let result =
          let response_location
              {
                Ast.Location.path;
                start = { Ast.Location.line = start_line; column = start_column };
                stop = { Ast.Location.line = stop_line; column = stop_column };
              } =
            {
              (* Temporary paths are OS-dependent. *)
              Location.uri =
                Path.uri
                  (Path.create_relative
                     ~root:(Path.create_absolute temporary_directory)
                     ~relative:path);
              range =
                {
                  start = { Position.line = start_line; character = start_column };
                  end_ = { Position.line = stop_line; character = stop_column };
                }
            }
          in
          response
          >>| response_location
          |> Option.to_list
        in
        {
          TextDocumentDefinitionResponse.jsonrpc = "2.0";
          id = 0;
          result = Some result;
          error = None;
        }
        |> TextDocumentDefinitionResponse.to_yojson
      in
      let json_diff_printer format json =
        Yojson.Safe.pretty_to_string json
        |> Format.fprintf format "%s\n"
      in
      assert_equal
        ~printer:Yojson.Safe.pretty_to_string
        ~pp_diff:(diff ~print:json_diff_printer)
        expected_response
        actual_response
    in

    let sources =
      [
        "library.py", "def function() -> int: ...";
        "client.py",
        {|
        from library import function
        def foo() -> int:
          return function()
        |};
      ]
    in
    (* Invalid request for an invalid file (no exception raised). *)
    assert_response
      ~sources
      ~line:0
      ~column:0
      None;
    (* Invalid request for a valid file. *)
    assert_response
      ~sources
      ~filename:"client.py"
      ~line:0
      ~column:0
      None;
    (* Valid request for a valid file. *)
    assert_response
      ~sources
      ~filename:"client.py"
      ~line:4
      ~column:9
      (Some {
          Location.path = "library.py";
          start = { Location.line = 0; column = 0};
          stop = { Location.line = 0; column = 26};
        })
  in
  OUnit2.with_bracket_chdir context temporary_directory run_test


let () =
  "request">:::
  [
    "process_client_shutdown_request">::test_process_client_shutdown_request;
    "process_type_query_request">::test_process_type_query_request;
    "process_display_type_errors_request">::test_process_display_type_errors_request;
    "process_type_check_request">::test_process_type_check_request;
    "process_get_definition_request">::test_process_get_definition_request;
  ]
  |> Test.run
