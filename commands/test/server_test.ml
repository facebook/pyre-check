(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Test

module Parallel = Hack_parallel.Std
module Error = PyreError


exception Timeout


let file ?(content = None) relative =
  let root = Path.current_working_directory () in
  File.create ~content (Path.create_relative ~root ~relative)


let test_language_server_protocol_json_format context =
  let open TypeCheck.Error in
  let filename, _ = bracket_tmpfile ~suffix:".py" context in
  let type_error : TypeCheck.Error.t =
    Command_test.make_errors
      {|
        def foo() -> None:
          return 1
      |}
    |> List.hd_exn
  in
  let type_error =
    { type_error
      with location = { type_error.location with Location.path = filename }
    } in
  let json_error =
    LanguageServerProtocol.PublishDiagnostics.of_errors
      ~root:(Path.create_absolute "/tmp")
      (File.Handle.create filename)
      [type_error]
    |> Or_error.ok_exn
    |> LanguageServerProtocol.PublishDiagnostics.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.to_string
    |> Yojson.Safe.prettify
  in
  let json_error_expect =
    Format.sprintf
      {|
        {
          "jsonrpc": "2.0",
          "method": "textDocument/publishDiagnostics",
          "params": {
            "diagnostics": [
              {
                "message":
                  "Incompatible return type [7]: expected `None (void)` but got `int`. Type `None (void)` expected on line 3, specified on line 2.",
                "range": {
                  "end": { "character": 10, "line": 2 },
                  "start": { "character": 2, "line": 2 }
                },
                "severity": 1,
                "source": "Pyre"
              }
            ],
            "uri": "file://%s"
          }
        }
     |}
      filename
    |> Test.trim_extra_indentation
    |> String.strip
  in
  assert_equal
    ~printer:ident
    ~cmp:String.equal
    json_error_expect
    json_error


let with_timeout ~seconds f x =
  let timeout_option ~seconds f x =
    let signal = Signal.Expert.signal Signal.alrm (`Handle (fun _ -> raise Timeout)) in
    let cleanup () =
      let _ = Unix.alarm 0 in
      Signal.Expert.set Signal.alrm signal
    in
    try
      let _ = Unix.alarm seconds in
      let result = f x in
      cleanup ();
      Some result
    with
    | Timeout -> cleanup (); None
    | _ -> cleanup (); failwith "Timeout function failed" in
  match timeout_option ~seconds f x with
  | Some x -> x
  | None -> raise Timeout


let test_server_exists _ =
  (* Ideally we could just call `Command.run PyreServer.command` and catch the
     failure. But Janestreet library decides to catch all exceptions on
     Command.run so scratch that. *)
  Command_test.start_server () |> ignore;
  (* Clean up: Kill server *)
  Command.run ~argv:["_"] Server.stop_command;
  let { ServerConfiguration.lock_path; _ } = ServerConfiguration.create (Configuration.create ()) in
  let rec poll_for_deletion () =
    if Path.file_exists lock_path then
      (Unix.nanosleep 0.1 |> ignore; poll_for_deletion ())
    else
      ()
  in
  with_timeout ~seconds:3 poll_for_deletion ();
  Command_test.clean_environment ()


let test_server_stops _ =
  let pid = Pid.of_int (Command_test.start_server ()) in
  Command.run ~argv:["_"] Server.stop_command;
  (try
     with_timeout
       ~seconds:2
       (fun pid ->
          (match Unix.waitpid pid with
           | Ok _ -> assert true
           | Error _ -> assert false))
       pid
   with
   | Timeout -> failwith "Timed out while waiting for server to be killed"
   | _ -> failwith "Timeout error");
  Command_test.clean_environment ()


let test_stop_handles_unix_errors context =
  let long_path = bracket_tmpdir ~suffix:(String.init ~f:(fun _ -> 'a') 140) context in
  Command.run ~argv:["_"; long_path] Server.stop_command


let environment () =
  let environment = Environment.Builder.create () in
  Environment.populate
    (Environment.reader environment)
    [
      parse {|
        class int(float): pass
        class str: pass
        class typing.Generic(object): pass
        _T = TypeVar("_T")
        class list(typing.Generic[_T]): pass
        class C(int): pass
      |}
    ];
  environment


let associate_errors_and_filenames error_list =
  let error_file ({ Error.location = { Ast.Location.path; _ }; _ } as error) =
    File.Handle.create path, error
  in
  List.map ~f:error_file error_list
  |> (List.fold
        ~init:File.Handle.Map.empty
        ~f:(fun map (handle, error) -> Map.add_multi map ~key:handle ~data:error))
  |> Map.to_alist


let make_errors ?(path = "test.py") ?(qualifier = []) source =
  let source = Preprocessing.preprocess (parse ~path ~qualifier source) in
  let environment_reader = Environment.reader (environment ()) in
  Environment.populate (environment_reader) [source];
  let configuration = Command_test.mock_analysis_configuration () in
  (Analysis.TypeCheck.check configuration environment_reader source).TypeCheck.errors

let mock_server_state
    ?(initial_errors = Error.Hash_set.create ())
    ?(initial_environment = environment ())
    errors =
  {
    State.deferred_requests = [];
    environment = Environment.reader initial_environment;
    initial_errors;
    errors;
    handles = File.Handle.Set.empty;
    lookups = String.Table.create ();
    lock = Mutex.create ();
    connections = ref {
        State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
        persistent_clients = Unix.File_descr.Table.create ();
        file_notifiers = [];
      };
    service = Service.create ~is_parallel:false ();
  }

let mock_client_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null"

let assert_request_gets_response
    ?(initial_errors = Error.Hash_set.create ())
    ?(project_root = Path.current_working_directory ())
    ?state
    source
    request
    expected_response =
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      (make_errors source)
      ~f:(fun error ->
          let { Ast.Location.path; _ } = Error.location error in
          Hashtbl.add_multi errors ~key:(File.Handle.create path) ~data:error);
    errors
  in
  let mock_server_state =
    match state with
    | Some state -> state
    | None -> mock_server_state ~initial_errors errors
  in
  let _, response =
    ServerRequest.process_request
      mock_client_socket
      mock_server_state
      (Command_test.mock_server_configuration ~project_root ())
      request
  in
  assert_is_some response;
  let response = Option.value_exn response in
  Service.destroy mock_server_state.State.service;
  Command_test.clean_environment ();
  assert_equal ~printer:Protocol.show_response response expected_response


let test_shutdown _ =
  let source = "x = 1" in
  assert_request_gets_response
    source
    (Protocol.Request.ClientShutdownRequest 1)
    (Protocol.LanguageServerProtocolResponse
       (LanguageServerProtocol.ShutdownResponse.default 1
        |> LanguageServerProtocol.ShutdownResponse.to_yojson
        |> Yojson.Safe.to_string))


let test_language_service_shutdown _ =
  let source = "x = 1" in
  assert_request_gets_response
    source
    (Protocol.Request.LanguageServerProtocolRequest
       {|
        {
           "jsonrpc": "2.0",
           "id": 2,
           "method": "shutdown",
           "params": null
        }
       |})
    (Protocol.LanguageServerProtocolResponse
       (LanguageServerProtocol.ShutdownResponse.default 2
        |> LanguageServerProtocol.ShutdownResponse.to_yojson
        |> Yojson.Safe.to_string))


let test_protocol_type_check _ =
  let source =
    {|
        def foo() -> None:
          return 1
    |}
  in
  let errors = make_errors source in
  assert_request_gets_response
    source
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = true })
    (Protocol.TypeCheckResponse (associate_errors_and_filenames errors));

  assert_request_gets_response
    ~initial_errors:(Error.Hash_set.of_list errors)
    source
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = true })
    (Protocol.TypeCheckResponse (associate_errors_and_filenames errors))


let test_query _ =
  let source = "a = 1" in
  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest (Protocol.LessOrEqual (Type.integer, Type.string)))
    (Protocol.TypeQueryResponse "false");

  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest
       (Protocol.LessOrEqual
          (Type.list (Type.Primitive (Identifier.create "C")),
           Type.list (Type.integer))))
    (Protocol.TypeQueryResponse "true");

  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest
       (Protocol.Join
          (Type.list (Type.Primitive (Identifier.create "C")),
           Type.list (Type.integer))))
    (Protocol.TypeQueryResponse "`typing.List[int]`");

  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest
       (Protocol.Meet
          (Type.list (Type.Primitive (Identifier.create "C")),
           Type.list (Type.integer))))
    (Protocol.TypeQueryResponse "`typing.List[C]`")


let test_connect _ =
  Command_test.start_server ~version:"A" () |> ignore;
  let { ServerConfiguration.configuration; _ } =
    Command_test.mock_server_configuration ~version:"B" ()
  in
  assert_raises
    (Server.VersionMismatch { Server.server_version = "A"; client_version = "B" })
    (fun () -> Server.connect ~retries:1 ~configuration);
  Server.stop "." ();
  Command_test.clean_environment ()


let test_incremental_typecheck _ =
  let source =
    {|
        def foo()-> None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let errors =
    associate_errors_and_filenames
      (make_errors ~qualifier:(Source.qualifier ~path:"test.py") source)
  in
  assert_request_gets_response
    source
    (Protocol.Request.TypeCheckRequest
       { Protocol.files = [file "test.py"]; check_dependents = true })
    (Protocol.TypeCheckResponse [(File.Handle.create "test.py"), []]);
  let request_with_content =
    (Protocol.Request.TypeCheckRequest
       { Protocol.files = [file ~content:(Some source) "test.py"]; check_dependents = true })
  in
  assert_request_gets_response
    source
    request_with_content
    (Protocol.TypeCheckResponse errors);

  let state = mock_server_state (File.Handle.Table.create ()) in
  assert_request_gets_response
    source
    ~state
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = false })
    (Protocol.TypeCheckResponse []);
  assert_request_gets_response
    source
    ~state:{ state with State.deferred_requests = [request_with_content] }
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = false })
    (Protocol.TypeCheckResponse errors)


let test_protocol_language_server_protocol _ =
  let server_state = mock_server_state (File.Handle.Table.create ()) in
  let _, response =
    ServerRequest.process_request
      mock_client_socket
      server_state
      (Command_test.mock_server_configuration ())
      (Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
  in
  assert_is_none response;
  Service.destroy server_state.State.service;
  Command_test.clean_environment ()


let test_did_save_with_content context =
  let filename, _ = bracket_tmpfile ~suffix:".py" context in
  let project_root = "/tmp" in
  let filename =
    String.chop_prefix ~prefix:(project_root ^ "/") filename
    |> Option.value ~default:filename
  in
  let source =
    {|
        def foo()->None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let errors =
    make_errors
      ~path:filename
      ~qualifier:(Source.qualifier ~path:filename) source in
  let request =
    LanguageServerProtocol.DidSaveTextDocument.create
      ~root:(Path.create_absolute project_root)
      (project_root ^/ filename)
      (Some source)
    |> Or_error.ok_exn
    |> LanguageServerProtocol.DidSaveTextDocument.to_yojson
    |> Yojson.Safe.to_string
  in
  assert_request_gets_response
    ~project_root:(Path.create_absolute project_root)
    source
    (Protocol.Request.LanguageServerProtocolRequest request)
    (Protocol.TypeCheckResponse (associate_errors_and_filenames errors))

let test_protocol_persistent _ =
  let source =
    {|
        def foo() -> None:
          return 1
    |}
  in
  assert_request_gets_response
    source
    (Protocol.Request.ClientConnectionRequest Protocol.Persistent)
    (Protocol.ClientConnectionResponse Protocol.Persistent)


let test_incremental_dependencies _ =

  let a_source = {|
    import b
    def foo()->str:
      return b.do()
    |} |> trim_extra_indentation
  in
  let b_source = {|
      def do()->str:
          pass
    |} |> trim_extra_indentation
  in

  Out_channel.write_all "a.py" ~data:a_source;
  Out_channel.write_all "b.py" ~data:b_source;
  let environment = Environment.Builder.create () in
  Environment.populate (Environment.reader environment) [
    parse ~path:"a.py" a_source;
    parse ~path:"b.py" b_source;
  ];
  let expected_errors = [
    File.Handle.create "b.py", [];
  ]
  in
  let initial_state =
    mock_server_state ~initial_environment:environment (File.Handle.Table.create ()) in
  let state, response = ServerRequest.process_request
      mock_client_socket
      initial_state
      (Command_test.mock_server_configuration ())
      (Protocol.Request.TypeCheckRequest
         { Protocol.files = [file "b.py"]; check_dependents = true })
  in
  Sys.remove "a.py";
  Sys.remove "b.py";
  Service.destroy initial_state.State.service;
  Command_test.clean_environment ();
  assert_is_some response;
  assert_equal
    ~printer:Protocol.show_response
    (Protocol.TypeCheckResponse expected_errors)
    (Option.value_exn response);
  assert_equal
    state.State.deferred_requests
    [Protocol.Request.TypeCheckRequest { Protocol.files = [file "a.py"]; check_dependents = false }]


let test_incremental_lookups _ =
  let parse content =
    Ast.Source.create
      ~path:"test.py"
      ~qualifier:(Source.qualifier ~path:"test.py")
      (PythonParse.parse ~path:"test.py" (String.split_lines (content ^ "\n")))
    |> Analysis.Preprocessing.preprocess
  in
  let source =
    {|
      def foo(x):
          return 1
      def boo(x):
          foo(x)
          return 2
    |}
    |> trim_extra_indentation
  in
  let environment = Environment.Builder.create () in
  let (module Reader: Environment.Reader) = Environment.reader environment in
  Environment.populate (Environment.reader environment) [parse source];
  let request =
    Protocol.Request.TypeCheckRequest
      { Protocol.files = [file ~content:(Some source) "test.py"]; check_dependents = true }
  in
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~initial_environment:environment
      errors
  in
  let state, _ =
    ServerRequest.process_request
      mock_client_socket
      initial_state
      (Command_test.mock_server_configuration ())
      request
  in
  let _, response =
    ServerRequest.process_request
      mock_client_socket
      state
      (Command_test.mock_server_configuration ())
      (Protocol.Request.GetDefinitionRequest {
          Protocol.DefinitionRequest.id = 1;
          path = "test.py";
          position = { Ast.Location.line = 5; column = 4 };
        })
  in
  Service.destroy initial_state.State.service;
  Command_test.clean_environment ();
  assert_is_some response;
  let response = Option.value_exn response in
  assert_true (Hashtbl.mem state.State.lookups "test.py");
  let definition_map = Hashtbl.find_exn state.State.lookups "test.py" in
  assert_equal (Hashtbl.length definition_map) 1;
  assert_equal
    response
    (Protocol.GetDefinitionResponse (Some {
         Ast.Location.start = { Ast.Location.line = 2; column = 0 };
         stop = { Ast.Location.line = 3; column = 12 };
         path = "test.py";
       }))


let test_incremental_repopulate _ =
  let parse content =
    Ast.Source.create
      ~path:"test.py"
      ~qualifier:(Source.qualifier ~path:"test.py")
      (PythonParse.parse ~path:"test.py" (String.split_lines (content ^ "\n")))
    |> Analysis.Preprocessing.preprocess
  in
  let source =
    {|
      def foo(x)->int:
          return 1
    |}
    |> trim_extra_indentation
  in
  let environment = Environment.Builder.create () in
  let (module Reader: Environment.Reader) = Environment.reader environment in
  Out_channel.write_all ~data:source "test.py";
  Environment.populate (Environment.reader environment) [parse source];
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~initial_environment:environment
      errors
  in
  let get_annotation access_name =
    match Reader.function_definitions (Ast.Instantiated.Access.create access_name) with
    | Some [ { Ast.Node.value = { Ast.Statement.Define.return_annotation; _ }; _ } ] ->
        return_annotation
    | _ -> None
  in
  begin match (get_annotation "test.foo") with
    | Some expression -> assert_equal (Ast.Expression.show expression) "int"
    | None -> assert_unreached ()
  end;
  let source =
    {|
      def foo(x)->str:
        return ""
    |}
    |> trim_extra_indentation
  in
  Out_channel.write_all ~data:source "test.py";
  let _, _ =
    ServerRequest.process_request
      mock_client_socket
      initial_state
      (Command_test.mock_server_configuration ())
      (Protocol.Request.TypeCheckRequest
         { Protocol.files = [file "test.py"]; check_dependents = true })
  in
  Sys.remove "test.py";
  begin match (get_annotation "test.foo") with
    | Some expression -> assert_equal (Ast.Expression.show expression) "str"
    | None -> assert_unreached ()
  end;
  Service.destroy initial_state.State.service;
  Command_test.clean_environment ()


let test_language_service_definition context =
  let filename, _ = bracket_tmpfile ~suffix:".py" context in
  let request =
    Format.sprintf {|
      {
        "jsonrpc": "2.0",
        "method": "textDocument/definition",
        "id": 3,
        "params": {
          "textDocument": {
            "uri": "file://%s"
          },
          "position": {
            "line": 5,
            "character": 7
          }
        }
      }
      |}
      filename
  in
  let expected_response =
    LanguageServerProtocol.TextDocumentDefinitionResponse.create
      ~root:(Path.current_working_directory ())
      ~id:3
      ~location:None
    |> LanguageServerProtocol.TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.to_string
  in
  assert_request_gets_response
    "a = 1"
    (Protocol.Request.LanguageServerProtocolRequest request)
    (Protocol.LanguageServerProtocolResponse expected_response)


let () =
  Command_test.run_command_tests
    "server"
    [
      "server_exists", test_server_exists;
      "server_stops", test_server_stops;
      "connect", test_connect;
      "stop_handles_unix_errors", test_stop_handles_unix_errors;
      "protocol_type_check", test_protocol_type_check;
      "protocol_language_server_protocol", test_protocol_language_server_protocol;
      "protocol_persistent", test_protocol_persistent;
      "query", test_query;
      "shutdown", test_shutdown;
      "language_service_shutdown", test_language_service_shutdown;
      "did_save_with_content", test_did_save_with_content;
      "incremental_dependencies", test_incremental_dependencies;
      "incremental_typecheck", test_incremental_typecheck;
      "incremental_repopulate", test_incremental_repopulate;
      "incremental_lookups", test_incremental_lookups;
      "language_service_definition", test_language_service_definition;
      "language_server_protocol_json_format", test_language_server_protocol_json_format;
    ]
