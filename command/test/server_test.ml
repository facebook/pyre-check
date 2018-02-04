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
module State = ServerState
module Scheduler = Service.Scheduler


exception Timeout


let file ?(content = None) relative =
  let root = Path.current_working_directory () in
  File.create ~content (Path.create_relative ~root ~relative)

let poll_for_deletion lock_path =
  let rec poll () =
    if Path.file_exists lock_path then
      (Unix.nanosleep 0.1 |> ignore; poll ())
    else
      ()
  in
  poll ()


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
                  "Incompatible return type [7]: Expected `None` but got `int`. Type `None` expected on line 3, specified on line 2.",
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
  let { ServerConfiguration.lock_path; socket_path; _ } =
    ServerConfiguration.create (Configuration.create ())
  in
  with_timeout ~seconds:3 poll_for_deletion lock_path;
  with_timeout ~seconds:3 poll_for_deletion socket_path;
  Command_test.clean_environment ()


let test_server_stops _ =
  let pid = Pid.of_int (Command_test.start_server ()) in
  Command.run ~argv:["_"] Server.stop_command;
  Exn.protect
    ~f:(fun () ->
        with_timeout
          ~seconds:2
          (fun pid ->
             (match Unix.waitpid pid with
              | Ok _ -> assert true
              | Error _ -> assert false))
          pid)
    ~finally:Command_test.clean_environment


let test_stop_handles_unix_errors context =
  let long_path = bracket_tmpdir ~suffix:(String.init ~f:(fun _ -> 'a') 140) context in
  Command.run ~argv:["_"; long_path] Server.stop_command


let configuration = Configuration.create ()


let environment () =
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    (Environment.handler ~configuration environment)
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
  let configuration = Command_test.mock_analysis_configuration () in
  let source = Preprocessing.preprocess (parse ~path ~qualifier source) in
  let environment_handler = Environment.handler ~configuration (environment ()) in
  Environment.populate ~configuration (environment_handler) [source];
  (Analysis.TypeCheck.check configuration environment_handler source).TypeCheck.errors

let mock_server_state
    ?(initial_errors = Error.Hash_set.create ())
    ?(initial_environment = environment ())
    errors =
  let configuration = Configuration.create () in
  {
    State.deferred_requests = [];
    environment = Environment.handler ~configuration initial_environment;
    initial_errors;
    errors;
    handles = File.Handle.Set.empty;
    last_request_time = Unix.time ();
    lookups = String.Table.create ();
    lock = Mutex.create ();
    connections = ref {
        State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
        persistent_clients = Unix.File_descr.Table.create ();
        file_notifiers = [];
      };
    scheduler = Scheduler.create ~is_parallel:false ();
  }

let mock_client_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null"

let assert_request_gets_response
    ?(initial_errors = Error.Hash_set.create ())
    ?(source_root = Path.current_working_directory ())
    ?state
    ?(path = "test.py")
    source
    request
    expected_response =
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      (make_errors ~path source)
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
      (Command_test.mock_server_configuration ~source_root ())
      request
  in
  Scheduler.destroy mock_server_state.State.scheduler;
  Command_test.clean_environment ();
  assert_equal response expected_response


let test_shutdown _ =
  let source = "x = 1" in
  assert_request_gets_response
    source
    (Protocol.Request.ClientShutdownRequest 1)
    (Some (Protocol.LanguageServerProtocolResponse
             (LanguageServerProtocol.ShutdownResponse.default 1
              |> LanguageServerProtocol.ShutdownResponse.to_yojson
              |> Yojson.Safe.to_string)))


let test_language_scheduler_shutdown _ =
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
    (Some (Protocol.LanguageServerProtocolResponse
             (LanguageServerProtocol.ShutdownResponse.default 2
              |> LanguageServerProtocol.ShutdownResponse.to_yojson
              |> Yojson.Safe.to_string)))


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
    (Some (Protocol.TypeCheckResponse (associate_errors_and_filenames errors)));

  assert_request_gets_response
    ~initial_errors:(Error.Hash_set.of_list errors)
    source
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = true })
    (Some (Protocol.TypeCheckResponse (associate_errors_and_filenames errors)))


let test_query _ =
  let source = "a = 1" in
  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest (Protocol.LessOrEqual (Type.integer, Type.string)))
    (Some (Protocol.TypeQueryResponse "false"));

  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest
       (Protocol.LessOrEqual
          (Type.list (Type.Primitive (Identifier.create "C")),
           Type.list (Type.integer))))
    (Some (Protocol.TypeQueryResponse "true"));

  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest
       (Protocol.Join
          (Type.list (Type.Primitive (Identifier.create "C")),
           Type.list (Type.integer))))
    (Some (Protocol.TypeQueryResponse "`typing.List[int]`"));

  assert_request_gets_response
    source
    (Protocol.Request.TypeQueryRequest
       (Protocol.Meet
          (Type.list (Type.Primitive (Identifier.create "C")),
           Type.list (Type.integer))))
    (Some (Protocol.TypeQueryResponse "`typing.List[C]`"))


let test_connect _ =
  Command_test.start_server ~version:"A" () |> ignore;
  let { ServerConfiguration.configuration; lock_path; _ } =
    Command_test.mock_server_configuration ~version:"B" ()
  in
  (* This sleep ensures that the server doesn't receive an EPIPE while the Hack_parallel library is
   * iniitializing the daemon in the hack_parallel/utils/handle.ml. In that codepath, an external
   * routine is called, and due to the nature of the Lazy library this is non-reentrant. *)
  Unix.nanosleep 0.5
  |> ignore;
  let cleanup () =
    Server.stop "." ();
    with_timeout ~seconds:3 poll_for_deletion lock_path;
    Command_test.clean_environment ()
  in
  Exn.protect
    ~f:(fun () ->
        assert_raises
          (Server.VersionMismatch { Server.server_version = "A"; client_version = "B" })
          (fun () -> Server.connect ~retries:1 ~configuration))
    ~finally:cleanup


let test_incremental_typecheck _ =
  let path, _ =
    Filename.open_temp_file
      ~in_dir:(Path.current_working_directory () |> Path.absolute)
      "test" ".py"
  in
  let relative_path =
    Path.create_relative ~root:(Path.current_working_directory ()) ~relative:path
    |> Path.relative
    |> Option.value ~default:path
  in
  let source =
    {|
        def foo()-> None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let errors =
    associate_errors_and_filenames
      (make_errors ~path:relative_path ~qualifier:(Source.qualifier ~path:relative_path) source)
  in
  assert_request_gets_response
    source
    (Protocol.Request.TypeCheckRequest
       { Protocol.files = [file path]; check_dependents = true })
    (Some (Protocol.TypeCheckResponse [(File.Handle.create relative_path), []]));
  let request_with_content =
    (Protocol.Request.TypeCheckRequest
       { Protocol.files = [file ~content:(Some source) path]; check_dependents = true })
  in
  assert_request_gets_response
    ~path
    source
    request_with_content
    (Some (Protocol.TypeCheckResponse errors));
  let state = mock_server_state (File.Handle.Table.create ()) in
  assert_request_gets_response
    ~path
    source
    ~state
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = false })
    (Some (Protocol.TypeCheckResponse []));
  assert_request_gets_response
    ~path
    source
    ~state:{ state with State.deferred_requests = [request_with_content] }
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = false })
    (Some (Protocol.TypeCheckResponse errors))


let test_protocol_language_server_protocol _ =
  let server_state = mock_server_state (File.Handle.Table.create ()) in
  let _, response =
    ServerRequest.process_request
      mock_client_socket
      server_state
      (Command_test.mock_server_configuration ())
      (Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
  in
  let cleanup () =
    Scheduler.destroy server_state.State.scheduler;
    Command_test.clean_environment ()
  in
  Exn.protect ~f:(fun () -> assert_is_none response) ~finally:cleanup


let test_did_save_with_content context =
  let filename, _ = bracket_tmpfile ~suffix:".py" context in
  let source_root = "/tmp" in
  let filename =
    String.chop_prefix ~prefix:(source_root ^ "/") filename
    |> Option.value ~default:filename
  in
  let source =
    {|
        def foo()->None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let request =
    LanguageServerProtocol.DidSaveTextDocument.create
      ~root:(Path.create_absolute source_root)
      (source_root ^/ filename)
      (Some source)
    |> Or_error.ok_exn
    |> LanguageServerProtocol.DidSaveTextDocument.to_yojson
    |> Yojson.Safe.to_string
  in
  assert_request_gets_response
    ~source_root:(Path.create_absolute source_root)
    source
    (Protocol.Request.LanguageServerProtocolRequest request)
    None


let test_protocol_persistent _ =
  let server_state = mock_server_state (File.Handle.Table.create ()) in
  assert_raises
    ServerRequest.InvalidRequest
    (fun () ->
       ServerRequest.process_request
         mock_client_socket
         server_state
         (Command_test.mock_server_configuration ())
         (Protocol.Request.ClientConnectionRequest Protocol.Persistent));
  Scheduler.destroy server_state.State.scheduler;
  Command_test.clean_environment ()


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
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    (Environment.handler ~configuration environment) [
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
  Scheduler.destroy initial_state.State.scheduler;
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
  let path, _ =
    Filename.open_temp_file
      ~in_dir:(Path.current_working_directory () |> Path.absolute)
      "test" ".py"
  in
  let relative_path =
    Path.create_relative ~root:(Path.current_working_directory ()) ~relative:path
    |> Path.relative
    |> Option.value ~default:path
  in
  let parse content =
    Ast.Source.create
      ~path
      ~qualifier:(Source.qualifier ~path)
      (ParserParser.parse ~path (String.split_lines (content ^ "\n")))
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
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  Environment.populate
    ~configuration
    (Environment.handler ~configuration environment)
    [parse source];
  let request =
    Protocol.Request.TypeCheckRequest
      { Protocol.files = [file ~content:(Some source) path]; check_dependents = true }
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
          path = relative_path;
          position = { Ast.Location.line = 5; column = 4 };
        })
  in
  Scheduler.destroy initial_state.State.scheduler;
  Command_test.clean_environment ();
  assert_is_some response;
  let response = Option.value_exn response in
  assert_true (Hashtbl.mem state.State.lookups relative_path);
  let definition_map = Hashtbl.find_exn state.State.lookups relative_path in
  assert_equal (Hashtbl.length definition_map) 1;
  assert_equal
    response
    (Protocol.GetDefinitionResponse (Some {
         Ast.Location.start = { Ast.Location.line = 2; column = 0 };
         stop = { Ast.Location.line = 3; column = 12 };
         path = relative_path;
       }))


let test_incremental_repopulate _ =
  let parse content =
    Ast.Source.create
      ~path:"test.py"
      ~qualifier:(Source.qualifier ~path:"test.py")
      (ParserParser.parse ~path:"test.py" (String.split_lines (content ^ "\n")))
    |> Analysis.Preprocessing.preprocess
  in
  let source =
    {|
      def foo(x)->int:
          return 1
    |}
    |> trim_extra_indentation
  in
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  Out_channel.write_all ~data:source "test.py";
  Environment.populate
    ~configuration
    (Environment.handler ~configuration environment)
    [parse source];
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~initial_environment:environment
      errors
  in
  let get_annotation access_name =
    match Handler.function_definitions (Ast.Expression.Access.create access_name) with
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
  Scheduler.destroy initial_state.State.scheduler;
  Command_test.clean_environment ()


let test_language_scheduler_definition context =
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
    (Some (Protocol.LanguageServerProtocolResponse expected_response))


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
      "language_scheduler_shutdown", test_language_scheduler_shutdown;
      "did_save_with_content", test_did_save_with_content;
      "incremental_dependencies", test_incremental_dependencies;
      "incremental_typecheck", test_incremental_typecheck;
      "incremental_repopulate", test_incremental_repopulate;
      "incremental_lookups", test_incremental_lookups;
      "language_scheduler_definition", test_language_scheduler_definition;
      "language_server_protocol_json_format", test_language_server_protocol_json_format;
    ]
