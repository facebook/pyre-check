(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Expression
open Pyre
open PyreParser
open Server
open Test

module Parallel = Hack_parallel.Std
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
  let ({ Error.location; _ } as type_error) =
    CommandTest.make_errors
      {|
        class unittest.mock.Base: ...
        class unittest.mock.NonCallableMock: ...
        def foo() -> None:
          return 1
      |}
    |> List.hd_exn
  in
  let type_error =
    { type_error with location = { location with Location.path = filename } }
  in
  let normalize string =
    (* Working around OS inconsitencies. *)
    string
    |> String.split ~on:'\n'
    |> String.concat
    |> String.filter ~f:(fun character -> not (Char.is_whitespace character))
  in
  let json_error =
    LanguageServer.Protocol.PublishDiagnostics.of_errors
      ~root:(Path.create_absolute Filename.temp_dir_name)
      (File.Handle.create filename)
      [type_error]
    |> Or_error.ok_exn
    |> LanguageServer.Protocol.PublishDiagnostics.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.to_string
    |> normalize
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
                  "Incompatible return type [7]: Expected `None` but got `int`. Type `None` expected on line 5, specified on line 4.",
                "range": {
                  "end": { "character": 10, "line": 4 },
                  "start": { "character": 2, "line": 4 }
                },
                "severity": 1,
                "source": "Pyre"
              }
            ],
            "uri":
              "file://%s"
          }
        }
     |}
      filename
    |> Test.trim_extra_indentation
    |> normalize
  in
  assert_equal ~printer:ident ~cmp:String.equal json_error_expect json_error


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


let test_server_stops _ =
  let pid = Pid.of_int (CommandTest.start_server ()) in
  Command.run ~argv:["_"; "-graceful"] Commands.Server.stop_command;
  let { ServerConfiguration.lock_path; socket_path; _ } =
    ServerConfiguration.create (Configuration.create ())
  in
  with_timeout ~seconds:3 poll_for_deletion lock_path;
  with_timeout ~seconds:3 poll_for_deletion socket_path;
  Exn.protect
    ~f:(fun () ->
        with_timeout
          ~seconds:1
          (fun pid ->
             match Unix.waitpid pid with
             | Ok _ -> assert true
             | Error _ -> assert false)
          pid)
    ~finally:CommandTest.clean_environment


let test_server_exits_on_directory_removal context =
  let directory = bracket_tmpdir context in
  let pid =
    Pid.of_int (CommandTest.start_server ~local_root:(Path.create_absolute directory) ())
  in
  Sys_utils.rm_dir_tree directory;
  Exn.protect
    ~f:(fun () ->
        with_timeout ~seconds:6 (fun () ->
            match Unix.waitpid pid with
            | Ok _
            (* I was only able to get non-zero exits in the OUnit test environment,
                doing the equivalent calls in the command line always resulted in an exit of 0. *)
            | Error (`Exit_non_zero 2) -> assert true
            | _ -> assert false
          ))
    ~finally:CommandTest.clean_environment
    ()


let test_stop_handles_unix_errors context =
  let long_path = bracket_tmpdir ~suffix:(String.init ~f:(fun _ -> 'a') 140) context in
  Command.run ~argv:["_"; "-graceful"; long_path] Commands.Server.stop_command


let configuration = Configuration.create ~infer:true ()


let environment () =
  let environment = Environment.Builder.create () in
  Service.Environment.populate
    (Environment.handler ~configuration environment)
    [
      parse {|
        class int(float): pass
        class str: pass
        class typing.Generic(object): pass
        _T = TypeVar("_T")
        class list(typing.Generic[_T]): pass
      |}
    ];
  environment


let associate_errors_and_filenames error_list =
  let error_file error =
    File.Handle.create (Error.path error), error
  in
  List.map ~f:error_file error_list
  |> (List.fold
        ~init:File.Handle.Map.empty
        ~f:(fun map (handle, error) -> Map.add_multi map ~key:handle ~data:error))
  |> Map.to_alist


let make_errors ?(path = "test.py") ?(qualifier = []) source =
  let configuration = CommandTest.mock_analysis_configuration () in
  let source = Preprocessing.preprocess (parse ~path ~qualifier source) in
  let environment_handler = Environment.handler ~configuration (environment ()) in
  add_defaults_to_environment environment_handler;
  Service.Environment.populate (environment_handler) [source];
  (TypeCheck.check configuration environment_handler source).TypeCheck.Result.errors

let mock_server_state
    ?(initial_errors = Error.Hash_set.create ())
    ?(initial_environment = environment ())
    errors =
  let environment = Environment.handler ~configuration initial_environment in
  add_defaults_to_environment environment;
  {
    State.deferred_requests = [];
    environment;
    initial_errors;
    errors;
    handles = File.Handle.Set.empty;
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

let mock_client_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null"

let assert_response
    ?(initial_errors = Error.Hash_set.create ())
    ?(local_root = Path.current_working_directory ())
    ?state
    ?(path = "test.py")
    ~source
    ~request
    expected_response =
  Ast.SharedMemory.remove_paths [File.Handle.create path];
  let parsed = parse ~path source in
  Ast.SharedMemory.add_source (File.Handle.create path) parsed;
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      (make_errors ~path source)
      ~f:(fun error ->
          Hashtbl.add_multi errors ~key:(File.Handle.create (Error.path error)) ~data:error);
    errors
  in
  let initial_environment =
    let environment = environment () in
    Service.Environment.populate (Environment.handler ~configuration environment) [parsed];
    environment
  in
  let mock_server_state =
    match state with
    | Some state -> state
    | None -> mock_server_state ~initial_environment ~initial_errors errors
  in
  let _, response =
    Request.process_request
      mock_client_socket
      mock_server_state
      (CommandTest.mock_server_configuration ~local_root ())
      request
  in
  CommandTest.clean_environment ();
  let printer = function
    | None -> "None"
    | Some response -> Protocol.show_response response
  in
  let pp_opt formatter =
    function
    | None -> Format.pp_print_string formatter "None"
    | Some response -> Protocol.pp_response formatter response
  in
  assert_equal ~pp_diff:(diff ~print:pp_opt) ~printer expected_response response


let test_shutdown _ =
  let source = "x = 1" in
  assert_response
    ~source
    ~request:(Protocol.Request.ClientShutdownRequest 1)
    (Some
       (Protocol.LanguageServerProtocolResponse
          (LanguageServer.Protocol.ShutdownResponse.default 1
           |> LanguageServer.Protocol.ShutdownResponse.to_yojson
           |> Yojson.Safe.to_string)))


let test_language_scheduler_shutdown _ =
  let source = "x = 1" in
  assert_response
    ~source
    ~request:(Protocol.Request.LanguageServerProtocolRequest
                {|
        {
           "jsonrpc": "2.0",
           "id": 2,
           "method": "shutdown",
           "params": null
        }
       |})
    (Some (Protocol.LanguageServerProtocolResponse
             (LanguageServer.Protocol.ShutdownResponse.default 2
              |> LanguageServer.Protocol.ShutdownResponse.to_yojson
              |> Yojson.Safe.to_string)))


let test_protocol_type_check _ =
  let source =
    {|
        def foo() -> None:
          return 1
    |}
  in
  let errors = make_errors source in
  assert_response
    ~source
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Some (Protocol.TypeCheckResponse (associate_errors_and_filenames errors)));

  assert_response
    ~initial_errors:(Error.Hash_set.of_list errors)
    ~source
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Some (Protocol.TypeCheckResponse (associate_errors_and_filenames errors)))


let test_query _ =
  let assert_type_query_response ~source ~query response =
    let query = Commands.Query.parse_query ~root:(Path.current_working_directory ()) query in
    match query with
    | None ->
        assert_unreached ()
    | Some request ->
        assert_response ~source ~request (Some (Protocol.TypeQueryResponse response))
  in
  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, str)"
    (Protocol.TypeQuery.Response "false");

  assert_type_query_response
    ~source:
      {|
        A = int
      |}
    ~query:"less_or_equal(int, A)"
    (Protocol.TypeQuery.Response "true");

  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");

  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"less_or_equal(list[C], list[int])"
    (Protocol.TypeQuery.Response "true");
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"join(list[C], list[int])"
    (Protocol.TypeQuery.Response "typing.List[int]");
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"meet(list[C], list[int])"
    (Protocol.TypeQuery.Response "typing.List[C]");
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"superclasses(C)"
    (Protocol.TypeQuery.Response "int");

  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");

  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown[int])"
    (Protocol.TypeQuery.Error "No class definition found for Unknown[int]");

  assert_type_query_response
    ~source:"A = int"
    ~query:"normalizeType(A)"
    (Protocol.TypeQuery.Response "int");

  assert_type_query_response
    ~source:{|
      class C:
        def C.foo(self) -> int: ...
        def C.bar(self) -> str: ...
    |}
    ~query:"methods(C)"
    (Protocol.TypeQuery.Response "foo: (self) -> int\nbar: (self) -> str");

  assert_type_query_response
    ~source:""
    ~query:"methods(Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");

  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_location(test.py, 1, 4)"
    (Protocol.TypeQuery.Response "int");
  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_location(test.py, 1, 3)"
    (Protocol.TypeQuery.Error "Not able to get lookup at test.py:1:3");

  assert_type_query_response
    ~source:{|
      class C:
        C.x = 1
        C.y = ""
        def C.foo() -> int: ...
    |}
    ~query:"attributes(C)"
    (Protocol.TypeQuery.Response "foo: typing.Callable(C.foo)[[], int]\nx: int\ny: str")


let test_connect _ =
  CommandTest.start_server ~expected_version:"A" () |> ignore;
  let { ServerConfiguration.configuration; lock_path; _ } =
    CommandTest.mock_server_configuration ~expected_version:"B" ()
  in
  (* This sleep ensures that the server doesn't receive an EPIPE while the Hack_parallel library is
   * iniitializing the daemon in the hack_parallel/utils/handle.ml. In that codepath, an external
   * routine is called, and due to the nature of the Lazy library this is non-reentrant. *)
  Unix.nanosleep 0.5
  |> ignore;
  let cleanup () =
    Commands.Server.stop ~graceful:true "." ();
    with_timeout ~seconds:3 poll_for_deletion lock_path;
    CommandTest.clean_environment ()
  in
  Exn.protect
    ~f:(fun () ->
        assert_raises
          (Server.Operations.VersionMismatch {
              Server.Operations.server_version = "A";
              expected_version = "B";
            })
          (fun () -> Server.Operations.connect ~retries:1 ~configuration))
    ~finally:cleanup


let test_incremental_typecheck _ =
  let path_to_python_file prefix =
    Filename.open_temp_file
      ~in_dir:(Path.current_working_directory () |> Path.absolute)
      prefix ".py"
    |> fst
  in
  let path = path_to_python_file "test" in
  let stub_path = path_to_python_file "stub" in
  Out_channel.write_all (stub_path ^ "i") ~data:"";

  let relativize path =
    Path.create_relative ~root:(Path.current_working_directory ()) ~relative:path
    |> Path.relative
    |> Option.value ~default:path
  in
  let relative_path = relativize path in
  let relative_stub_path = relativize stub_path in
  let source =
    {|
        def foo() -> None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let assert_response ?state ~request response =
    assert_response ~path ~source ?state ~request (Some response)
  in
  assert_response
    ~request:(Protocol.Request.TypeCheckRequest
                (Protocol.TypeCheckRequest.create
                   ~update_environment_with:[file path]
                   ~check:[file path]
                   ()))
    (Protocol.TypeCheckResponse [(File.Handle.create relative_path), []]);
  let files = [file ~content:(Some source) path] in
  let request_with_content =
    (Protocol.Request.TypeCheckRequest
       (Protocol.TypeCheckRequest.create ~update_environment_with:files ~check:files ()))
  in
  let errors =
    associate_errors_and_filenames
      (make_errors ~path:relative_path ~qualifier:(Source.qualifier ~path:relative_path) source)
  in
  assert_response ~request:request_with_content (Protocol.TypeCheckResponse errors);
  (* Assert that only files getting used to update the environment get parsed. *)
  let open Protocol in
  let check_request ?(update_environment_with = []) ?(check = []) () =
    Request.TypeCheckRequest
      (TypeCheckRequest.create
         ~update_environment_with
         ~check
         ())
  in
  assert_response
    ~request:(check_request ~check:[file ~content:(Some "def foo() -> int: return 1") path] ())
    (Protocol.TypeCheckResponse errors);
  let () =
    let stub_file = file ~content:(Some "") (stub_path ^ "i") in
    assert_response
      ~request:(check_request ~update_environment_with:[stub_file] ())
      (Protocol.TypeCheckResponse [])
  in
  assert_response
    ~request:(
      check_request
        ~check:[file ~content:(Some "def foo() -> int: return \"\"") stub_path]
        ())
    (Protocol.TypeCheckResponse [File.Handle.create relative_stub_path, []]);

  let () =
    let file = file ~content:(Some "def foo() -> int: return 1") path in
    assert_response
      ~request:(check_request ~update_environment_with:[file] ~check:[file] ())
      (Protocol.TypeCheckResponse [File.Handle.create relative_path, []])
  in

  let state = mock_server_state (File.Handle.Table.create ()) in
  assert_response
    ~state
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Protocol.TypeCheckResponse []);
  assert_response
    ~state:{ state with State.deferred_requests = [request_with_content] }
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Protocol.TypeCheckResponse errors)


let test_protocol_language_server_protocol _ =
  let server_state = mock_server_state (File.Handle.Table.create ()) in
  let _, response =
    Request.process_request
      mock_client_socket
      server_state
      (CommandTest.mock_server_configuration ())
      (Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
  in
  let cleanup () =
    CommandTest.clean_environment ()
  in
  Exn.protect ~f:(fun () -> assert_is_none response) ~finally:cleanup


let test_did_save_with_content context =
  let root, filename =
    let root = Path.create_absolute Filename.temp_dir_name in
    let filename =
      bracket_tmpfile ~suffix:".py" context
      |> fst
      |> Path.create_absolute
      |> (fun path -> Path.get_relative_to_root ~root ~path)
      |> (fun relative -> Option.value_exn relative)
    in
    root, filename
  in
  let source =
    {|
        def foo()->None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let qualifier =
    String.chop_suffix_exn ~suffix:".py" filename
    |> Access.create
  in
  let errors = make_errors ~path:filename ~qualifier source in
  let request =
    LanguageServer.Protocol.DidSaveTextDocument.create ~root filename (Some source)
    |> Or_error.ok_exn
    |> LanguageServer.Protocol.DidSaveTextDocument.to_yojson
    |> Yojson.Safe.to_string
  in
  assert_response
    ~local_root:root
    ~source
    ~request:(Protocol.Request.LanguageServerProtocolRequest request)
    (Some (Protocol.TypeCheckResponse (associate_errors_and_filenames errors)))


let test_protocol_persistent _ =
  let server_state = mock_server_state (File.Handle.Table.create ()) in
  assert_raises
    Request.InvalidRequest
    (fun () ->
       Request.process_request
         mock_client_socket
         server_state
         (CommandTest.mock_server_configuration ())
         (Protocol.Request.ClientConnectionRequest Protocol.Persistent));
  CommandTest.clean_environment ()


let test_incremental_dependencies _ =
  let a_source =
    {|
      import b
      def foo() -> str:
        return b.do()
    |}
    |> trim_extra_indentation
  in
  let b_source =
    {|
      def do() -> str:
          pass
    |}
    |> trim_extra_indentation
  in
  Out_channel.write_all "a.py" ~data:a_source;
  Out_channel.write_all "b.py" ~data:b_source;
  let assert_dependencies_analyzed () =
    let environment = Environment.Builder.create () in
    let environment_handler = Environment.handler ~configuration environment in
    add_defaults_to_environment environment_handler;
    Service.Environment.populate
      environment_handler
      [
        parse ~path:"a.py" ~qualifier:(Access.create "a") a_source;
        parse ~path:"b.py" ~qualifier:(Access.create "b") b_source;
      ];
    let expected_errors = [
      File.Handle.create "b.py", [];
    ]
    in
    let initial_state =
      mock_server_state ~initial_environment:environment (File.Handle.Table.create ()) in
    let check_request ?update ?check () =
      Protocol.Request.TypeCheckRequest
        (Protocol.TypeCheckRequest.create
           ?update_environment_with:update
           ?check
           ())
    in
    let process_request request =
      Request.process_request
        mock_client_socket
        initial_state
        (CommandTest.mock_server_configuration ())
        request
    in
    let state, response =
      process_request (check_request ~update:[file "b.py"] ~check:[file "b.py"] ())
    in
    assert_equal (Some (Protocol.TypeCheckResponse expected_errors)) response;
    assert_equal state.State.deferred_requests [check_request ~check:[file "a.py"] ()];
    let state, response =
      process_request (check_request ~update:[file "b.py"] ~check:[file "a.py"; file "b.py"] ())
    in
    let printer = function
      | None -> "None"
      | Some response -> Protocol.show_response response
    in
    assert_equal
      ~printer
      (Some (Protocol.TypeCheckResponse [
           File.Handle.create "a.py", [];
           File.Handle.create "b.py", [];
         ]))
      response;
    assert_equal
      ~printer:(List.to_string ~f:(Protocol.Request.show))
      state.State.deferred_requests
      []
  in
  let finally () =
    CommandTest.clean_environment ();
    Sys.remove "a.py";
    Sys.remove "b.py"
  in
  Exn.protect ~f:assert_dependencies_analyzed ~finally


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
    Source.create
      ~path
      ~qualifier:(Source.qualifier ~path)
      (Parser.parse ~path (String.split_lines (content ^ "\n")))
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
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let environment_handler = Environment.handler ~configuration environment in
  add_defaults_to_environment environment_handler;
  Service.Environment.populate environment_handler [parse source];
  let request =
    Protocol.Request.TypeCheckRequest
      (Protocol.TypeCheckRequest.create
         ~update_environment_with:[file ~content:(Some source) path]
         ~check:[file ~content:(Some source) path]
         ())
  in
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~initial_environment:environment
      errors
  in
  let state, _ =
    Request.process_request
      mock_client_socket
      initial_state
      (CommandTest.mock_server_configuration ())
      request
  in
  CommandTest.clean_environment ();
  let annotations =
    File.Handle.create relative_path
    |> Ast.SharedMemory.get_source
    |> (fun value -> Option.value_exn value)
    |> Lookup.create_of_source state.State.environment
    |> Lookup.get_all_annotations
    |> List.map ~f:(fun (key, data) ->
        Format.asprintf "%s/%a" (Location.Reference.to_string key) Type.pp data
        |> String.chop_prefix_exn ~prefix:(Int.to_string (String.hash relative_path)))
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      ":3:11-3:12/int";
      ":3:4-3:12/int";
      ":5:8-5:9/typing.Unbound";
      ":6:11-6:12/int";
      ":6:4-6:12/int";
    ]
    annotations



let test_incremental_repopulate _ =
  let parse content =
    Source.create
      ~path:"test.py"
      ~qualifier:(Source.qualifier ~path:"test.py")
      (Parser.parse ~path:"test.py" (String.split_lines (content ^ "\n")))
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
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let environment_handler = Environment.handler ~configuration environment in
  Out_channel.write_all ~data:source "test.py";
  add_defaults_to_environment environment_handler;
  Service.Environment.populate environment_handler [parse source];
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~initial_environment:environment
      errors
  in
  let get_annotation access_name =
    match Handler.function_definitions (Access.create access_name) with
    | Some [ { Node.value = { Statement.Define.return_annotation; _ }; _ } ] ->
        return_annotation
    | _ -> None
  in
  begin
    match (get_annotation "test.foo") with
    | Some expression -> assert_equal (Expression.show expression) "int"
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
    Request.process_request
      mock_client_socket
      initial_state
      (CommandTest.mock_server_configuration ())
      (Protocol.Request.TypeCheckRequest
         (Protocol.TypeCheckRequest.create
            ~update_environment_with:[file "test.py"]
            ~check:[file "test.py"]
            ()))
  in
  Sys.remove "test.py";
  begin match (get_annotation "test.foo") with
    | Some expression -> assert_equal (Expression.show expression) "str"
    | None -> assert_unreached ()
  end;
  CommandTest.clean_environment ()


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
    LanguageServer.Protocol.TextDocumentDefinitionResponse.create
      ~root:(Path.current_working_directory ())
      ~id:3
      ~location:None
    |> LanguageServer.Protocol.TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.to_string
  in
  assert_response
    ~source:"a = 1"
    ~request:(Protocol.Request.LanguageServerProtocolRequest request)
    (Some (Protocol.LanguageServerProtocolResponse expected_response))


let test_incremental_attribute_caching context =
  let directory = bracket_tmpdir context |> Path.create_absolute in
  let configuration =
    Configuration.create ~local_root:directory ~project_root:directory ()
  in
  let server_configuration = ServerConfiguration.create configuration in
  let environment =
    Analysis.Environment.Builder.create ()
    |> Analysis.Environment.handler ~configuration
  in
  add_defaults_to_environment environment;
  let ({ State.connections; lock = server_lock; _ } as old_state) =
    mock_server_state (File.Handle.Table.create ())
  in
  let source_path = Path.create_relative ~root:directory ~relative:"a.py" in
  let write_to_file ~content =
    Out_channel.write_all (Path.absolute source_path) ~data:(trim_extra_indentation content)
  in
  let content_with_annotation =
    {|
      class A:
        pass
      class C:
        def __init__(self):
          self.a: A = A()
        def f(self)->int:
          bleh = self.a
          return 1
    |}
  in
  write_to_file ~content:content_with_annotation;
  let initial_state =
    Server.Operations.initialize ~old_state server_lock connections server_configuration
  in
  let request_typecheck state =
    Request.process_request
      Unix.stdout
      state
      server_configuration
      (Protocol.Request.TypeCheckRequest
         (Protocol.TypeCheckRequest.create
            ~update_environment_with:[File.create source_path]
            ~check:[File.create source_path]
            ()))
    |> fst
  in
  let get_errors { State.errors; _ } = Hashtbl.to_alist errors in
  let state = request_typecheck initial_state in
  assert_equal (get_errors state) [];

  let content_without_annotation =
    {|
      class A:
        pass
      class C:
        def f(self)->int:
          bleh = self.a
          return 1
    |}
  in
  write_to_file ~content:content_without_annotation;
  let state = request_typecheck { state with State.environment } in
  begin
    match get_errors state with
    | [_, [error]] ->
        assert_equal
          ~cmp:String.equal
          ~printer:ident
          "Undefined attribute [16]: `C` has no attribute `a`."
          (Error.description ~detailed:false error)
    | _ ->
        assert_unreached ()
  end;

  write_to_file ~content:content_with_annotation;
  let state = request_typecheck { state with State.environment } in
  assert_equal (get_errors state) []


let () =
  CommandTest.run_command_tests
    "server"
    [
      "server_stops", test_server_stops;
      "server_exits_on_directory_removal", test_server_exits_on_directory_removal;
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
      "incremental_attribute_caching", test_incremental_attribute_caching;
      "language_scheduler_definition", test_language_scheduler_definition;
      "language_server_protocol_json_format", test_language_server_protocol_json_format;
    ]
