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


exception Timeout


let file ?content relative =
  let root = Path.current_working_directory () in
  File.create ?content (Path.create_relative ~root ~relative)


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
  let handle = File.Handle.create filename in
  Ast.SharedMemory.Sources.add
    handle
    (Source.create ~handle ~path:(Path.create_absolute filename) []);
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
  assert_equal ~printer:ident ~cmp:String.equal json_error_expect json_error;

  let malformed_response =
    LanguageServer.Protocol.PublishDiagnostics.of_errors
      (File.Handle.create "nonexistent_file")
      [type_error]
  in
  assert_true (Or_error.is_error malformed_response)


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


let make_errors ?(handle = "test.py") ?(qualifier = []) source =
  let configuration = CommandTest.mock_analysis_configuration () in
  let source = Preprocessing.preprocess (parse ~handle ~qualifier source) in
  let environment_handler = Environment.handler ~configuration (environment ()) in
  add_defaults_to_environment environment_handler;
  Service.Environment.populate (environment_handler) [source];
  (TypeCheck.check configuration environment_handler source).TypeCheck.Result.errors

let mock_server_state
    ?(initial_environment = environment ())
    errors =
  let environment = Environment.handler ~configuration initial_environment in
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

let mock_client_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null"

let assert_response
    ?(local_root = Path.current_working_directory ())
    ?state
    ?(handle = "test.py")
    ~source
    ~request
    expected_response =
  Ast.SharedMemory.HandleKeys.clear ();
  Ast.SharedMemory.Sources.remove ~handles:[File.Handle.create handle];
  let parsed = parse ~handle source in
  Ast.SharedMemory.Sources.add (File.Handle.create handle) parsed;
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      (make_errors ~handle source)
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
    | None -> mock_server_state ~initial_environment errors
  in
  let { Request.response; _ } =
    Request.process
      ~socket:mock_client_socket
      ~state:mock_server_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request
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
    ~source
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Some (Protocol.TypeCheckResponse (associate_errors_and_filenames errors)))


let test_query _ =
  let assert_type_query_response ~source ~query response =
    let query = Commands.Query.parse_query ~root:(Path.current_working_directory ()) query in
    assert_response ~source ~request:query (Some (Protocol.TypeQueryResponse response))
  in
  let parse_annotation serialized =
    serialized
    |> (fun literal -> String (StringLiteral.create literal))
    |> Node.create_with_default_location
    |> Type.create ~aliases:(fun _ -> None)
  in
  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, str)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean false));

  assert_type_query_response
    ~source:
      {|
        A = int
      |}
    ~query:"less_or_equal(int, A)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean true));

  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");

  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"less_or_equal(list[C], list[int])"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean true));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"join(list[C], list[int])"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type (parse_annotation "typing.List[int]")));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"meet(list[C], list[int])"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type (parse_annotation "typing.List[C]")));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"superclasses(C)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Superclasses [Type.integer]));

  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");

  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown[int])"
    (Protocol.TypeQuery.Error "No class definition found for Unknown.__getitem__.(...)");

  assert_type_query_response
    ~source:"A = int"
    ~query:"normalize_type(A)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.integer));

  assert_type_query_response
    ~source:{|
      class C:
        def C.foo(self) -> int: ...
        def C.bar(self) -> str: ...
    |}
    ~query:"methods(C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundMethods [
           {
             Protocol.TypeQuery.name = "foo";
             parameters = [Type.primitive "self"];
             return_annotation = Type.integer;
           };
           {
             Protocol.TypeQuery.name = "bar";
             parameters = [Type.primitive "self"];
             return_annotation = Type.string;
           }
         ]));

  assert_type_query_response
    ~source:""
    ~query:"methods(Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");

  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_location('test.py', 1, 4)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.integer));
  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_location('test.py', 1, 3)"
    (Protocol.TypeQuery.Error "Not able to get lookup at test.py:1:3");

  assert_type_query_response
    ~source:{|
      class C:
        C.x = 1
        C.y = ""
        def C.foo() -> int: ...
    |}
    ~query:"attributes(C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundAttributes [
           {
             Protocol.TypeQuery.name = "foo";
             annotation = Type.Callable {
                 Type.Callable.kind = Type.Callable.Named (Access.create "C.foo");
                 overloads = [
                   {
                     Type.Callable.annotation = Type.integer;
                     parameters = Type.Callable.Defined [];
                   };
                 ];
                 implicit = Type.Callable.Instance;
               }
           };
           { Protocol.TypeQuery.name = "x"; annotation = Type.integer };
           { Protocol.TypeQuery.name = "y"; annotation = Type.string };
         ]));
  ();

  assert_type_query_response
    ~source:{|
      def foo(x: int) -> int:
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature [
           {
             Protocol.TypeQuery.return_type = Some Type.integer;
             Protocol.TypeQuery.parameters = [
               {
                 Protocol.TypeQuery.parameter_name = "x";
                 Protocol.TypeQuery.annotation = Some Type.integer;
               }
             ];
           };
         ]));

  assert_type_query_response
    ~source:{|
      def foo(x) -> int:
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature [
           {
             Protocol.TypeQuery.return_type = Some Type.integer;
             Protocol.TypeQuery.parameters = [
               {
                 Protocol.TypeQuery.parameter_name = "x";
                 Protocol.TypeQuery.annotation = None;
               }
             ];
           };
         ]));

  assert_type_query_response
    ~source:{|
      def foo(x: int):
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature [
           {
             Protocol.TypeQuery.return_type = None;
             Protocol.TypeQuery.parameters = [
               {
                 Protocol.TypeQuery.parameter_name = "x";
                 Protocol.TypeQuery.annotation = Some Type.integer;
               }
             ];
           };
         ]));

  assert_type_query_response
    ~source:{|
      alias = int
      def foo(x: alias):
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature [
           {
             Protocol.TypeQuery.return_type = None;
             Protocol.TypeQuery.parameters = [
               {
                 Protocol.TypeQuery.parameter_name = "x";
                 Protocol.TypeQuery.annotation = Some Type.integer;
               }
             ];
           };
         ]));

  assert_type_query_response
    ~source:{|
      x = 1
    |}
    ~query:"signature(x)"
    (Protocol.TypeQuery.Error "x is not a callable");

  assert_type_query_response
    ~source:""
    ~query:"signature(unknown)"
    (Protocol.TypeQuery.Error "No signature found for unknown");

  assert_type_query_response
    ~source:{|
      foo: str = "bar"
    |}
    ~query:"type(foo)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.string));

  assert_type_query_response
    ~source:{|
      foo = 7
    |}
    ~query:"type(foo)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.integer));

  assert_type_query_response
    ~source:{|
    |}
    ~query:"type(8)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.integer));

  assert_type_query_response
    ~source:{|
      def foo(a: str) -> str:
        return a
      bar: str = "baz"
    |}
    ~query:"type(foo(bar))"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.string));

  assert_type_query_response
    ~source:{|
      def foo(a: str) -> str:
        return a
      bar: int = 7
    |}
    ~query:"type(foo(bar))"
    (Protocol.TypeQuery.Error
       "Expression had errors: Incompatible parameter type [6]: Expected `str` but got `int`.")


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
  let handle = relativize path in
  let stub_handle = relativize stub_path in
  let source =
    {|
        def foo() -> None:
          return 1
    |}
    |> trim_extra_indentation
  in
  let assert_response ?state ~request response =
    assert_response ~handle:path ~source ?state ~request (Some response)
  in
  assert_response
    ~request:(Protocol.Request.TypeCheckRequest
                (Protocol.TypeCheckRequest.create
                   ~update_environment_with:[file path]
                   ~check:[file path]
                   ()))
    (Protocol.TypeCheckResponse [(File.Handle.create handle), []]);
  (* The handles get updated in shared memory. *)
  assert_equal
    ~printer:(List.to_string ~f:File.Handle.show)
    (Ast.SharedMemory.HandleKeys.get ())
    [File.Handle.create handle];

  let files = [file ~content:source path] in
  let request_with_content =
    (Protocol.Request.TypeCheckRequest
       (Protocol.TypeCheckRequest.create ~update_environment_with:files ~check:files ()))
  in
  let errors =
    associate_errors_and_filenames
      (make_errors ~handle ~qualifier:(Source.qualifier ~handle:(File.Handle.create handle)) source)
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
    ~request:(check_request ~check:[file ~content:"def foo() -> int: return 1" path] ())
    (Protocol.TypeCheckResponse errors);
  let () =
    let stub_file = file ~content:"" (stub_path ^ "i") in
    assert_response
      ~request:(check_request ~update_environment_with:[stub_file] ())
      (Protocol.TypeCheckResponse []);
    assert_equal
      ~printer:(List.to_string ~f:File.Handle.show)
      (Ast.SharedMemory.HandleKeys.get ())
      [File.Handle.create (relativize stub_path ^ "i")];

  in
  assert_response
    ~request:(
      check_request
        ~check:[file ~content:"def foo() -> int: return \"\"" stub_path]
        ())
    (Protocol.TypeCheckResponse [File.Handle.create stub_handle, []]);

  let () =
    let file = file ~content:"def foo() -> int: return 1" path in
    assert_response
      ~request:(check_request ~update_environment_with:[file] ~check:[file] ())
      (Protocol.TypeCheckResponse [File.Handle.create handle, []])
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
  let { Request.response; _ } =
    Request.process
      ~socket:mock_client_socket
      ~state:server_state
      ~configuration:(CommandTest.mock_server_configuration ())
      ~request:(Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
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
  let errors = make_errors ~handle:filename ~qualifier source in
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
       Request.process
         ~socket:mock_client_socket
         ~state:server_state
         ~configuration:(CommandTest.mock_server_configuration ())
         ~request:(Protocol.Request.ClientConnectionRequest Protocol.Persistent));
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
    let handles =
      [
        File.Handle.create "a.py";
        File.Handle.create "b.py";
      ]
    in
    let sources =
      [
        parse ~handle:"a.py" ~path:(mock_path "a.py") ~qualifier:(Access.create "a") a_source;
        parse ~handle:"b.py" ~path:(mock_path "b.py") ~qualifier:(Access.create "b") b_source;
      ]
    in
    List.zip_exn handles sources
    |> List.iter ~f:(fun (handle, source) -> Ast.SharedMemory.Sources.add handle source);

    let environment = Environment.Builder.create () in
    let environment_handler = Environment.handler ~configuration environment in
    add_defaults_to_environment environment_handler;
    Service.Environment.populate environment_handler sources;
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
    let process request =
      Request.process
        ~socket:mock_client_socket
        ~state:initial_state
        ~configuration:(CommandTest.mock_server_configuration ())
        ~request
    in
    let { Request.state; response } =
      process (check_request ~update:[file "b.py"] ~check:[file "b.py"] ())
    in
    assert_equal (Some (Protocol.TypeCheckResponse expected_errors)) response;
    assert_equal state.State.deferred_requests [check_request ~check:[file "a.py"] ()];
    let { Request.state; response } =
      process (check_request ~update:[file "b.py"] ~check:[file "a.py"; file "b.py"] ())
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
    Sys.remove "b.py";
    Ast.SharedMemory.Sources.remove ~handles:[File.Handle.create "a.py"; File.Handle.create "b.py"]
  in
  Exn.protect ~f:assert_dependencies_analyzed ~finally


let test_incremental_lookups _ =
  let path, _ =
    Filename.open_temp_file
      ~in_dir:(Path.current_working_directory () |> Path.absolute)
      "test" ".py"
  in
  let handle =
    Path.create_relative ~root:(Path.current_working_directory ()) ~relative:path
    |> Path.relative
    |> Option.value ~default:path
    |> File.Handle.create
  in
  let parse content =
    Source.create
      ~handle
      ~qualifier:(Source.qualifier ~handle)
      (Parser.parse ~handle (String.split_lines (content ^ "\n")))
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
         ~update_environment_with:[file ~content:source path]
         ~check:[file ~content:source path]
         ())
  in
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~initial_environment:environment
      errors
  in
  let { Request.state; _ } =
    Request.process
      ~socket:mock_client_socket
      ~state:initial_state
      ~configuration:(CommandTest.mock_server_configuration ())
      ~request
  in
  CommandTest.clean_environment ();
  let annotations =
    handle
    |> Ast.SharedMemory.Sources.get
    |> (fun value -> Option.value_exn value)
    |> Lookup.create_of_source state.State.environment
    |> Lookup.get_all_annotations
    |> List.map ~f:(fun (key, data) ->
        Format.asprintf "%s/%a" (Location.Reference.to_string key) Type.pp data
        |> String.chop_prefix_exn ~prefix:(Int.to_string (String.hash (File.Handle.show handle))))
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
    let handle = File.Handle.create "test.py" in
    Source.create
      ~handle
      ~qualifier:(Source.qualifier ~handle)
      (Parser.parse ~handle (String.split_lines (content ^ "\n")))
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
  let _ =
    Request.process
      ~socket:mock_client_socket
      ~state:initial_state
      ~configuration:(CommandTest.mock_server_configuration ())
      ~request:(Protocol.Request.TypeCheckRequest
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
    Server.Operations.start
      ~old_state
      ~lock:server_lock
      ~connections
      ~configuration:server_configuration
      ()
  in
  let request_typecheck state =
    Request.process
      ~socket:Unix.stdout
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.TypeCheckRequest
                  (Protocol.TypeCheckRequest.create
                     ~update_environment_with:[File.create source_path]
                     ~check:[File.create source_path]
                     ()))
    |> fun { Request.state; _ } -> state
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
