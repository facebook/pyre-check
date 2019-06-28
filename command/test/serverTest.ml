(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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

let int_request_id id = LanguageServer.Types.RequestId.Int id

let string_request_id id = LanguageServer.Types.RequestId.String id

let file ~local_root ?content path =
  File.create ?content (Path.create_relative ~root:local_root ~relative:path)


let test_language_server_protocol_json_format context =
  let open TypeCheck.Error in
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let configuration = Configuration.Analysis.create ~local_root () in
  let filename =
    let path = Path.create_relative ~root:local_root ~relative:"filename.py" in
    File.write (File.create ~content:"" path);
    "filename.py"
  in
  let handle = File.Handle.create_for_testing filename in
  Ast.SharedMemory.Sources.add handle (Source.create ~handle []);
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
  let type_error = { type_error with location = { location with Location.path = filename } } in
  let normalize string =
    (* Working around OS inconsitencies. *)
    string
    |> String.split ~on:'\n'
    |> String.concat
    |> String.filter ~f:(fun character -> not (Char.is_whitespace character))
  in
  let json_error =
    LanguageServer.Protocol.PublishDiagnostics.of_errors
      ~configuration
      (File.Handle.create_for_testing filename)
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
                  "Incompatible return type [7]: Expected `None` but got `int`.\nType `None` expected on line 5, specified on line 4.",
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
      (Path.create_relative ~root:local_root ~relative:filename |> Path.absolute)
    |> Test.trim_extra_indentation
    |> normalize
  in
  assert_equal ~printer:ident ~cmp:String.equal json_error_expect json_error;
  let malformed_response =
    LanguageServer.Protocol.PublishDiagnostics.of_errors
      ~configuration
      (File.Handle.create_for_testing "nonexistent_file")
      [type_error]
  in
  assert_true (Or_error.is_error malformed_response)


let test_server_stops context =
  let local_root = bracket_tmpdir context |> Pyre.Path.create_absolute in
  let pid = Pid.of_int (CommandTest.start_server ~local_root ()) in
  Commands.Stop.stop ~local_root:(Path.absolute local_root) |> ignore;
  let { Configuration.Server.socket = { path = socket_path; _ };
        json_socket = { path = json_socket_path; _ };
        _
      }
    =
    Operations.create_configuration (Configuration.Analysis.create ~local_root ())
  in
  CommandTest.with_timeout ~seconds:3 CommandTest.poll_for_deletion socket_path;
  CommandTest.with_timeout ~seconds:3 CommandTest.poll_for_deletion json_socket_path;
  CommandTest.with_timeout
    ~seconds:1
    (fun () ->
      match Unix.waitpid pid with
      | Ok _ -> assert true
      | Error _ -> assert false)
    ()


let test_server_exits_on_directory_removal context =
  let directory = bracket_tmpdir context in
  let pid =
    Pid.of_int (CommandTest.start_server ~local_root:(Path.create_absolute directory) ())
  in
  Sys_utils.rm_dir_tree directory;
  CommandTest.with_timeout
    ~seconds:6
    (fun () ->
      match Unix.waitpid pid with
      (* I was only able to get non-zero exits in the OUnit test environment, doing the equivalent
         calls in the command line always resulted in an exit of 0. *)
      | Ok _
      | Error (`Exit_non_zero 2) ->
          assert true
      | _ -> assert false)
    ()


let test_stop_handles_unix_errors context =
  let long_path = bracket_tmpdir ~suffix:(String.init ~f:(fun _ -> 'a') 140) context in
  Commands.Stop.stop ~local_root:long_path |> ignore


let test_json_socket context =
  (* The server does not respond on the json socket, so this test is just to sanity check the
     handshake and verify that a client can send LSP to the server without crashing it. *)
  let local_root = bracket_tmpdir context |> Pyre.Path.create_absolute in
  let start_server _ =
    Pid.of_int (CommandTest.start_server ~local_root ~expected_version:"1234" ())
  in
  let stop_server pid _ =
    Commands.Stop.stop ~local_root:(Path.absolute local_root) |> ignore;
    CommandTest.with_timeout
      ~seconds:1
      (fun () ->
        match Unix.waitpid pid with
        | Ok _ -> assert true
        | Error _ -> assert false)
      ()
  in
  let pid = bracket start_server stop_server context in
  let socket_path =
    CommandTest.mock_analysis_configuration ~local_root ~expected_version:"1234" ()
    |> Service.Constants.Server.root
    |> (fun root -> Path.create_relative ~root ~relative:"json_server.sock")
    |> Path.real_path
    |> Path.absolute
  in
  let in_channel, out_channel = Unix.open_connection (Unix.ADDR_UNIX socket_path) in
  (* first, read and validate server handshake message *)
  ( in_channel
  |> LanguageServer.Protocol.read_message
  >>| LanguageServer.Types.HandshakeServer.of_yojson
  |> function
  | Some
      (Ok
        { jsonrpc = "2.0"; method_ = "handshake/server"; parameters = Some { version = "1234" } })
    ->
      ()
  | _ -> assert_bool "Handshake received from server is malformed" false );

  (* then, write client handshake message back *)
  { LanguageServer.Types.HandshakeClient.jsonrpc = "2.0";
    method_ = "handshake/client";
    parameters = Some ()
  }
  |> LanguageServer.Types.HandshakeClient.to_yojson
  |> LanguageServer.Protocol.write_message out_channel;
  Out_channel.flush out_channel;

  (* send valid and invalid LSP over the json socket *)
  `Assoc ["jsonrpc", `String "2.0"; "id", `Int 42; "method", `String "telemetry/rage"]
  |> LanguageServer.Protocol.write_message out_channel;
  Out_channel.flush out_channel;
  `Assoc [] |> LanguageServer.Protocol.write_message out_channel;
  Out_channel.flush out_channel;

  (* verify that the server is still alive at this point *)
  Unix.sleep 1;
  match Unix.wait_nohang (`Pid pid) with
  | Some _ -> assert false
  | None -> assert true


let configuration ~local_root = Configuration.Analysis.create ~local_root ~infer:true ()

let environment ~local_root =
  let environment = Environment.Builder.create () in
  let configuration = configuration ~local_root in
  let sources = typeshed_stubs ~include_helper_builtins:false () in
  Test.populate ~configuration (Environment.handler environment) sources;
  environment


let make_errors ~local_root ?(handle = "test.py") ?(qualifier = Reference.empty) source =
  let configuration = CommandTest.mock_analysis_configuration () in
  let source = parse ~handle ~qualifier source |> Preprocessing.preprocess in
  let environment = Environment.handler (environment ~local_root) in
  add_defaults_to_environment ~configuration environment;
  Test.populate ~configuration environment [source];
  TypeCheck.run ~configuration ~environment ~source


let mock_server_state ~local_root ?(initial_environment = environment ~local_root) errors =
  let configuration = configuration ~local_root in
  let module_tracker = Service.ModuleTracker.create configuration in
  let environment = Environment.handler initial_environment in
  add_defaults_to_environment ~configuration environment;
  { State.module_tracker;
    environment;
    errors;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    lookups = String.Table.create ();
    connections =
      { lock = Mutex.create ();
        connections =
          ref
            { State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
              json_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
              persistent_clients = Network.Socket.Map.empty;
              file_notifiers = []
            }
      };
    scheduler = Scheduler.mock ();
    open_documents = Path.Map.empty
  }


let assert_response
    ~local_root
    ?state
    ?(handle = "test.py")
    ?qualifier
    ~source
    ~request
    expected_response
  =
  Ast.SharedMemory.HandleKeys.clear ();
  Ast.SharedMemory.HandleKeys.add
    ~handles:(File.Handle.Set.Tree.singleton (File.Handle.create_for_testing handle));
  Ast.SharedMemory.Sources.remove ~handles:[File.Handle.create_for_testing handle];
  let parsed = parse ~handle ?qualifier source |> Preprocessing.preprocess in
  Ast.SharedMemory.Sources.add (File.Handle.create_for_testing handle) parsed;
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter (make_errors ~local_root ~handle source) ~f:(fun error ->
        Hashtbl.add_multi
          errors
          ~key:(File.Handle.create_for_testing (Error.path error))
          ~data:error);
    errors
  in
  let initial_environment =
    let environment = environment ~local_root in
    let configuration = configuration ~local_root in
    Test.populate ~configuration (Environment.handler environment) [parsed];
    environment
  in
  let mock_server_state =
    match state with
    | Some state -> state
    | None -> mock_server_state ~initial_environment ~local_root errors
  in
  let { Request.response; _ } =
    Request.process
      ~state:mock_server_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request
  in
  let printer = function
    | None -> "None"
    | Some response -> Protocol.show_response response
  in
  let pp_opt formatter = function
    | None -> Format.pp_print_string formatter "None"
    | Some response -> Protocol.pp_response formatter response
  in
  assert_equal
    ~cmp:(Option.equal Protocol.equal_response)
    ~pp_diff:(diff ~print:pp_opt)
    ~printer
    expected_response
    response


let test_shutdown context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let source = "x = 1" in
  assert_response
    ~local_root
    ~source
    ~request:(Protocol.Request.ClientShutdownRequest (int_request_id 1))
    (Some
       (Protocol.LanguageServerProtocolResponse
          ( LanguageServer.Protocol.ShutdownResponse.default (int_request_id 1)
          |> LanguageServer.Protocol.ShutdownResponse.to_yojson
          |> Yojson.Safe.to_string )))


let test_language_scheduler_shutdown context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let source = "x = 1" in
  assert_response
    ~local_root
    ~source
    ~request:
      (Protocol.Request.LanguageServerProtocolRequest
         {|
        {
           "jsonrpc": "2.0",
           "id": 2,
           "method": "shutdown",
           "params": null
        }
       |})
    (Some
       (Protocol.LanguageServerProtocolResponse
          ( LanguageServer.Protocol.ShutdownResponse.default (int_request_id 2)
          |> LanguageServer.Protocol.ShutdownResponse.to_yojson
          |> Yojson.Safe.to_string )))


let test_protocol_type_check context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let source = {|
        def foo() -> None:
          return 1
    |} in
  let errors = make_errors ~local_root source in
  assert_response
    ~local_root
    ~source
    ~request:(Protocol.Request.DisplayTypeErrors [])
    (Some (Protocol.TypeCheckResponse (CommandTest.associate_errors_and_filenames errors)))


let test_query context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let assert_type_query_response ?handle ?qualifier ~source ~query response =
    let query = Query.parse_query ~configuration:(configuration ~local_root) query in
    assert_response
      ?handle
      ?qualifier
      ~local_root
      ~source
      ~request:query
      (Some (Protocol.TypeQueryResponse response))
  in
  let parse_annotation serialized =
    serialized
    |> (fun literal -> String (StringLiteral.create literal))
    |> Node.create_with_default_location
    |> Type.create ~aliases:(fun _ -> None)
  in
  let create_location ~path start_line start_column stop_line stop_column =
    let start = { Location.line = start_line; column = start_column } in
    let stop = { Location.line = stop_line; column = stop_column } in
    { Location.path; start; stop }
  in
  let create_types_at_locations =
    let convert (start_line, start_column, end_line, end_column, annotation) =
      { Protocol.TypeQuery.location =
          create_location ~path:"test.py" start_line start_column end_line end_column;
        annotation
      }
    in
    List.map ~f:convert
  in
  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, str)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean false));
  assert_type_query_response
    ~source:{|
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
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean false));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"join(list[C], list[int])"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.Type (parse_annotation "typing.List[typing.Any]")));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"meet(list[C], list[int])"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type (Type.list Type.Bottom)));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"superclasses(C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.Superclasses
          [Type.integer; Type.float; Type.complex; Type.object_primitive]));
  let assert_compatibility_response ~source ~query ~actual ~expected result =
    assert_type_query_response
      ~source
      ~query
      (Protocol.TypeQuery.Response (Protocol.TypeQuery.Compatibility { actual; expected; result }))
  in
  assert_compatibility_response
    ~source:""
    ~query:"is_compatible_with(int, str)"
    ~actual:Type.integer
    ~expected:Type.string
    false;
  assert_compatibility_response
    ~source:{|
        A = int
      |}
    ~query:"is_compatible_with(int, A)"
    ~actual:Type.integer
    ~expected:Type.integer
    true;
  assert_type_query_response
    ~source:""
    ~query:"is_compatible_with(int, Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(int, unknown)"
    ~actual:Type.integer
    ~expected:Type.Top
    true;
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(typing.List[int], typing.List[unknown])"
    ~actual:(Type.list Type.integer)
    ~expected:(Type.list Type.Top)
    true;
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(int, typing.List[unknown])"
    ~actual:Type.integer
    ~expected:(Type.list Type.Top)
    false;
  assert_compatibility_response
    ~source:""
    ~query:"is_compatible_with(int, typing.Coroutine[typing.Any, typing.Any, int])"
    ~actual:Type.integer
    ~expected:Type.integer
    true;
  assert_compatibility_response
    ~source:""
    ~query:"is_compatible_with(int, typing.Coroutine[typing.Any, typing.Any, str])"
    ~actual:Type.integer
    ~expected:Type.string
    false;
  assert_compatibility_response
    ~source:"A = int"
    ~query:"is_compatible_with(A, typing.Coroutine[typing.Any, typing.Any, A])"
    ~actual:Type.integer
    ~expected:Type.integer
    true;
  assert_compatibility_response
    ~source:{|
         class A: ...
         class B(A): ...
      |}
    ~query:"is_compatible_with(B, typing.Coroutine[typing.Any, typing.Any, A])"
    ~actual:(Type.Primitive "B")
    ~expected:(Type.Primitive "A")
    true;
  assert_compatibility_response
    ~source:{|
         class A: ...
         class B(A): ...
      |}
    ~query:
      ( "is_compatible_with(typing.Type[B],"
      ^ "typing.Coroutine[typing.Any, typing.Any, typing.Type[A]])" )
    ~actual:(Type.meta (Type.Primitive "B"))
    ~expected:(Type.meta (Type.Primitive "A"))
    true;
  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");
  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown[int])"
    (Protocol.TypeQuery.Error "Type `Unknown[int]` was not found in the type order.");
  assert_type_query_response
    ~source:"A = int"
    ~query:"normalize_type(A)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.integer));
  assert_type_query_response
    ~source:
      {|
      class C:
        def C.foo(self) -> int: ...
        def C.bar(self) -> str: ...
    |}
    ~query:"methods(C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundMethods
          [ { Protocol.TypeQuery.name = "foo";
              parameters = [Type.Primitive "self"];
              return_annotation = Type.integer
            };
            { Protocol.TypeQuery.name = "bar";
              parameters = [Type.Primitive "self"];
              return_annotation = Type.string
            } ]));
  assert_type_query_response
    ~source:""
    ~query:"methods(Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");
  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_position('test.py', 1, 4)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypeAtLocation
          { Protocol.TypeQuery.location = create_location ~path:"test.py" 1 4 1 5;
            annotation = Type.literal_integer 2
          }));
  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_position('test.py', 1, 3)"
    (Protocol.TypeQuery.Error
       ("Not able to get lookup at " ^ Path.absolute local_root ^/ "test.py:1:3"));
  assert_type_query_response
    ~source:{|
      a: int = 1
      a = 2
    |}
    ~query:"type_at_position('test.py', 3, 0)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypeAtLocation
          { Protocol.TypeQuery.location = create_location ~path:"test.py" 3 0 3 1;
            annotation = Type.integer
          }));
  assert_type_query_response
    ~source:{|
      def foo(x: int = 10, y: str = "bar") -> None:
        a = 42
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 3, 6, 3, 8, Type.literal_integer 42;
              2, 24, 2, 27, Type.meta Type.string;
              2, 21, 2, 22, Type.string;
              2, 40, 2, 44, Type.none;
              2, 17, 2, 19, Type.literal_integer 10;
              3, 2, 3, 3, Type.literal_integer 42;
              2, 30, 2, 35, Type.literal_string "bar";
              2, 11, 2, 14, Type.meta Type.integer;
              2, 8, 2, 9, Type.integer ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:
      {|
       def foo(x: int, y: str) -> str:
        x = 4
        y = 5
        return x
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 2, 19, 2, 22, Type.meta Type.string;
              5, 8, 5, 9, Type.integer;
              2, 27, 2, 30, Type.meta Type.string;
              4, 1, 4, 2, Type.string;
              4, 5, 4, 6, Type.literal_integer 5;
              3, 1, 3, 2, Type.integer;
              2, 11, 2, 14, Type.meta Type.integer;
              3, 5, 3, 6, Type.literal_integer 4;
              2, 8, 2, 9, Type.integer;
              2, 16, 2, 17, Type.string ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:{|
        x = 4
        y = 3
     |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 2, 4, 2, 5, Type.literal_integer 4;
              2, 0, 2, 1, Type.integer;
              3, 0, 3, 1, Type.integer;
              3, 4, 3, 5, Type.literal_integer 3 ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:{|
      def foo():
        if True:
         x = 1
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 3, 5, 3, 9, Type.Literal (Boolean true);
              4, 3, 4, 4, Type.literal_integer 1;
              4, 7, 4, 8, Type.literal_integer 1 ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:{|
       def foo():
         for x in [1, 2]:
          y = 1
     |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 3, 12, 3, 13, Type.literal_integer 1;
              3, 15, 3, 16, Type.literal_integer 2;
              3, 11, 3, 17, Type.list Type.integer;
              ( 3,
                6,
                3,
                7,
                (* TODO(T46070919): The type here should be int, not the iter callable *)
                parse_callable
                  ~name:(Reference.create "list.__iter__")
                  "typing.Callable[[], typing.Iterator[int]]" );
              4, 3, 4, 4, Type.literal_integer 1;
              4, 7, 4, 8, Type.literal_integer 1 ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:{|
        try:
          x = 1
        except Exception:
          y = 2
      |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 4, 7, 4, 16, Type.parametric "type" [Type.Primitive "Exception"];
              5, 2, 5, 3, Type.literal_integer 2;
              3, 2, 3, 3, Type.literal_integer 1;
              3, 6, 3, 7, Type.literal_integer 1;
              5, 6, 5, 7, Type.literal_integer 2 ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:{|
       with open() as x:
        y = 2
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [3, 1, 3, 2, Type.literal_integer 2; 3, 5, 3, 6, Type.literal_integer 2]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:{|
      while x is True:
        y = 1
   |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 2, 11, 2, 15, Type.Literal (Boolean true);
              3, 2, 3, 3, Type.literal_integer 1;
              3, 6, 3, 7, Type.literal_integer 1;
              2, 6, 2, 15, Type.bool ]
          |> create_types_at_locations )));
  assert_type_query_response
    ~source:
      {|
       def foo(x: int) -> str:
         def bar(y: int) -> str:
           return y
         return x
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 2, 19, 2, 22, parse_annotation "typing.Type[str]";
              5, 9, 5, 10, Type.integer;
              3, 21, 3, 24, parse_annotation "typing.Type[str]";
              3, 13, 3, 16, parse_annotation "typing.Type[int]";
              4, 11, 4, 12, Type.integer;
              2, 11, 2, 14, parse_annotation "typing.Type[int]";
              2, 8, 2, 9, Type.integer;
              3, 10, 3, 11, Type.integer ]
          |> create_types_at_locations )));

  assert_type_query_response
    ~source:{|
       def foo(x: typing.List[int]) -> None:
        pass
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ( [ 2, 32, 2, 36, Type.none;
              2, 23, 2, 26, Type.meta Type.integer;
              (* TODO:(T46070919): Missing Type[List[int]] *)
              2, 8, 2, 9, Type.list Type.integer;
              2, 11, 2, 22, Type.Primitive "typing.TypeAlias" ]
          |> create_types_at_locations )));

  assert_type_query_response
    ~source:{|
       class Foo:
         x = 1
     |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          [ (* TODO:(T46070919): Interprets this assignment as `FooFoo.x = 1` and insanity ensues. *)
            { Protocol.TypeQuery.location = create_location ~path:"test.py" 3 2 3 3;
              annotation = parse_annotation "typing.Type[Foo]"
            };
            { Protocol.TypeQuery.location = create_location ~path:"test.py" 3 6 3 7;
              annotation = Type.literal_integer 1
            } ]));

  assert_type_query_response
    ~source:{|
      class C:
        x = 1
        y = ""
        def foo() -> int: ...
    |}
    ~query:"attributes(C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundAttributes
          [ { Protocol.TypeQuery.name = "foo";
              annotation =
                Type.Callable
                  { Type.Callable.kind = Type.Callable.Named !&"C.foo";
                    implementation =
                      { Type.Callable.annotation = Type.integer;
                        parameters = Type.Callable.Defined [];
                        define_location = None
                      };
                    overloads = [];
                    implicit = Some { implicit_annotation = Type.Primitive "C"; name = "self" }
                  }
            };
            { Protocol.TypeQuery.name = "x"; annotation = Type.integer };
            { Protocol.TypeQuery.name = "y"; annotation = Type.string } ]));
  ();
  assert_type_query_response
    ~source:{|
      def foo(x: int) -> int:
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ { Protocol.TypeQuery.return_type = Some Type.integer;
              Protocol.TypeQuery.parameters =
                [ { Protocol.TypeQuery.parameter_name = "x";
                    Protocol.TypeQuery.annotation = Some Type.integer
                  } ]
            } ]));
  assert_type_query_response
    ~source:{|
      def foo(x) -> int:
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ { Protocol.TypeQuery.return_type = Some Type.integer;
              Protocol.TypeQuery.parameters =
                [{ Protocol.TypeQuery.parameter_name = "x"; Protocol.TypeQuery.annotation = None }]
            } ]));
  assert_type_query_response
    ~source:{|
      def foo(x: int):
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ { Protocol.TypeQuery.return_type = None;
              Protocol.TypeQuery.parameters =
                [ { Protocol.TypeQuery.parameter_name = "x";
                    Protocol.TypeQuery.annotation = Some Type.integer
                  } ]
            } ]));
  assert_type_query_response
    ~source:{|
      alias = int
      def foo(x: alias):
        pass
    |}
    ~query:"signature(foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ { Protocol.TypeQuery.return_type = None;
              Protocol.TypeQuery.parameters =
                [ { Protocol.TypeQuery.parameter_name = "x";
                    Protocol.TypeQuery.annotation = Some Type.integer
                  } ]
            } ]));
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
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type (Type.literal_integer 8)));
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
       ( "Expression had errors: Incompatible parameter type [6]: "
       ^ "Expected `str` for 1st anonymous parameter to call `foo` but got `int`." ));
  let temporary_directory = OUnit2.bracket_tmpdir context in
  assert_type_query_response
    ~source:""
    ~query:(Format.sprintf "save_server_state('%s/state')" temporary_directory)
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Success "Saved state."));
  assert_equal `Yes (Sys.is_file (temporary_directory ^/ "state"));
  Path.create_relative ~root:local_root ~relative:"a.py"
  |> File.create ~content:"pass"
  |> File.write;
  assert_type_query_response
    ~qualifier:!&"a"
    ~handle:"a.py"
    ~source:"pass"
    ~query:"path_of_module(a)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundPath
          (Path.create_relative ~root:local_root ~relative:"a.py" |> Path.absolute)))


let test_compute_hashes_to_keys context =
  let open Protocol in
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let (module Handler : Analysis.Environment.Handler) =
    (module Service.Environment.SharedHandler)
  in
  let set_up_shared_memory _ =
    Handler.TypeOrderHandler.add_key 15;
    Handler.TypeOrderHandler.add_key 16;
    Handler.TypeOrderHandler.set
      (Handler.TypeOrderHandler.annotations ())
      ~key:15
      ~data:(Primitive "fifteen");
    Handler.TypeOrderHandler.set
      (Handler.TypeOrderHandler.indices ())
      ~key:(Primitive "fifteen")
      ~data:15;
    Handler.TypeOrderHandler.set
      (Handler.TypeOrderHandler.annotations ())
      ~key:16
      ~data:(Primitive "sixteen");
    Handler.TypeOrderHandler.set
      (Handler.TypeOrderHandler.indices ())
      ~key:(Primitive "sixteen")
      ~data:16;
    Handler.TypeOrderHandler.set
      (Handler.TypeOrderHandler.edges ())
      ~key:15
      ~data:[{ TypeOrder.Target.target = 16; parameters = [] }];
    Handler.TypeOrderHandler.set
      (Handler.TypeOrderHandler.backedges ())
      ~key:16
      ~data:(TypeOrder.Target.Set.of_list [{ TypeOrder.Target.target = 15; parameters = [] }]);
    ResolutionSharedMemory.Keys.remove_batch
      (File.Handle.create_for_testing "sample.py" |> ResolutionSharedMemory.Keys.KeySet.singleton);
    ResolutionSharedMemory.add
      ~handle:(File.Handle.create_for_testing "sample.py")
      (Reference.create "name")
      Int.Map.Tree.empty
  in
  let tear_down_shared_memory () _ =
    let open Service.EnvironmentSharedMemory in
    OrderEdges.remove_batch (OrderEdges.KeySet.of_list [15; 16]);
    OrderBackedges.remove_batch (OrderBackedges.KeySet.of_list [15; 16]);
    OrderAnnotations.remove_batch (OrderAnnotations.KeySet.of_list [15; 16]);
    OrderIndices.remove_batch
      (OrderIndices.KeySet.of_list [Primitive "fifteen"; Primitive "sixteen"]);
    Ast.SharedMemory.HandleKeys.clear ()
  in
  OUnit2.bracket set_up_shared_memory tear_down_shared_memory context;
  let expected =
    let open Service.EnvironmentSharedMemory in
    let compare { TypeQuery.hash = left; _ } { TypeQuery.hash = right; _ } =
      String.compare left right
    in
    let to_binding hash key = { TypeQuery.hash; key } in
    ( Ast.SharedMemory.HandleKeys.compute_hashes_to_keys ()
    |> String.Map.to_alist
    |> List.map ~f:(fun (hash, key) -> { TypeQuery.hash; key }) )
    @ [ to_binding
          (OrderKeys.hash_of_key SharedMemory.SingletonKey.key)
          (OrderKeys.serialize_key SharedMemory.SingletonKey.key);
        to_binding
          (OrderIndices.hash_of_key (Primitive "sixteen"))
          (OrderIndices.serialize_key (Primitive "sixteen"));
        to_binding (OrderAnnotations.hash_of_key 15) (OrderAnnotations.serialize_key 15);
        to_binding (OrderEdges.hash_of_key 16) (OrderEdges.serialize_key 16);
        to_binding (OrderEdges.hash_of_key 15) (OrderEdges.serialize_key 15);
        to_binding
          (OrderIndices.hash_of_key (Primitive "fifteen"))
          (OrderIndices.serialize_key (Primitive "fifteen"));
        to_binding (OrderBackedges.hash_of_key 15) (OrderBackedges.serialize_key 15);
        to_binding (OrderAnnotations.hash_of_key 16) (OrderAnnotations.serialize_key 16);
        to_binding (OrderBackedges.hash_of_key 16) (OrderBackedges.serialize_key 16);
        to_binding
          (Ast.SharedMemory.SymlinksToPaths.hash_of_key "sample.py")
          (Ast.SharedMemory.SymlinksToPaths.serialize_key "sample.py");
        to_binding
          (Ast.SharedMemory.Handles.hash_of_key (String.hash "sample.py"))
          (Ast.SharedMemory.Handles.serialize_key (String.hash "sample.py"));
        to_binding
          (Ast.SharedMemory.Sources.Sources.hash_of_key
             (File.Handle.create_for_testing "sample.py"))
          (Ast.SharedMemory.Sources.Sources.serialize_key
             (File.Handle.create_for_testing "sample.py"));
        to_binding
          (Ast.SharedMemory.Sources.QualifiersToHandles.hash_of_key (Reference.create "sample"))
          (Ast.SharedMemory.Sources.QualifiersToHandles.serialize_key (Reference.create "sample"));
        to_binding
          (Ast.SharedMemory.Modules.hash_of_key !&"sample")
          (Ast.SharedMemory.Modules.serialize_key !&"sample");
        to_binding
          (FunctionKeys.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (FunctionKeys.serialize_key (File.Handle.create_for_testing "sample.py"));
        to_binding
          (ClassKeys.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (ClassKeys.serialize_key (File.Handle.create_for_testing "sample.py"));
        to_binding
          (GlobalKeys.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (GlobalKeys.serialize_key (File.Handle.create_for_testing "sample.py"));
        to_binding
          (AliasKeys.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (AliasKeys.serialize_key (File.Handle.create_for_testing "sample.py"));
        to_binding
          (DependentKeys.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (DependentKeys.serialize_key (File.Handle.create_for_testing "sample.py"));
        to_binding
          (ResolutionSharedMemory.hash_of_key (Reference.create "$toplevel"))
          (ResolutionSharedMemory.serialize_key (Reference.create "$toplevel"));
        to_binding
          (ResolutionSharedMemory.hash_of_key (Reference.create "name"))
          (ResolutionSharedMemory.serialize_key (Reference.create "name"));
        to_binding
          (ResolutionSharedMemory.Keys.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (ResolutionSharedMemory.Keys.serialize_key (File.Handle.create_for_testing "sample.py"));
        to_binding
          (Analysis.Dependencies.Callgraph.SharedMemory.hash_of_key (Reference.create "$toplevel"))
          (Analysis.Dependencies.Callgraph.SharedMemory.serialize_key
             (Reference.create "$toplevel"));
        to_binding
          (Coverage.SharedMemory.hash_of_key (File.Handle.create_for_testing "sample.py"))
          (Coverage.SharedMemory.serialize_key (File.Handle.create_for_testing "sample.py")) ]
    |> List.sort ~compare
  in
  assert_response
    ~local_root
    ~source:""
    ~handle:"sample.py"
    ~request:(Request.TypeQueryRequest TypeQuery.ComputeHashesToKeys)
    (Some (Protocol.TypeQueryResponse (TypeQuery.Response (TypeQuery.FoundKeyMapping expected))))


let test_decode_serialized_ocaml_values context =
  let open Service.EnvironmentSharedMemory in
  let open Protocol in
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let (module Handler : Analysis.Environment.Handler) =
    (module Service.Environment.SharedHandler)
  in
  (* Note that we're not adding any values to the shared environment here. *)
  assert_response
    ~local_root
    ~source:""
    ~request:
      (Request.TypeQueryRequest
         (TypeQuery.DecodeOcamlValues
            [ TypeQuery.SerializedValue
                { serialized_key = OrderEdges.serialize_key 16;
                  serialized_value =
                    [{ TypeOrder.Target.target = 15; parameters = [Type.integer] }]
                    |> (fun value -> Marshal.to_string value [Marshal.Closures])
                    |> Base64.encode_exn
                };
              TypeQuery.SerializedValue
                { serialized_key = OrderBackedges.serialize_key 15;
                  serialized_value =
                    [{ TypeOrder.Target.target = 16; parameters = [Type.string] }]
                    |> (fun value -> Marshal.to_string value [Marshal.Closures])
                    |> Base64.encode_exn
                };
              TypeQuery.SerializedValue
                { serialized_key = "Can't decode this"; serialized_value = "Nope" } ]))
    (Some
       (Protocol.TypeQueryResponse
          (TypeQuery.Response
             (TypeQuery.Decoded
                { TypeQuery.decoded =
                    [ TypeQuery.DecodedValue
                        { serialized_key = OrderBackedges.serialize_key 15;
                          kind = "Backedges";
                          actual_key = "Undecodable(15)";
                          actual_value = Some "(\"Undecodable(16)[str]\")"
                        };
                      TypeQuery.DecodedValue
                        { serialized_key = OrderEdges.serialize_key 16;
                          kind = "Edges";
                          actual_key = "Undecodable(16)";
                          actual_value = Some "(\"Undecodable(15)[int]\")"
                        } ];
                  undecodable_keys = ["Can't decode this"]
                }))));
  let assert_decode ~key ~value ~response:(kind, actual_key, actual_value) =
    let value = Marshal.to_string value [Marshal.Closures] |> Base64.encode_exn in
    let response base =
      Protocol.TypeQueryResponse
        (TypeQuery.Response
           (TypeQuery.Decoded { TypeQuery.decoded = [base]; undecodable_keys = [] }))
    in
    assert_response
      ~local_root
      ~source:""
      ~request:
        (Request.TypeQueryRequest
           (TypeQuery.DecodeOcamlValues
              [TypeQuery.SerializedValue { serialized_key = key; serialized_value = value }]))
      (Some
         (response
            (TypeQuery.DecodedValue { serialized_key = key; kind; actual_key; actual_value })));
    assert_response
      ~local_root
      ~source:""
      ~request:
        (Request.TypeQueryRequest
           (TypeQuery.DecodeOcamlValues
              [ TypeQuery.SerializedPair
                  { serialized_key = key;
                    first_serialized_value = value;
                    second_serialized_value = value
                  } ]))
      (Some
         (response
            (TypeQuery.DecodedPair
               { serialized_key = key;
                 kind;
                 actual_key;
                 first_value = actual_value;
                 second_value = actual_value;
                 equal = true
               })))
  in
  assert_decode
    ~key:(ClassDefinitions.serialize_key "int")
    ~value:(Node.create_with_default_location (Test.parse_single_class "class C: pass"))
    ~response:
      ( "Class",
        "int",
        let json =
          let class_definition =
            "{ Statement.Record.Class.name = C; bases = []; body = [Statement.Pass];\n\
            \  decorators = []; docstring = None }"
          in
          Format.sprintf {|{"class_definition": "%s"}|} class_definition
        in
        Some (Yojson.Safe.to_string (Yojson.Safe.from_string json)) );
  assert_decode
    ~key:(ClassMetadata.serialize_key "int")
    ~value:{ Resolution.successors = ["str"]; is_test = false; is_final = false }
    ~response:
      ("Class metadata", "int", Some {|{"successors":"(str)","is_test":false,"is_final":false}|});
  assert_decode
    ~key:(Aliases.serialize_key "my_integer")
    ~value:(Type.TypeAlias Type.integer)
    ~response:("Alias", "my_integer", Some "(Type.TypeAlias int)");
  assert_decode
    ~key:(Globals.serialize_key !&"string_global")
    ~value:(Annotation.create Type.string |> Node.create_with_default_location)
    ~response:("Global", "string_global", Some "((annotation(Primitive str))(mutability Mutable))");
  assert_decode
    ~key:(Dependents.serialize_key !&"module")
    ~value:([!&"dependentA"; !&"dependentB"] |> Reference.Set.Tree.of_list)
    ~response:("Dependent", "module", Some "(dependentA dependentB)");
  assert_decode
    ~key:(Ast.SharedMemory.SymlinksToPaths.serialize_key "symbolic_link.py")
    ~value:(Path.create_absolute ~follow_symbolic_links:false "actual_filename.py")
    ~response:("SymlinkSource", "symbolic_link.py", Some "actual_filename.py");
  assert_decode
    ~key:
      (Ast.SharedMemory.Sources.Sources.serialize_key (File.Handle.create_for_testing "handle.py"))
    ~value:(Source.create [Test.parse_single_statement "x = 1 + 2"])
    ~response:("AST", "handle.py", Some "x = 1.__add__(2)\n");
  assert_decode
    ~key:(Ast.SharedMemory.Sources.QualifiersToHandles.serialize_key !&"handle")
    ~value:(File.Handle.create_for_testing "handle.py")
    ~response:("File handle", "handle", Some "handle.py");
  assert_decode
    ~key:(Ast.SharedMemory.Handles.Paths.serialize_key 5)
    ~value:(File.Handle.create_for_testing "five.py")
    ~response:("Path", "5", Some "five.py");
  assert_decode
    ~key:(Coverage.SharedMemory.serialize_key (File.Handle.create_for_testing "file.py"))
    ~value:(Coverage.create ~full:5 ~partial:3 ())
    ~response:
      ( "Coverage",
        "file.py",
        Some "{ Coverage.full = 5; partial = 3; untyped = 0; ignore = 0; crashes = 0 }" );
  assert_decode
    ~key:(ResolutionSharedMemory.serialize_key (Reference.create "$toplevel"))
    ~value:
      (Int.Map.Tree.singleton
         0
         { ResolutionSharedMemory.precondition =
             Reference.Map.Tree.of_alist_exn [!&"x", Annotation.create Type.integer];
           postcondition =
             Reference.Map.Tree.of_alist_exn
               [!&"x", Annotation.create Type.string; !&"y", Annotation.create Type.integer]
         })
    ~response:
      ( "Node type resolution",
        "$toplevel",
        Some
          "{0: { \"Precondition\": {\"x\": \"(int: m)\", }, \"Postcondition\": {\"x\": \"(str: \
           m)\", \"y\": \"(int: m)\", }}" );
  assert_decode
    ~key:(OrderKeys.serialize_key SharedMemory.SingletonKey.key)
    ~value:[15; 16]
    ~response:("Order keys", "0", Some "(\"Undecodable(15)\"\"Undecodable(16)\")");
  assert_decode
    ~key:(UndecoratedFunctions.serialize_key (Reference.create "f"))
    ~value:
      { Type.Callable.annotation = Type.integer;
        parameters = Type.Callable.Undefined;
        define_location = None
      }
    ~response:
      ( "Undecorated functions",
        "f",
        Some
          "{ Type.Record.Callable.annotation = int;\n\
          \  parameters = Type.Record.Callable.Undefined; define_location = None }" );
  let assert_decode_pair ~key ~first ~second ~response =
    let first_serialized_value = Marshal.to_string first [Marshal.Closures] |> Base64.encode_exn in
    let second_serialized_value =
      Marshal.to_string second [Marshal.Closures] |> Base64.encode_exn
    in
    let response =
      Protocol.TypeQueryResponse
        (TypeQuery.Response
           (TypeQuery.Decoded { TypeQuery.decoded = [response]; undecodable_keys = [] }))
    in
    assert_response
      ~local_root
      ~source:""
      ~request:
        (Request.TypeQueryRequest
           (TypeQuery.DecodeOcamlValues
              [ TypeQuery.SerializedPair
                  { serialized_key = key; first_serialized_value; second_serialized_value } ]))
      (Some response)
  in
  assert_decode_pair
    ~key:(Aliases.serialize_key "my_integer")
    ~first:(Type.TypeAlias Type.integer)
    ~second:(Type.TypeAlias Type.string)
    ~response:
      (TypeQuery.DecodedPair
         { serialized_key = Aliases.serialize_key "my_integer";
           kind = "Alias";
           actual_key = "my_integer";
           first_value = Some "(Type.TypeAlias int)";
           second_value = Some "(Type.TypeAlias str)";
           equal = false
         });
  assert_decode_pair
    ~key:(Globals.serialize_key !&"string_global")
    ~first:(Annotation.create Type.string |> Node.create_with_default_location)
    ~second:(Annotation.create Type.string |> Node.create ~location:Location.Reference.synthetic)
    ~response:
      (TypeQuery.DecodedPair
         { serialized_key = Globals.serialize_key !&"string_global";
           kind = "Global";
           actual_key = "string_global";
           first_value = Some "((annotation(Primitive str))(mutability Mutable))";
           second_value = Some "((annotation(Primitive str))(mutability Mutable))";
           equal = true
         });
  assert_decode_pair
    ~key:(Globals.serialize_key !&"string_global")
    ~first:(Annotation.create Type.string |> Node.create_with_default_location)
    ~second:(Annotation.create Type.integer |> Node.create ~location:Location.Reference.synthetic)
    ~response:
      (TypeQuery.DecodedPair
         { serialized_key = Globals.serialize_key !&"string_global";
           kind = "Global";
           actual_key = "string_global";
           first_value = Some "((annotation(Primitive str))(mutability Mutable))";
           second_value = Some "((annotation(Primitive int))(mutability Mutable))";
           equal = false
         })


let test_connect context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  CommandTest.start_server ~local_root ~expected_version:"A" () |> ignore;
  let { Configuration.Server.configuration;
        socket = { path = socket_path; _ };
        json_socket = { path = json_socket_path; _ };
        _
      }
    =
    CommandTest.mock_server_configuration ~local_root ~expected_version:"B" ()
  in
  (* This sleep ensures that the server doesn't receive an EPIPE while the Hack_parallel library is
   * initializing the daemon in the hack_parallel/utils/handle.ml. In that codepath, an external
   * routine is called, and due to the nature of the Lazy library this is non-reentrant. *)
  Unix.nanosleep 0.5 |> ignore;
  let cleanup () =
    Commands.Stop.stop ~local_root:(Path.absolute local_root) |> ignore;
    CommandTest.with_timeout CommandTest.poll_for_deletion socket_path ~seconds:3;
    CommandTest.with_timeout CommandTest.poll_for_deletion json_socket_path ~seconds:3
  in
  Exn.protect
    ~f:(fun () ->
      assert_raises
        (Server.Operations.VersionMismatch
           { Server.Operations.server_version = "A"; expected_version = "B" })
        (fun () -> Server.Operations.connect ~retries:1 ~configuration))
    ~finally:cleanup


let test_incremental_typecheck context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let path_to_python_file prefix =
    Filename.open_temp_file ~in_dir:(Path.absolute local_root) prefix ".py" |> fst
  in
  let path = path_to_python_file "test" in
  let stub_path = path_to_python_file "stub" ^ "i" in
  Out_channel.write_all stub_path ~data:"";
  let relativize path =
    Path.create_relative ~root:local_root ~relative:path
    |> Path.relative
    |> Option.value ~default:path
  in
  let handle = relativize path in
  let source = {|
        def foo() -> None:
          return 1
    |} |> trim_extra_indentation in
  let assert_response ?(handle = handle) ?state ~request response =
    assert_response ~local_root ~handle ~source ?state ~request (Some response)
  in
  assert_response
    ~request:(Protocol.Request.TypeCheckRequest [file ~local_root path])
    (Protocol.TypeCheckResponse [File.Handle.create_for_testing handle, []]);

  (* The handles get updated in shared memory. *)
  let print_tree tree = File.Handle.Set.Tree.to_list tree |> List.to_string ~f:File.Handle.show in
  assert_equal
    ~printer:print_tree
    (Ast.SharedMemory.HandleKeys.get ())
    (File.Handle.Set.Tree.singleton (File.Handle.create_for_testing handle));
  let files = [file ~local_root ~content:source path] in
  let request_with_content = Protocol.Request.TypeCheckRequest files in
  let errors =
    CommandTest.associate_errors_and_filenames
      (make_errors
         ~local_root
         ~handle
         ~qualifier:(Source.qualifier ~handle:(File.Handle.create_for_testing handle))
         source)
  in
  assert_response ~request:request_with_content (Protocol.TypeCheckResponse errors);

  (* Assert that only files getting used to update the environment get parsed. *)
  let open Protocol in
  assert_response
    ~request:
      (Request.TypeCheckRequest [file ~local_root ~content:"def foo() -> int: return 1" path])
    (Protocol.TypeCheckResponse [File.Handle.create_for_testing (Filename.basename path), []]);
  let () =
    let stub_file = file ~local_root ~content:"" stub_path in
    assert_response
      ~handle:(relativize stub_path)
      ~request:(Request.TypeCheckRequest [stub_file])
      (Protocol.TypeCheckResponse [File.Handle.create_for_testing (Filename.basename stub_path), []]);
    assert_equal
      ~printer:print_tree
      (Ast.SharedMemory.HandleKeys.get ())
      (File.Handle.Set.Tree.singleton (File.Handle.create_for_testing (relativize stub_path)))
  in
  let source = "def foo() -> int: return \"\"" in
  assert_response
    ~request:(Request.TypeCheckRequest [file ~local_root ~content:source stub_path])
    (* We also error on stub files. *)
    (Protocol.TypeCheckResponse
       (CommandTest.associate_errors_and_filenames
          (make_errors
             ~local_root
             ~handle:(relativize stub_path)
             ~qualifier:
               (Source.qualifier ~handle:(File.Handle.create_for_testing (relativize stub_path)))
             source)));
  let file = file ~local_root ~content:"def foo() -> int: return 1" path in
  assert_response
    ~request:(Request.TypeCheckRequest [file])
    (Protocol.TypeCheckResponse [File.Handle.create_for_testing handle, []])


let test_protocol_language_server_protocol context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let server_state = mock_server_state ~local_root (File.Handle.Table.create ()) in
  let { Request.response; _ } =
    Request.process
      ~state:server_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request:(Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
  in
  assert_is_none response


let test_did_save_with_content context =
  let root, filename =
    let root = bracket_tmpdir context |> Path.create_absolute in
    let filename =
      Filename.open_temp_file ~in_dir:(Path.absolute root) "" ".py"
      |> fst
      |> Path.create_absolute
      |> (fun path -> Path.get_relative_to_root ~root ~path)
      |> fun relative -> Option.value_exn relative
    in
    root, filename
  in
  let source = {|
        def foo()->None:
          return 1
    |} |> trim_extra_indentation in
  let qualifier = String.chop_suffix_exn ~suffix:".py" filename |> Reference.create in
  let errors = make_errors ~local_root:root ~handle:filename ~qualifier source in
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
    (Some (Protocol.TypeCheckResponse (CommandTest.associate_errors_and_filenames errors)))


let test_protocol_persistent context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  assert_response
    ~local_root
    ~source:"a = 1"
    ~request:(Protocol.Request.ClientConnectionRequest Protocol.Persistent)
    None


let test_query_dependencies context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let a_source =
    {|
      import b
      def foo() -> str:
        return b.do()
    |}
    |> trim_extra_indentation
  in
  let b_source = {|
    from c import do
    |} |> trim_extra_indentation in
  let c_source = {|
      def do() -> str:
          pass
    |} |> trim_extra_indentation in
  Out_channel.write_all (Path.absolute local_root ^/ "a.py") ~data:a_source;
  Out_channel.write_all (Path.absolute local_root ^/ "b.py") ~data:b_source;
  Out_channel.write_all (Path.absolute local_root ^/ "c.py") ~data:c_source;
  let assert_query_dependencies () =
    let handles =
      [ File.Handle.create_for_testing "a.py";
        File.Handle.create_for_testing "b.py";
        File.Handle.create_for_testing "c.py" ]
    in
    let sources =
      [ parse ~handle:"a.py" ~qualifier:!&"a" a_source;
        parse ~handle:"b.py" ~qualifier:!&"b" b_source;
        parse ~handle:"c.py" ~qualifier:!&"c" c_source ]
    in
    List.zip_exn handles sources
    |> List.iter ~f:(fun (handle, source) -> Ast.SharedMemory.Sources.add handle source);
    let environment = environment ~local_root in
    let configuration = configuration ~local_root in
    let environment_handler = Environment.handler environment in
    add_defaults_to_environment ~configuration environment_handler;
    Test.populate ~configuration environment_handler sources;
    let initial_state =
      mock_server_state ~local_root ~initial_environment:environment (File.Handle.Table.create ())
    in
    let assert_response ~request expected =
      let { Request.response; _ } =
        Request.process
          ~state:initial_state
          ~configuration:(CommandTest.mock_server_configuration ~local_root ())
          ~request
      in
      let printer = function
        | None -> "None"
        | Some response -> Protocol.show_response response
      in
      assert_equal ~printer (Some expected) response
    in
    let reference_response references =
      Protocol.TypeQueryResponse
        (Protocol.TypeQuery.Response (Protocol.TypeQuery.References references))
    in
    assert_response
      ~request:
        (Protocol.Request.TypeQueryRequest
           (Protocol.TypeQuery.DependentDefines [file ~local_root "a.py"]))
      (reference_response []);
    assert_response
      ~request:
        (Protocol.Request.TypeQueryRequest
           (Protocol.TypeQuery.DependentDefines [file ~local_root "b.py"]))
      (reference_response [Reference.create "a.$toplevel"]);
    assert_response
      ~request:
        (Protocol.Request.TypeQueryRequest
           (Protocol.TypeQuery.DependentDefines [file ~local_root "c.py"]))
      (reference_response [Reference.create "a.$toplevel"; Reference.create "b.$toplevel"])
  in
  let finally () =
    Ast.SharedMemory.Sources.remove
      ~handles:[File.Handle.create_for_testing "a.py"; File.Handle.create_for_testing "b.py"]
  in
  Exn.protect ~f:assert_query_dependencies ~finally


let test_incremental_dependencies context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let a_source =
    {|
      import b
      def foo() -> str:
        return b.do()
    |}
    |> trim_extra_indentation
  in
  let b_source = {|
      def do() -> str:
          pass
    |} |> trim_extra_indentation in
  Out_channel.write_all (Path.absolute local_root ^/ "a.py") ~data:a_source;
  Out_channel.write_all (Path.absolute local_root ^/ "b.py") ~data:b_source;
  let assert_dependencies_analyzed () =
    let handles = [File.Handle.create_for_testing "a.py"; File.Handle.create_for_testing "b.py"] in
    let sources =
      [ parse ~handle:"a.py" ~qualifier:!&"a" a_source;
        parse ~handle:"b.py" ~qualifier:!&"b" b_source ]
    in
    List.zip_exn handles sources
    |> List.iter ~f:(fun (handle, source) -> Ast.SharedMemory.Sources.add handle source);
    let environment = environment ~local_root in
    let configuration = configuration ~local_root in
    let environment_handler = Environment.handler environment in
    add_defaults_to_environment ~configuration environment_handler;
    Test.populate ~configuration environment_handler sources;
    let initial_state =
      mock_server_state ~local_root ~initial_environment:environment (File.Handle.Table.create ())
    in
    let process request =
      Request.process
        ~state:initial_state
        ~configuration:(CommandTest.mock_server_configuration ~local_root ())
        ~request
    in
    let { Request.response; _ } =
      process (Protocol.Request.TypeCheckRequest [file ~local_root "b.py"])
    in
    let printer = function
      | None -> "None"
      | Some response -> Protocol.show_response response
    in
    assert_equal
      ~printer
      (Some
         (Protocol.TypeCheckResponse
            [File.Handle.create_for_testing "a.py", []; File.Handle.create_for_testing "b.py", []]))
      response;
    let { Request.response; _ } =
      process
        (Protocol.Request.TypeCheckRequest [file ~local_root "a.py"; file ~local_root "b.py"])
    in
    assert_equal
      ~printer
      (Some
         (Protocol.TypeCheckResponse
            [File.Handle.create_for_testing "a.py", []; File.Handle.create_for_testing "b.py", []]))
      response
  in
  let finally () =
    Ast.SharedMemory.Sources.remove
      ~handles:[File.Handle.create_for_testing "a.py"; File.Handle.create_for_testing "b.py"]
  in
  Exn.protect ~f:assert_dependencies_analyzed ~finally


let test_incremental_lookups context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let path, _ = Filename.open_temp_file ~in_dir:(Path.absolute local_root) "test" ".py" in
  let handle =
    Path.create_relative ~root:local_root ~relative:path
    |> Path.relative
    |> Option.value ~default:path
    |> File.Handle.create_for_testing
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
  let configuration = configuration ~local_root in
  let environment = Environment.Builder.create () in
  let (module Handler : Environment.Handler) = Environment.handler environment in
  let environment_handler = Environment.handler environment in
  Test.populate
    ~configuration
    environment_handler
    (typeshed_stubs ~include_helper_builtins:false ());
  add_defaults_to_environment ~configuration environment_handler;
  Test.populate ~configuration environment_handler [parse source];
  let request = Protocol.Request.TypeCheckRequest [file ~local_root ~content:source path] in
  let errors = File.Handle.Table.create () in
  let initial_state = mock_server_state ~local_root ~initial_environment:environment errors in
  let { Request.state; _ } =
    Request.process
      ~state:initial_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request
  in
  let annotations =
    handle
    |> Ast.SharedMemory.Sources.get
    |> (fun value -> Option.value_exn value)
    |> Lookup.create_of_source state.State.environment
    |> Lookup.get_all_annotations
    |> List.map ~f:(fun (key, data) ->
           Format.asprintf "%a/%a" Location.Instantiated.pp key Type.pp data
           |> String.chop_prefix_exn ~prefix:(File.Handle.show handle))
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    [ ":2:8-2:9/typing.Any";
      ":3:11-3:12/typing_extensions.Literal[1]";
      ":4:8-4:9/typing.Any";
      Format.sprintf
        ":5:4-5:7/typing.Callable(%s.foo)[[Named(x, unknown)], unknown]"
        (Source.qualifier ~handle |> Reference.show);
      ":5:8-5:9/typing.Any";
      ":6:11-6:12/typing_extensions.Literal[2]" ]
    annotations


let test_incremental_repopulate context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let handle = "test_incremental.py" in
  let parse content =
    let handle = File.Handle.create_for_testing handle in
    Source.create
      ~handle
      ~qualifier:(Source.qualifier ~handle)
      (Parser.parse ~handle (String.split_lines (content ^ "\n")))
    |> Analysis.Preprocessing.preprocess
  in
  let source = {|
      def foo(x)->int:
          return 1
    |} |> trim_extra_indentation in
  let environment = Environment.Builder.create () in
  let configuration = configuration ~local_root in
  let ((module Handler : Environment.Handler) as environment_handler) =
    Environment.handler environment
  in
  let resolution = TypeCheck.resolution environment_handler () in
  let path = Path.create_relative ~root:local_root ~relative:handle in
  let file = File.create ~content:source path in
  File.write file;
  add_defaults_to_environment ~configuration environment_handler;
  Service.Parser.parse_sources
    ~configuration
    ~scheduler:(Scheduler.mock ())
    ~preprocessing_state:None
    ~files:[file]
  |> ignore;
  Test.populate ~configuration environment_handler [parse source];
  let errors = File.Handle.Table.create () in
  let initial_state = mock_server_state ~local_root ~initial_environment:environment errors in
  let get_annotation access_name =
    match Resolution.function_definitions resolution !&access_name with
    | Some [{ Node.value = { Statement.Define.signature = { return_annotation; _ }; _ }; _ }] ->
        return_annotation
    | _ -> None
  in
  ( match get_annotation "test_incremental.foo" with
  | Some expression -> assert_equal ~printer:Fn.id (Expression.show expression) "int"
  | None -> assert_unreached () );
  let file =
    File.create
      ~content:
        ({|
          def foo(x)->str:
            return ""
        |} |> trim_extra_indentation)
      path
  in
  File.write file;
  Request.process
    ~state:initial_state
    ~configuration:(CommandTest.mock_server_configuration ~local_root ())
    ~request:(Protocol.Request.TypeCheckRequest [file])
  |> ignore;
  match get_annotation "test_incremental.foo" with
  | Some expression -> assert_equal (Expression.show expression) "str"
  | None -> assert_unreached ()


let test_language_scheduler_definition context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let configuration = Configuration.Analysis.create ~local_root () in
  let filename =
    let path = Path.create_relative ~root:local_root ~relative:"filename.py" in
    File.write (File.create ~content:"" path);
    Path.absolute path
  in
  let request =
    Format.sprintf
      {|
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
      ~configuration
      ~id:(int_request_id 3)
      ~location:None
    |> LanguageServer.Protocol.TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.to_string
  in
  assert_response
    ~local_root
    ~source:"a = 1"
    ~request:(Protocol.Request.LanguageServerProtocolRequest request)
    (Some (Protocol.LanguageServerProtocolResponse expected_response))


let test_incremental_attribute_caching context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let configuration = Configuration.Analysis.create ~local_root ~project_root:local_root () in
  let server_configuration = Operations.create_configuration configuration in
  let environment = Analysis.Environment.Builder.create () |> Analysis.Environment.handler in
  Test.populate ~configuration environment (typeshed_stubs ~include_helper_builtins:false ());
  add_defaults_to_environment ~configuration environment;
  let ({ State.connections; _ } as old_state) =
    mock_server_state ~local_root (File.Handle.Table.create ())
  in
  let source_path = Path.create_relative ~root:local_root ~relative:"a.py" in
  let write_to_file ~content =
    Out_channel.write_all (Path.absolute source_path) ~data:(trim_extra_indentation content)
  in
  let error_printer errors_by_file =
    errors_by_file
    |> List.map ~f:snd
    |> List.concat
    |> List.map ~f:(Error.description ~show_error_traces:false)
    |> String.concat ~sep:"\n"
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
    Server.Operations.start ~old_state ~connections ~configuration:server_configuration ()
  in
  let request_typecheck state =
    Request.process
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.TypeCheckRequest [File.create source_path])
    |> fun { Request.state; _ } -> state
  in
  let get_errors { State.errors; _ } = Hashtbl.to_alist errors in
  let state = request_typecheck { initial_state with State.environment } in
  assert_equal ~printer:error_printer (get_errors state) [];
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
  ( match get_errors state with
  | [(_, [error])] ->
      assert_equal
        ~cmp:String.equal
        ~printer:ident
        "Undefined attribute [16]: `C` has no attribute `a`."
        (Error.description ~show_error_traces:false error)
  | _ -> assert_unreached () );
  write_to_file ~content:content_with_annotation;
  let state = request_typecheck { state with State.environment } in
  assert_equal ~printer:error_printer (get_errors state) []


let () =
  CommandTest.run_command_tests
    "server"
    [ "server_stops", test_server_stops;
      "server_exits_on_directory_removal", test_server_exits_on_directory_removal;
      "compute_hashes_to_keys", test_compute_hashes_to_keys;
      "connect", test_connect;
      "decode_serialized_ocaml_values", test_decode_serialized_ocaml_values;
      "stop_handles_unix_errors", test_stop_handles_unix_errors;
      "json_socket", test_json_socket;
      "protocol_type_check", test_protocol_type_check;
      "protocol_language_server_protocol", test_protocol_language_server_protocol;
      "protocol_persistent", test_protocol_persistent;
      "query", test_query;
      "shutdown", test_shutdown;
      "language_scheduler_shutdown", test_language_scheduler_shutdown;
      "did_save_with_content", test_did_save_with_content;
      "incremental_dependencies", test_incremental_dependencies;
      "query_dependencies", test_query_dependencies;
      "incremental_typecheck", test_incremental_typecheck;
      "incremental_repopulate", test_incremental_repopulate;
      "incremental_lookups", test_incremental_lookups;
      "incremental_attribute_caching", test_incremental_attribute_caching;
      "language_scheduler_definition", test_language_scheduler_definition;
      "language_server_protocol_json_format", test_language_server_protocol_json_format ]
