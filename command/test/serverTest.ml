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
open Server
open Test
open CommandTest

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
  Ast.SharedMemory.Sources.add (Source.create ~relative:filename []);
  let ({ Error.location; _ } as type_error) =
    CommandTest.make_errors
      ~handle:filename
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
    LanguageServer.Protocol.PublishDiagnostics.of_errors ~configuration filename [type_error]
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
      "nonexistent_file"
      [type_error]
  in
  assert_true (Or_error.is_error malformed_response)


let test_server_stops context =
  let local_root = bracket_tmpdir context |> Pyre.Path.create_absolute in
  let pid = Pid.of_int (CommandTest.start_server ~local_root ()) in
  Commands.Stop.stop ~local_root:(Path.absolute local_root) |> ignore;
  let {
    Configuration.Server.socket = { path = socket_path; _ };
    json_socket = { path = json_socket_path; _ };
    _;
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
  {
    LanguageServer.Types.HandshakeClient.jsonrpc = "2.0";
    method_ = "handshake/client";
    parameters = Some ();
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


let assert_response_equal expected_response response =
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


let assert_response ~context ~sources ~request expected_response =
  let { ScratchServer.server_configuration; state; _ } = ScratchServer.start ~context sources in
  let { Request.response; _ } =
    Request.process ~state ~configuration:server_configuration ~request
  in
  assert_response_equal expected_response response


let assert_query_response ~context ~sources ~query expected_response =
  let { ScratchServer.configuration; server_configuration; state; _ } =
    ScratchServer.start ~context sources
  in
  let request = Query.parse_query ~configuration query in
  let { Request.response; _ } =
    Request.process ~state ~configuration:server_configuration ~request
  in
  assert_response_equal expected_response response


let test_shutdown context =
  assert_response
    ~context
    ~sources:["test_shutdown.py", "x = 1"]
    ~request:(Protocol.Request.ClientShutdownRequest (int_request_id 1))
    (Some
       (Protocol.LanguageServerProtocolResponse
          ( LanguageServer.Protocol.ShutdownResponse.default (int_request_id 1)
          |> LanguageServer.Protocol.ShutdownResponse.to_yojson
          |> Yojson.Safe.to_string )))


let test_language_scheduler_shutdown context =
  assert_response
    ~context
    ~sources:["test_language_scheduler_shutdown.py", "x=1"]
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
  let assert_response ~sources ~request expected =
    let {
      ScratchServer.configuration = { Configuration.Analysis.local_root; _ };
      server_configuration;
      state;
      _;
    }
      =
      ScratchServer.start ~context sources
    in
    let request =
      let paths =
        List.map request ~f:(fun relative -> Path.create_relative ~root:local_root ~relative)
      in
      Protocol.Request.DisplayTypeErrors paths
    in
    let { Request.response; _ } =
      Request.process ~state ~configuration:server_configuration ~request
    in
    let expected_response = Some (Protocol.TypeCheckResponse expected) in
    assert_response_equal expected_response response
  in
  let handle = "test_protocol_type_check.pyi" in
  let source = {|
        def foo() -> None:
          return 1
    |} in
  let errors = CommandTest.make_errors ~handle source in
  assert_response ~sources:[handle, source] ~request:[] errors;
  assert_response ~sources:[handle, source] ~request:[handle] errors;
  assert_response ~sources:[handle, source] ~request:["wrong_handle.pyi"] [];

  let shadowed_handle = "test_protocol_type_check.py" in
  assert_response ~sources:[handle, source; shadowed_handle, source] ~request:[shadowed_handle] []


let test_query context =
  let assert_type_query_response ?handle ~source ~query response =
    let handle = Option.value handle ~default:"test.py" in
    assert_query_response
      ~context
      ~sources:[handle, source]
      ~query
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
      {
        Protocol.TypeQuery.location =
          create_location ~path:"test.py" start_line start_column end_line end_column;
        annotation;
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
    ~query:"less_or_equal(int, test.A)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean true));
  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"less_or_equal(list[test.C], list[int])"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Boolean false));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"join(list[test.C], list[int])"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.Type (parse_annotation "typing.List[typing.Any]")));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"meet(list[test.C], list[int])"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type (Type.list Type.Bottom)));
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"superclasses(test.C)"
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
    ~query:"is_compatible_with(int, test.A)"
    ~actual:Type.integer
    ~expected:Type.integer
    true;
  assert_type_query_response
    ~source:""
    ~query:"is_compatible_with(int, Unknown)"
    (Protocol.TypeQuery.Error "Type `Unknown` was not found in the type order.");
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(int, $unknown)"
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
    ~query:"is_compatible_with(test.A, typing.Coroutine[typing.Any, typing.Any, test.A])"
    ~actual:Type.integer
    ~expected:Type.integer
    true;
  assert_compatibility_response
    ~source:{|
         class A: ...
         class B(A): ...
      |}
    ~query:"is_compatible_with(test.B, typing.Coroutine[typing.Any, typing.Any, test.A])"
    ~actual:(Type.Primitive "test.B")
    ~expected:(Type.Primitive "test.A")
    true;
  assert_compatibility_response
    ~source:{|
         class A: ...
         class B(A): ...
      |}
    ~query:
      ( "is_compatible_with(typing.Type[test.B],"
      ^ "typing.Coroutine[typing.Any, typing.Any, typing.Type[test.A]])" )
    ~actual:(Type.meta (Type.Primitive "test.B"))
    ~expected:(Type.meta (Type.Primitive "test.A"))
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
    ~query:"normalize_type(test.A)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.integer));
  assert_type_query_response
    ~source:
      {|
      class C:
        def C.foo(self) -> int: ...
        def C.bar(self) -> str: ...
    |}
    ~query:"methods(test.C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundMethods
          [ {
              Protocol.TypeQuery.name = "foo";
              parameters = [Type.Primitive "self"];
              return_annotation = Type.integer;
            };
            {
              Protocol.TypeQuery.name = "bar";
              parameters = [Type.Primitive "self"];
              return_annotation = Type.string;
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
          {
            Protocol.TypeQuery.location = create_location ~path:"test.py" 1 4 1 5;
            annotation = Type.literal_integer 2;
          }));
  assert_type_query_response
    ~source:{|
      a: int = 1
      a = 2
    |}
    ~query:"type_at_position('test.py', 3, 0)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypeAtLocation
          {
            Protocol.TypeQuery.location = create_location ~path:"test.py" 3 0 3 1;
            annotation = Type.integer;
          }));
  let assert_type_query_response_with_local_root
      ?(handle = "test.py")
      ~source
      ~query
      build_expected_response
    =
    let { ScratchServer.configuration; server_configuration; state; _ } =
      ScratchServer.start ~context [handle, source]
    in
    let request = Query.parse_query ~configuration query in
    let { Request.response; _ } =
      Request.process ~state ~configuration:server_configuration ~request
    in
    let expected_response =
      let { Configuration.Analysis.local_root; _ } = configuration in
      build_expected_response local_root
    in
    assert_response_equal (Some (Protocol.TypeQueryResponse expected_response)) response
  in
  assert_type_query_response_with_local_root
    ~handle:"test.py"
    ~source:"a = 2"
    ~query:"path_of_module(test)"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.FoundPath
           (Path.create_relative ~root:local_root ~relative:"test.py" |> Path.absolute)));
  assert_type_query_response_with_local_root
    ~handle:"test.pyi"
    ~source:"a = 2"
    ~query:"path_of_module(test)"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.FoundPath
           (Path.create_relative ~root:local_root ~relative:"test.pyi" |> Path.absolute)));
  assert_type_query_response
    ~source:"a = 2"
    ~query:"path_of_module(notexist)"
    (Protocol.TypeQuery.Error "No path found for module `notexist`");
  assert_type_query_response_with_local_root
    ~source:"a = 2"
    ~query:"type_at_position('notexist.py', 1, 1)"
    (fun local_root ->
      Protocol.TypeQuery.Error
        ("Not able to get lookup at " ^ Path.absolute local_root ^/ "notexist.py:1:1"));
  assert_type_query_response_with_local_root
    ~source:"a = 2"
    ~query:"type_at_position('test.py', 1, 3)"
    (fun local_root ->
      Protocol.TypeQuery.Error
        ("Not able to get lookup at " ^ Path.absolute local_root ^/ "test.py:1:3"));

  (* test.py is shadowed by test.pyi *)
  assert_type_query_response_with_local_root
    ~handle:"test.pyi"
    ~source:"a = 2"
    ~query:"type_at_position('test.py', 1, 4)"
    (fun local_root ->
      Protocol.TypeQuery.Error
        ("Not able to get lookup at " ^ Path.absolute local_root ^/ "test.py:1:4"));
  assert_type_query_response_with_local_root
    ~source:{|
      def foo(x: int = 10, y: str = "bar") -> None:
        a = 42
    |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 2, 24, 2, 27, Type.meta Type.string;
                   3, 6, 3, 8, Type.literal_integer 42;
                   2, 21, 2, 22, Type.string;
                   2, 40, 2, 44, Type.none;
                   2, 30, 2, 35, Type.literal_string "bar";
                   2, 11, 2, 14, Type.meta Type.integer;
                   2, 17, 2, 19, Type.literal_integer 10;
                   2, 8, 2, 9, Type.integer;
                   3, 2, 3, 3, Type.literal_integer 42 ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:
      {|
       def foo(x: int, y: str) -> str:
        x = 4
        y = 5
        return x
    |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 3, 5, 3, 6, Type.literal_integer 4;
                   3, 1, 3, 2, Type.integer;
                   2, 27, 2, 30, Type.meta Type.string;
                   4, 1, 4, 2, Type.string;
                   5, 8, 5, 9, Type.integer;
                   2, 16, 2, 17, Type.string;
                   2, 11, 2, 14, Type.meta Type.integer;
                   2, 8, 2, 9, Type.integer;
                   4, 5, 4, 6, Type.literal_integer 5;
                   2, 19, 2, 22, Type.meta Type.string ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:{|
        x = 4
        y = 3
     |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 2, 4, 2, 5, Type.literal_integer 4;
                   3, 0, 3, 1, Type.integer;
                   3, 4, 3, 5, Type.literal_integer 3;
                   2, 0, 2, 1, Type.integer ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:{|
      def foo():
        if True:
         x = 1
    |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 4, 3, 4, 4, Type.literal_integer 1;
                   3, 5, 3, 9, Type.Literal (Boolean true);
                   4, 7, 4, 8, Type.literal_integer 1 ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:{|
       def foo():
         for x in [1, 2]:
          y = 1
     |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 4, 3, 4, 4, Type.literal_integer 1;
                   3, 11, 3, 17, Type.list Type.integer;
                   3, 12, 3, 13, Type.literal_integer 1;
                   3, 6, 3, 7, Type.integer;
                   4, 7, 4, 8, Type.literal_integer 1;
                   3, 15, 3, 16, Type.literal_integer 2 ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:{|
        try:
          x = 1
        except Exception:
          y = 2
      |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 4, 7, 4, 16, Type.parametric "type" (Concrete [Type.Primitive "Exception"]);
                   3, 6, 3, 7, Type.literal_integer 1;
                   5, 6, 5, 7, Type.literal_integer 2;
                   5, 2, 5, 3, Type.literal_integer 2;
                   3, 2, 3, 3, Type.literal_integer 1 ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:{|
       with open() as x:
        y = 2
    |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [3, 5, 3, 6, Type.literal_integer 2; 3, 1, 3, 2, Type.literal_integer 2]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:{|
      while x is True:
        y = 1
   |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 3, 6, 3, 7, Type.literal_integer 1;
                   2, 11, 2, 15, Type.Literal (Boolean true);
                   3, 2, 3, 3, Type.literal_integer 1;
                   2, 6, 2, 15, Type.bool ]
                 |> create_types_at_locations;
             } ]));
  assert_type_query_response_with_local_root
    ~source:
      {|
       def foo(x: int) -> str:
         def bar(y: int) -> str:
           return y
         return x
    |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 3, 13, 3, 16, parse_annotation "typing.Type[int]";
                   3, 10, 3, 11, Type.integer;
                   5, 9, 5, 10, Type.integer;
                   2, 11, 2, 14, parse_annotation "typing.Type[int]";
                   2, 8, 2, 9, Type.integer;
                   4, 11, 4, 12, Type.integer;
                   3, 21, 3, 24, parse_annotation "typing.Type[str]";
                   2, 19, 2, 22, parse_annotation "typing.Type[str]" ]
                 |> create_types_at_locations;
             } ]));

  assert_type_query_response_with_local_root
    ~source:{|
       def foo(x: typing.List[int]) -> None:
        pass
    |}
    ~query:"types(path='test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ 2, 11, 2, 27, Type.meta (Type.list Type.integer);
                   2, 8, 2, 9, Type.list Type.integer;
                   2, 32, 2, 36, Type.none ]
                 |> create_types_at_locations;
             } ]));

  assert_type_query_response_with_local_root
    ~source:{|
       class Foo:
         x = 1
     |}
    ~query:"types('test.py')"
    (fun local_root ->
      Protocol.TypeQuery.Response
        (Protocol.TypeQuery.TypesByFile
           [ {
               Protocol.TypeQuery.path = Path.create_relative ~root:local_root ~relative:"test.py";
               types =
                 [ {
                     Protocol.TypeQuery.location = create_location ~path:"test.py" 3 6 3 7;
                     annotation = Type.literal_integer 1;
                   };
                   {
                     Protocol.TypeQuery.location = create_location ~path:"test.py" 3 2 3 3;
                     annotation = Type.integer;
                   } ];
             } ]));

  assert_type_query_response
    ~source:{|
      class C:
        x = 1
        y = ""
        def foo() -> int: ...
    |}
    ~query:"attributes(test.C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundAttributes
          [ {
              Protocol.TypeQuery.name = "foo";
              annotation =
                Type.Callable
                  {
                    Type.Callable.kind = Type.Callable.Named !&"test.C.foo";
                    implementation =
                      {
                        Type.Callable.annotation = Type.integer;
                        parameters = Type.Callable.Defined [];
                        define_location = None;
                      };
                    overloads = [];
                    implicit = Some { implicit_annotation = Type.Primitive "C"; name = "self" };
                  };
            };
            { Protocol.TypeQuery.name = "x"; annotation = Type.integer };
            { Protocol.TypeQuery.name = "y"; annotation = Type.string } ]));
  ();
  assert_type_query_response
    ~source:{|
      def foo(x: int) -> int:
        pass
    |}
    ~query:"signature(test.foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ {
              Protocol.TypeQuery.return_type = Some Type.integer;
              Protocol.TypeQuery.parameters =
                [ {
                    Protocol.TypeQuery.parameter_name = "x";
                    Protocol.TypeQuery.annotation = Some Type.integer;
                  } ];
            } ]));
  assert_type_query_response
    ~source:{|
      def foo(x) -> int:
        pass
    |}
    ~query:"signature(test.foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ {
              Protocol.TypeQuery.return_type = Some Type.integer;
              Protocol.TypeQuery.parameters =
                [{ Protocol.TypeQuery.parameter_name = "x"; Protocol.TypeQuery.annotation = None }];
            } ]));
  assert_type_query_response
    ~source:{|
      def foo(x: int):
        pass
    |}
    ~query:"signature(test.foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ {
              Protocol.TypeQuery.return_type = None;
              Protocol.TypeQuery.parameters =
                [ {
                    Protocol.TypeQuery.parameter_name = "x";
                    Protocol.TypeQuery.annotation = Some Type.integer;
                  } ];
            } ]));
  assert_type_query_response
    ~source:{|
      alias = int
      def foo(x: alias):
        pass
    |}
    ~query:"signature(test.foo)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundSignature
          [ {
              Protocol.TypeQuery.return_type = None;
              Protocol.TypeQuery.parameters =
                [ {
                    Protocol.TypeQuery.parameter_name = "x";
                    Protocol.TypeQuery.annotation = Some Type.integer;
                  } ];
            } ]));
  assert_type_query_response
    ~source:{|
      x = 1
    |}
    ~query:"signature(test.x)"
    (Protocol.TypeQuery.Error "test.x is not a callable");
  assert_type_query_response
    ~source:""
    ~query:"signature(unknown)"
    (Protocol.TypeQuery.Error "No signature found for unknown");
  assert_type_query_response
    ~source:{|
      foo: str = "bar"
    |}
    ~query:"type(test.foo)"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.string));
  assert_type_query_response
    ~source:{|
      foo = 7
    |}
    ~query:"type(test.foo)"
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
    ~query:"type(test.foo(test.bar))"
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Type Type.string));
  assert_type_query_response
    ~source:{|
      def foo(a: str) -> str:
        return a
      bar: int = 7
    |}
    ~query:"type(test.foo(test.bar))"
    (Protocol.TypeQuery.Error
       ( "Expression had errors: Incompatible parameter type [6]: "
       ^ "Expected `str` for 1st anonymous parameter to call `test.foo` but got `int`." ));

  let temporary_directory = OUnit2.bracket_tmpdir context in
  assert_type_query_response
    ~source:""
    ~query:(Format.sprintf "save_server_state('%s/state')" temporary_directory)
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Success "Saved state."));
  assert_equal `Yes (Sys.is_file (temporary_directory ^/ "state"));

  ()


let test_connect context =
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  CommandTest.start_server ~local_root ~expected_version:"A" () |> ignore;
  let {
    Configuration.Server.configuration;
    socket = { path = socket_path; _ };
    json_socket = { path = json_socket_path; _ };
    _;
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
  let handle = "test_incremental_typecheck.py" in
  let source = {|
        def foo() -> None:
          return 1
        |} in
  let stub_handle = "test_incremental_typecheck_stub.pyi" in
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start ~context [handle, source; stub_handle, ""]
  in
  let assert_response ~state ~request expected_response =
    let { Request.response; _ } =
      Request.process ~state ~configuration:server_configuration ~request
    in
    assert_response_equal (Some expected_response) response
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let path = Path.create_relative ~root:local_root ~relative:handle in
  let stub_path = Path.create_relative ~root:local_root ~relative:stub_handle in
  let errors = CommandTest.make_errors ~handle source in
  assert_response
    ~state
    ~request:(Protocol.Request.TypeCheckRequest [path])
    (Protocol.TypeCheckResponse errors);

  let update_file ~content path =
    let content = trim_extra_indentation content in
    let file = File.create ~content path in
    File.write file;
    path
  in
  assert_response
    ~state
    ~request:
      (Protocol.Request.TypeCheckRequest [update_file path ~content:"def foo() -> int: return 1"])
    (Protocol.TypeCheckResponse []);
  assert_response
    ~state
    ~request:(Protocol.Request.TypeCheckRequest [update_file stub_path ~content:""])
    (Protocol.TypeCheckResponse []);
  let source = "def foo() -> int: return \"\"" in
  let errors = CommandTest.make_errors ~handle:stub_handle source in
  assert_response
    ~state
    ~request:(Protocol.Request.TypeCheckRequest [update_file stub_path ~content:source])
    (Protocol.TypeCheckResponse errors)


let test_protocol_language_server_protocol context =
  let { ScratchServer.server_configuration; state; _ } = ScratchServer.start ~context [] in
  let { Request.response; _ } =
    Request.process
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
  in
  assert_is_none response


let test_did_save_with_content context =
  let handle = "test_did_save_with_content.py" in
  let source = {|
        def foo()->None:
          return 1
    |} |> trim_extra_indentation in
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start ~context [handle, source]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let errors = CommandTest.make_errors ~handle source in
  let request =
    LanguageServer.Protocol.DidSaveTextDocument.create ~root:local_root handle (Some source)
    |> Or_error.ok_exn
    |> LanguageServer.Protocol.DidSaveTextDocument.to_yojson
    |> Yojson.Safe.to_string
  in
  let { Request.response; _ } =
    Request.process
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.LanguageServerProtocolRequest request)
  in
  assert_response_equal (Some (Protocol.TypeCheckResponse errors)) response


let test_protocol_persistent context =
  assert_response
    ~context
    ~sources:["test_protocol_persistent.py", "a = 1"]
    ~request:(Protocol.Request.ClientConnectionRequest Protocol.Persistent)
    None


let test_query_dependencies context =
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start
      ~context
      [ "qa.py", {|
      import qb
      def foo() -> str:
        return qb.do()
    |};
        "qb.py", {|
    |};
        "qb.pyi", {|
    from qc import do
    |};
        "qc.py", {|
      def do() -> str:
          pass
    |} ]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let assert_response ~request expected =
    let { Request.response; _ } =
      Request.process ~state ~configuration:server_configuration ~request
    in
    assert_response_equal (Some expected) response
  in
  let reference_response references =
    Protocol.TypeQueryResponse
      (Protocol.TypeQuery.Response (Protocol.TypeQuery.References references))
  in
  let create_path relative = Path.create_relative ~root:local_root ~relative in
  assert_response
    ~request:
      (Protocol.Request.TypeQueryRequest
         (Protocol.TypeQuery.DependentDefines [create_path "qa.py"]))
    (reference_response []);
  assert_response
    ~request:
      (Protocol.Request.TypeQueryRequest
         (Protocol.TypeQuery.DependentDefines [create_path "nonexistent.py"]))
    (reference_response []);
  assert_response
    ~request:
      (Protocol.Request.TypeQueryRequest
         (Protocol.TypeQuery.DependentDefines [create_path "qb.py"]))
    (reference_response []);
  assert_response
    ~request:
      (Protocol.Request.TypeQueryRequest
         (Protocol.TypeQuery.DependentDefines [create_path "qb.pyi"]))
    (reference_response [Reference.create "qa.$toplevel"]);
  assert_response
    ~request:
      (Protocol.Request.TypeQueryRequest
         (Protocol.TypeQuery.DependentDefines [create_path "qc.py"]))
    (reference_response [Reference.create "qa.$toplevel"; Reference.create "qb.$toplevel"])


let test_incremental_dependencies context =
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start
      ~context
      [ "ia.py", {|
      import ib
      def foo() -> str:
        return ib.do()
    |};
        "ib.py", {|
      def do() -> str:
          pass
    |} ]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let assert_response ~request expected =
    let { Request.response; _ } =
      Request.process ~state ~configuration:server_configuration ~request
    in
    assert_equal (Some expected) response
  in
  let create_path relative = Path.create_relative ~root:local_root ~relative in
  assert_response
    ~request:(Protocol.Request.TypeCheckRequest [create_path "ib.py"])
    (TypeCheckResponse []);
  assert_response
    ~request:(Protocol.Request.TypeCheckRequest [create_path "ia.py"; create_path "ib.py"])
    (TypeCheckResponse [])


let test_incremental_lookups context =
  let handle = "test_incremental_lookups.py" in
  let qualifier = Ast.SourcePath.qualifier_of_relative handle in
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start
      ~context
      [ ( handle,
          {|
      def foo(x):
          return 1
      def boo(x):
          foo(x)
          return 2
    |}
        ) ]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let request =
    Protocol.Request.TypeCheckRequest [Path.create_relative ~root:local_root ~relative:handle]
  in
  let { Request.state; _ } = Request.process ~state ~configuration:server_configuration ~request in
  let global_resolution = Environment.resolution state.State.environment () in
  let annotations =
    Ast.SharedMemory.Sources.get qualifier
    |> (fun value -> Option.value_exn value)
    |> Lookup.create_of_source global_resolution
    |> Lookup.get_all_annotations
    |> List.map ~f:(fun (key, data) ->
           Format.asprintf "%a/%a" Location.Instantiated.pp key Type.pp data
           |> String.chop_prefix_exn ~prefix:handle)
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    [ ":2:8-2:9/typing.Any";
      ":3:11-3:12/typing_extensions.Literal[1]";
      ":4:8-4:9/typing.Any";
      Format.sprintf
        ":5:4-5:7/typing.Callable(%s.foo)[[Named(x, unknown)], unknown]"
        (Reference.show qualifier);
      ":5:8-5:9/typing.Any";
      ":6:11-6:12/typing_extensions.Literal[2]" ]
    annotations


let test_incremental_repopulate context =
  let handle = "test_incremental_repopulate.py" in
  let qualifier = Ast.SourcePath.qualifier_of_relative handle in
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start ~context [handle, {|
      def foo(x)->int:
          return 1
    |}]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let get_annotation { State.environment; _ } access_name =
    let resolution = Environment.resolution environment () in
    match
      GlobalResolution.function_definitions resolution (Reference.combine qualifier !&access_name)
    with
    | Some [{ Node.value = { Statement.Define.signature = { return_annotation; _ }; _ }; _ }] ->
        return_annotation
    | _ -> None
  in
  ( match get_annotation state "foo" with
  | Some expression -> assert_equal ~printer:Fn.id (Expression.show expression) "int"
  | None -> assert_unreached () );
  let path = Path.create_relative ~root:local_root ~relative:handle in
  let file =
    File.create
      path
      ~content:
        ({|
          def foo(x)->str:
            return ""
        |} |> trim_extra_indentation)
  in
  File.write file;
  let { Request.state; _ } =
    Request.process
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.TypeCheckRequest [path])
  in
  match get_annotation state "foo" with
  | Some expression -> assert_equal (Expression.show expression) "str"
  | None -> assert_unreached ()


let test_language_scheduler_definition context =
  let handle = "test_language_scheduler_definition.py" in
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start ~context [handle, ""]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let path = Path.create_relative ~root:local_root ~relative:handle in
  let request =
    Format.asprintf
      {|
      {
        "jsonrpc": "2.0",
        "method": "textDocument/definition",
        "id": 3,
        "params": {
          "textDocument": {
            "uri": "file://%a"
          },
          "position": {
            "line": 5,
            "character": 7
          }
        }
      }
      |}
      Path.pp
      path
  in
  let expected_response =
    LanguageServer.Protocol.TextDocumentDefinitionResponse.create
      ~configuration
      ~id:(int_request_id 3)
      ~location:None
    |> LanguageServer.Protocol.TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.to_string
  in
  let { Request.response; _ } =
    Request.process
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.LanguageServerProtocolRequest request)
  in
  assert_response_equal response (Some (Protocol.LanguageServerProtocolResponse expected_response))


let test_incremental_attribute_caching context =
  let handle = "test_incremental_attribute_caching.py" in
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
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start ~context [handle, content_with_annotation]
  in
  let assert_errors ~state expected =
    let get_error_strings { State.errors; _ } =
      Hashtbl.to_alist errors
      |> List.map ~f:snd
      |> List.concat
      |> List.map ~f:(Error.description ~show_error_traces:false)
    in
    let printer = String.concat ~sep:"\n" in
    assert_equal ~printer expected (get_error_strings state)
  in
  assert_errors ~state [];

  let update_and_request_typecheck ~state content =
    let { Configuration.Analysis.local_root; _ } = configuration in
    let content = trim_extra_indentation content in
    let path = Path.create_relative ~root:local_root ~relative:handle in
    let file = File.create ~content path in
    File.write file;
    Request.process
      ~state
      ~configuration:server_configuration
      ~request:(Protocol.Request.TypeCheckRequest [path])
    |> fun { Request.state; _ } -> state
  in
  let state = update_and_request_typecheck ~state content_without_annotation in
  assert_errors ~state ["Undefined attribute [16]: `C` has no attribute `a`."];

  let state = update_and_request_typecheck ~state content_with_annotation in
  assert_errors ~state []


let () =
  CommandTest.run_command_tests
    "server"
    [ "server_stops", test_server_stops;
      "server_exits_on_directory_removal", test_server_exits_on_directory_removal;
      "connect", test_connect;
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
