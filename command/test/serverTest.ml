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


let file ~local_root ?content path =
  File.create ?content (Path.create_relative ~root:local_root ~relative:path)


let test_language_server_protocol_json_format context =
  let open TypeCheck.Error in
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let configuration = Configuration.Analysis.create ~local_root () in
  let filename =
    let path = Path.create_relative ~root:local_root ~relative:"filename.py" in
    File.write (File.create ~content:"" path);
    "filename.py"
  in
  let handle = File.Handle.create filename in
  Ast.SharedMemory.Sources.add
    handle
    (Source.create ~handle []);
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
      ~configuration
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
      (Path.create_relative ~root:local_root ~relative:filename
       |> Path.absolute)
    |> Test.trim_extra_indentation
    |> normalize
  in
  assert_equal ~printer:ident ~cmp:String.equal json_error_expect json_error;

  let malformed_response =
    LanguageServer.Protocol.PublishDiagnostics.of_errors
      ~configuration
      (File.Handle.create "nonexistent_file")
      [type_error]
  in
  assert_true (Or_error.is_error malformed_response)


let test_server_stops context =
  let local_root =
    bracket_tmpdir context
    |> Pyre.Path.create_absolute
  in
  let pid = Pid.of_int (CommandTest.start_server ~local_root ()) in
  Commands.Stop.stop ~local_root:(Path.absolute local_root)
  |> ignore;
  let { Configuration.Server.socket_path; _ } =
    Operations.create_configuration (Configuration.Analysis.create ~local_root ())
  in
  CommandTest.with_timeout ~seconds:3 CommandTest.poll_for_deletion socket_path;
  CommandTest.with_timeout
    ~seconds:1
    (fun () -> (match Unix.waitpid pid with
         | Ok _ -> assert true
         | Error _ -> assert false))
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
       | Ok _
       (* I was only able to get non-zero exits in the OUnit test environment,
             doing the equivalent calls in the command line always resulted in an exit of 0. *)
       | Error (`Exit_non_zero 2) -> assert true
       | _ -> assert false
    )
    ()


let test_stop_handles_unix_errors context =
  let long_path = bracket_tmpdir ~suffix:(String.init ~f:(fun _ -> 'a') 140) context in
  Commands.Stop.stop ~local_root:long_path
  |> ignore


let configuration ~local_root = Configuration.Analysis.create ~local_root ~infer:true ()


let environment ~local_root =
  let environment = Environment.Builder.create () in
  let configuration = configuration ~local_root in
  Service.Environment.populate
    ~configuration
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


let make_errors ~local_root ?(handle = "test.py") ?(qualifier = []) source =
  let configuration = CommandTest.mock_analysis_configuration () in
  let source = Preprocessing.preprocess (parse ~handle ~qualifier source) in
  let environment = Environment.handler ~configuration (environment ~local_root) in
  add_defaults_to_environment ~configuration environment;
  Service.Environment.populate ~configuration environment [source];
  let { TypeCheck.Result.errors; _ } =
    TypeCheck.check
      ~configuration
      ~environment
      ~source
  in
  errors

let mock_server_state
    ~local_root
    ?(initial_environment = environment ~local_root)
    errors =
  let configuration = configuration ~local_root in
  let environment =
    Environment.handler ~configuration initial_environment
  in
  add_defaults_to_environment ~configuration environment;
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
    ~local_root
    ?state
    ?(handle = "test.py")
    ~source
    ~request
    expected_response =
  Ast.SharedMemory.HandleKeys.clear ();
  Ast.SharedMemory.Sources.remove ~handles:[File.Handle.create handle];
  let parsed = parse ~handle source |> Preprocessing.preprocess in
  Ast.SharedMemory.Sources.add (File.Handle.create handle) parsed;
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      (make_errors ~local_root ~handle source)
      ~f:(fun error ->
          Hashtbl.add_multi errors ~key:(File.Handle.create (Error.path error)) ~data:error);
    errors
  in
  let initial_environment =
    let environment = environment ~local_root in
    let configuration = configuration ~local_root in
    Service.Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      [parsed];
    environment
  in
  let mock_server_state =
    match state with
    | Some state -> state
    | None -> mock_server_state ~initial_environment ~local_root errors
  in
  let { Request.response; _ } =
    Request.process
      ~socket:mock_client_socket
      ~state:mock_server_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request
  in
  let printer = function
    | None -> "None"
    | Some response -> Protocol.show_response response
  in
  let pp_opt formatter =
    function
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
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let source = "x = 1" in
  assert_response
    ~local_root
    ~source
    ~request:(Protocol.Request.ClientShutdownRequest 1)
    (Some
       (Protocol.LanguageServerProtocolResponse
          (LanguageServer.Protocol.ShutdownResponse.default 1
           |> LanguageServer.Protocol.ShutdownResponse.to_yojson
           |> Yojson.Safe.to_string)))


let test_language_scheduler_shutdown context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let source = "x = 1" in
  assert_response
    ~local_root
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


let test_protocol_type_check context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let source =
    {|
        def foo() -> None:
          return 1
    |}
  in
  let errors = make_errors ~local_root source in
  assert_response
    ~local_root
    ~source
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Some (Protocol.TypeCheckResponse (CommandTest.associate_errors_and_filenames errors)));

  assert_response
    ~local_root
    ~source
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Some (Protocol.TypeCheckResponse (CommandTest.associate_errors_and_filenames errors)))


let test_query context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let assert_type_query_response ~source ~query response =
    let query = Commands.Query.parse_query ~root:local_root query in
    assert_response ~local_root ~source ~request:query (Some (Protocol.TypeQueryResponse response))
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
    ~query:"type_at_position('test.py', 1, 4)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypeAtLocation
          {
            Protocol.TypeQuery.location = create_location ~path:"test.py" 1 4 1 5;
            annotation = Type.integer;
          }));
  assert_type_query_response
    ~source:"a = 2"
    ~query:"type_at_position('test.py', 1, 3)"
    (Protocol.TypeQuery.Error
       ("Not able to get lookup at " ^ (Path.absolute local_root) ^/ "test.py:1:3"));

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

  assert_type_query_response
    ~source:{|
      def foo(x: int = 10, y: str = "bar") -> None:
        a = 42
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (3, 6, 3, 8, Type.integer);
            (2, 24, 2, 27, Type.meta Type.string);
            (2, 21, 2, 22, Type.string);
            (2, 40, 2, 44, Type.none);
            (2, 17, 2, 19, Type.integer);
            (3, 2, 3, 3, Type.integer);
            (2, 30, 2, 35, Type.string);
            (2, 11, 2, 14, Type.meta Type.integer);
            (2, 8, 2, 9, Type.integer);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
       def foo(x: int, y: str) -> str:
        x = 4
        y = 5
        return x
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (2, 19, 2, 22, Type.meta Type.string);
            (5, 8, 5, 9, Type.integer);
            (2, 27, 2, 30, Type.meta Type.string);
            (4, 1, 4, 2, Type.string);
            (4, 5, 4, 6, Type.integer);
            (3, 1, 3, 2, Type.integer);
            (2, 11, 2, 14, Type.meta Type.integer);
            (3, 5, 3, 6, Type.integer);
            (2, 8, 2, 9, Type.integer);
            (2, 16, 2, 17, Type.string);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
        x = 4
        y = 3
     |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (2, 4, 2, 5, Type.integer);
            (2, 0, 2, 1, Type.integer);
            (3, 0, 3, 1, Type.integer);
            (3, 4, 3, 5, Type.integer);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
      def foo():
        if True:
         x = 1
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (3, 5, 3, 9, Type.bool);
            (4, 3, 4, 4, Type.integer);
            (4, 7, 4, 8, Type.integer);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
       def foo():
         for x in [1, 2]:
          y = 1
     |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (3, 12, 3, 13, Type.integer);
            (3, 15, 3, 16, Type.integer);
            (3, 6, 3, 7, Type.list Type.integer);
            (4, 3, 4, 4, Type.integer);
            (4, 7, 4, 8, Type.integer);
          ] |> create_types_at_locations)
       ));

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
          ([
            (5, 2, 5, 3, Type.integer);
            (3, 2, 3, 3, Type.integer);
            (3, 6, 3, 7, Type.integer);
            (5, 6, 5, 7, Type.integer);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
       with open() as x:
        y = 2
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (3, 1, 3, 2, Type.integer);
            (3, 5, 3, 6, Type.integer);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
      while x is True:
        y = 1
   |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (2, 11, 2, 15, Type.bool);
            (2, 6, 2, 7, Type.bool);
            (3, 2, 3, 3, Type.integer);
            (3, 6, 3, 7, Type.integer);
          ] |> create_types_at_locations)
       ));

  assert_type_query_response
    ~source:{|
       def foo(x: int) -> str:
         def bar(y: int) -> str:
           return y
         return x
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (2, 19, 2, 22, parse_annotation "typing.Type[str]");
            (5, 9, 5, 10, Type.integer);
            (3, 21, 3, 24, parse_annotation "typing.Type[str]");
            (3, 13, 3, 16, parse_annotation "typing.Type[int]");
            (4, 11, 4, 12, Type.integer);
            (2, 11, 2, 14, parse_annotation "typing.Type[int]");
            (2, 8, 2, 9, Type.integer);
            (3, 10, 3, 11, Type.integer);
          ] |> create_types_at_locations)
       ));

  (* ==== Documenting known bad behavior below (T37772879) ==== *)

  (* Annotation type is Type[int] rather than Type[List[int]]. *)
  assert_type_query_response
    ~source:{|
       def foo(x: typing.List[int]) -> None:
        pass
    |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (2, 32, 2, 36, Type.none);
            (2, 23, 2, 26, Type.meta Type.integer);
            (2, 8, 2, 9, Type.list Type.integer);
          ] |> create_types_at_locations)
       ));

  (* Interprets this assignment as `FooFoo.x = 1` and insanity ensues. *)
  assert_type_query_response
    ~source:{|
       class Foo:
         x = 1
     |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          [
            {
              Protocol.TypeQuery.location = create_location ~path:"test.py" 3 2 3 3;
              annotation = Type.integer
            };
            {
              Protocol.TypeQuery.location = create_location ~path:"test.py" 3 2 3 3;
              annotation = parse_annotation "typing.Type[Foo]"
            };
            {
              Protocol.TypeQuery.location = create_location ~path:"test.py" 3 6 3 7;
              annotation = Type.integer
            };
          ]
       ));

  (* `x` is typed as List[int] rather than int. *)
  assert_type_query_response
    ~source:{|
        for x in [1, 2]:
          pass
      |}
    ~query:"types_in_file('test.py')"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.TypesAtLocations
          ([
            (2, 4, 2, 5, Type.list Type.integer);
            (2, 13, 2, 14, Type.integer);
            (2, 10, 2, 11, Type.integer);
          ] |> create_types_at_locations)
       ));

  (* ==== Documenting known bad behavior above (T37772879) ==== *)

  assert_type_query_response
    ~source:{|
      class C:
        x = 1
        y = ""
        def foo() -> int: ...
    |}
    ~query:"attributes(C)"
    (Protocol.TypeQuery.Response
       (Protocol.TypeQuery.FoundAttributes [
           {
             Protocol.TypeQuery.name = "foo";
             annotation = Type.Callable {
                 Type.Callable.kind = Type.Callable.Named (Access.create "C.foo");
                 implementation = {
                   Type.Callable.annotation = Type.integer;
                   parameters = Type.Callable.Defined [];
                 };
                 overloads = [];
                 implicit = Some {
                     implicit_annotation = Type.primitive "C";
                     name = Access.create "self";
                   };
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
       ("Expression had errors: Incompatible parameter type [6]: " ^
        "Expected `str` for 1st anonymous parameter to call `foo` but got `int`.")
    );

  let temporary_directory = OUnit2.bracket_tmpdir context in
  assert_type_query_response
    ~source:""
    ~query:(Format.sprintf "save_server_state('%s/state')" temporary_directory)
    (Protocol.TypeQuery.Response (Protocol.TypeQuery.Success ()));

  assert_equal `Yes (Sys.is_file (temporary_directory ^/ "state"))



let test_connect context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  CommandTest.start_server ~local_root ~expected_version:"A" () |> ignore;
  let { Configuration.Server.configuration; socket_path; _ } =
    CommandTest.mock_server_configuration ~local_root ~expected_version:"B" ()
  in
  (* This sleep ensures that the server doesn't receive an EPIPE while the Hack_parallel library is
   * initializing the daemon in the hack_parallel/utils/handle.ml. In that codepath, an external
   * routine is called, and due to the nature of the Lazy library this is non-reentrant. *)
  Unix.nanosleep 0.5
  |> ignore;
  let cleanup () =
    Commands.Stop.stop ~local_root:(Path.absolute local_root)
    |> ignore;
    CommandTest.with_timeout CommandTest.poll_for_deletion socket_path ~seconds:3;
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


let test_incremental_typecheck context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let path_to_python_file prefix =
    Filename.open_temp_file
      ~in_dir:(Path.absolute local_root)
      prefix ".py"
    |> fst
  in
  let path = path_to_python_file "test" in
  let stub_path = path_to_python_file "stub" in
  Out_channel.write_all (stub_path ^ "i") ~data:"";

  let relativize path =
    Path.create_relative ~root:local_root ~relative:path
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
    assert_response ~local_root ~handle:path ~source ?state ~request (Some response)
  in
  assert_response
    ~request:(Protocol.Request.TypeCheckRequest
                (Protocol.TypeCheckRequest.create
                   ~update_environment_with:[file ~local_root path]
                   ~check:[file ~local_root path]
                   ()))
    (Protocol.TypeCheckResponse [(File.Handle.create handle), []]);
  (* The handles get updated in shared memory. *)
  assert_equal
    ~printer:(List.to_string ~f:File.Handle.show)
    (Ast.SharedMemory.HandleKeys.get ())
    [File.Handle.create handle];

  let files = [file ~local_root ~content:source path] in
  let request_with_content =
    (Protocol.Request.TypeCheckRequest
       (Protocol.TypeCheckRequest.create ~update_environment_with:files ~check:files ()))
  in
  let errors =
    CommandTest.associate_errors_and_filenames
      (make_errors
         ~local_root
         ~handle
         ~qualifier:(Source.qualifier ~handle:(File.Handle.create handle))
         source)
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
    ~request:(check_request ~check:[file ~local_root ~content:"def foo() -> int: return 1" path] ())
    (Protocol.TypeCheckResponse errors);
  let () =
    let stub_file = file ~local_root ~content:"" (stub_path ^ "i") in
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
        ~check:[file ~local_root ~content:"def foo() -> int: return \"\"" stub_path]
        ())
    (Protocol.TypeCheckResponse [File.Handle.create stub_handle, []]);

  let () =
    let file = file ~local_root ~content:"def foo() -> int: return 1" path in
    assert_response
      ~request:(check_request ~update_environment_with:[file] ~check:[file] ())
      (Protocol.TypeCheckResponse [File.Handle.create handle, []])
  in

  let state = mock_server_state ~local_root (File.Handle.Table.create ()) in
  assert_response
    ~state
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Protocol.TypeCheckResponse []);
  assert_response
    ~state:{ state with State.deferred_requests = [request_with_content] }
    ~request:Protocol.Request.FlushTypeErrorsRequest
    (Protocol.TypeCheckResponse errors)


let test_protocol_language_server_protocol context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let server_state = mock_server_state ~local_root (File.Handle.Table.create ()) in
  let { Request.response; _ } =
    Request.process
      ~socket:mock_client_socket
      ~state:server_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request:(Protocol.Request.LanguageServerProtocolRequest "{\"method\":\"\"}")
  in
  assert_is_none response


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
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  assert_response
    ~local_root
    ~source:"a = 1"
    ~request:(Protocol.Request.ClientConnectionRequest Protocol.Persistent)
    None


let test_incremental_dependencies context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
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
  Out_channel.write_all (Path.absolute local_root ^/ "a.py") ~data:a_source;
  Out_channel.write_all (Path.absolute local_root ^/ "b.py") ~data:b_source;
  let assert_dependencies_analyzed () =
    let handles =
      [
        File.Handle.create "a.py";
        File.Handle.create "b.py";
      ]
    in
    let sources =
      [
        parse ~handle:"a.py" ~qualifier:(Access.create "a") a_source;
        parse ~handle:"b.py" ~qualifier:(Access.create "b") b_source;
      ]
    in
    List.zip_exn handles sources
    |> List.iter ~f:(fun (handle, source) -> Ast.SharedMemory.Sources.add handle source);

    let environment = environment ~local_root in
    let configuration = configuration ~local_root in
    let environment_handler =
      Environment.handler ~configuration environment
    in
    add_defaults_to_environment ~configuration environment_handler;
    Service.Environment.populate ~configuration environment_handler sources;
    let expected_errors = [
      File.Handle.create "b.py", [];
    ]
    in
    let initial_state =
      mock_server_state
        ~local_root
        ~initial_environment:environment
        (File.Handle.Table.create ())
    in
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
        ~configuration:(CommandTest.mock_server_configuration ~local_root ())
        ~request
    in
    let { Request.state; response } =
      process (check_request ~update:[file ~local_root "b.py"] ~check:[file ~local_root "b.py"] ())
    in
    assert_equal (Some (Protocol.TypeCheckResponse expected_errors)) response;
    assert_equal
      ~printer:(List.to_string ~f:Server.Protocol.Request.show)
      [check_request
         ~check:[file ~local_root "a.py"]
         ~update:[file ~local_root "a.py"]
         ()]
      state.State.deferred_requests;
    let { Request.state; response } =
      process
        (check_request
           ~update:[file ~local_root "b.py"]
           ~check:[file ~local_root "a.py"; file ~local_root "b.py"]
           ())
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
  let configuration = configuration ~local_root:(Path.current_working_directory ()) in
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let environment_handler = Environment.handler ~configuration environment in
  add_defaults_to_environment ~configuration environment_handler;
  Service.Environment.populate ~configuration environment_handler [parse source];

  let request =
    Protocol.Request.TypeCheckRequest
      (Protocol.TypeCheckRequest.create
         ~update_environment_with:[
           file
             ~local_root:(Path.current_working_directory ())
             ~content:source path;
         ]
         ~check:[file ~local_root:(Path.current_working_directory ()) ~content:source path]
         ())
  in
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~local_root:(Path.current_working_directory ())
      ~initial_environment:environment
      errors
  in
  let { Request.state; _ } =
    Request.process
      ~socket:mock_client_socket
      ~state:initial_state
      ~configuration:(
        CommandTest.mock_server_configuration
          ~local_root:(Path.current_working_directory ())
          ())
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
    [
      ":3:11-3:12/int";
      Format.sprintf
        ":5:4-5:7/typing.Callable(%s.foo)[[Named(x, unknown)], unknown]"
        (Source.qualifier ~handle
         |> Access.show);
      ":6:11-6:12/int";
    ]
    annotations



let test_incremental_repopulate context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
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
  let configuration = configuration ~local_root in
  let ((module Handler: Environment.Handler) as environment_handler) =
    Environment.handler ~configuration environment
  in
  Out_channel.write_all ~data:source (Path.absolute local_root ^/ "test.py");
  add_defaults_to_environment ~configuration environment_handler;
  Service.Environment.populate ~configuration environment_handler [parse source];
  let errors = File.Handle.Table.create () in
  let initial_state =
    mock_server_state
      ~local_root
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
  Out_channel.write_all ~data:source (Path.absolute local_root ^/ "test.py");
  let _ =
    Request.process
      ~socket:mock_client_socket
      ~state:initial_state
      ~configuration:(CommandTest.mock_server_configuration ~local_root ())
      ~request:(Protocol.Request.TypeCheckRequest
                  (Protocol.TypeCheckRequest.create
                     ~update_environment_with:[file ~local_root "test.py"]
                     ~check:[file ~local_root "test.py"]
                     ()))
  in
  begin match (get_annotation "test.foo") with
    | Some expression -> assert_equal (Expression.show expression) "str"
    | None -> assert_unreached ()
  end


let test_language_scheduler_definition context =
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let configuration = Configuration.Analysis.create ~local_root () in
  let filename =
    let path = Path.create_relative ~root:local_root ~relative:"filename.py" in
    File.write (File.create ~content:"" path);
    Path.absolute path
  in
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
      ~configuration
      ~id:3
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
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let configuration =
    Configuration.Analysis.create ~local_root ~project_root:local_root ()
  in
  let server_configuration = Operations.create_configuration configuration in
  let environment =
    Analysis.Environment.Builder.create ()
    |> Analysis.Environment.handler ~configuration
  in
  add_defaults_to_environment ~configuration environment;
  let ({ State.connections; lock = server_lock; _ } as old_state) =
    mock_server_state ~local_root (File.Handle.Table.create ())
  in
  let source_path = Path.create_relative ~root:local_root ~relative:"a.py" in
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
