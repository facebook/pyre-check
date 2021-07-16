(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Expression
open Pyre
open Server
open Test
open CommandTest

let int_request_id id = LanguageServer.Types.RequestId.Int id

let test_language_server_protocol_json_format context =
  let handle = "filename.py" in
  let configuration =
    let project = ScratchProject.setup ~context ~show_error_traces:true [handle, ""] in
    let _ = ScratchProject.parse_sources project in
    ScratchProject.configuration_of project
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let type_error =
    CommandTest.make_errors
      ~context
      ~handle
      ~show_error_traces:true
      {|
        class unittest.mock.Base: ...
        class unittest.mock.NonCallableMock: ...
        def foo() -> None:
          return 1
      |}
    |> List.hd_exn
  in
  let normalize string =
    (* Working around OS inconsitencies. *)
    string
    |> String.split ~on:'\n'
    |> String.concat
    |> String.filter ~f:(fun character -> not (Char.is_whitespace character))
  in
  let path =
    Path.create_relative ~root:local_root ~relative:handle
    |> Path.absolute
    |> Path.create_absolute ~follow_symbolic_links:true
    |> function
    | Path.Absolute path -> path
    | _ -> failwith "Absolute path expected"
  in
  let json_error =
    LanguageServer.Protocol.PublishDiagnostics.of_errors path [type_error]
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
      (Path.create_relative ~root:local_root ~relative:handle |> Path.absolute)
    |> Test.trim_extra_indentation
    |> normalize
  in
  assert_equal ~printer:ident ~cmp:String.equal json_error_expect json_error


let test_server_exits_on_directory_removal context =
  let directory = bracket_tmpdir context in
  let pid =
    Pid.of_int
      (CommandTest.start_server
         ~local_root:(Path.create_absolute ~follow_symbolic_links:true directory)
         ())
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
  Commands.Stop.stop ~log_directory:None ~local_root:long_path |> ignore


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


let test_shutdown context =
  assert_response
    ~context
    ~sources:["test_shutdown.py", "x = 1"]
    ~request:(Protocol.Request.ClientShutdownRequest (int_request_id 1))
    (Some
       (Protocol.LanguageServerProtocolResponse
          (LanguageServer.Protocol.ShutdownResponse.default (int_request_id 1)
          |> LanguageServer.Protocol.ShutdownResponse.to_yojson
          |> Yojson.Safe.to_string)))


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
          (LanguageServer.Protocol.ShutdownResponse.default (int_request_id 2)
          |> LanguageServer.Protocol.ShutdownResponse.to_yojson
          |> Yojson.Safe.to_string)))


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
  let errors = CommandTest.make_errors ~context ~handle source in
  assert_response ~sources:[handle, source] ~request:[] errors;
  assert_response ~sources:[handle, source] ~request:[handle] errors;
  assert_response ~sources:[handle, source] ~request:["wrong_handle.pyi"] [];

  let shadowed_handle = "test_protocol_type_check.py" in
  assert_response ~sources:[handle, source; shadowed_handle, source] ~request:[shadowed_handle] [];
  ()


let test_connect context =
  let local_root = bracket_tmpdir context |> Path.create_absolute ~follow_symbolic_links:true in
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
    Commands.Stop.stop ~log_directory:None ~local_root:(Path.absolute local_root) |> ignore;
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
  let errors = CommandTest.make_errors ~context ~handle source in
  let stub_handle = "test_incremental_typecheck_stub.pyi" in
  let stub_source = "def foo() -> int: return \"\"" in
  let stub_errors = CommandTest.make_errors ~context ~handle:stub_handle stub_source in
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
  assert_response
    ~state
    ~request:(Protocol.Request.TypeCheckRequest [update_file stub_path ~content:stub_source])
    (Protocol.TypeCheckResponse stub_errors)


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
  let errors = CommandTest.make_errors ~context ~handle source in
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start ~context [handle, source]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
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


let test_incremental_dependencies context =
  let { ScratchServer.configuration; server_configuration; state } =
    ScratchServer.start
      ~context
      [
        "ia.py", {|
      import ib
      def foo() -> str:
        return ib.do()
    |};
        "ib.py", {|
      def do() -> str:
          pass
    |};
      ]
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
      [
        ( handle,
          {|
      def foo(x):
          return 1
      def boo(x):
          foo(x)
          return 2
    |}
        );
      ]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let request =
    Protocol.Request.TypeCheckRequest [Path.create_relative ~root:local_root ~relative:handle]
  in
  let { Request.state = { environment; _ }; _ } =
    Request.process ~state ~configuration:server_configuration ~request
  in
  let annotations =
    Lookup.create_of_module (TypeEnvironment.read_only environment) qualifier
    |> Lookup.get_all_annotations
    |> List.map ~f:(fun (key, data) -> Format.asprintf "%a/%a" Location.pp key Type.pp data)
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "2:4-2:7/typing.Callable(test_incremental_lookups.foo)[[Named(x, unknown)], typing.Any]";
      "2:8-2:9/typing.Any";
      "3:11-3:12/typing_extensions.Literal[1]";
      "4:4-4:7/typing.Callable(test_incremental_lookups.boo)[[Named(x, unknown)], typing.Any]";
      "4:8-4:9/typing.Any";
      "5:4-5:10/typing.Any";
      Format.sprintf
        "5:4-5:7/typing.Callable(%s.foo)[[Named(x, unknown)], typing.Any]"
        (Reference.show qualifier);
      "5:8-5:9/typing.Any";
      "6:11-6:12/typing_extensions.Literal[2]";
    ]
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
    let resolution =
      TypeEnvironment.read_only environment |> TypeEnvironment.ReadOnly.global_resolution
    in
    match
      GlobalResolution.function_definitions resolution (Reference.combine qualifier !&access_name)
    with
    | Some [{ Node.value = { Statement.Define.signature = { return_annotation; _ }; _ }; _ }] ->
        return_annotation
    | _ -> None
  in
  (match get_annotation state "foo" with
  | Some expression -> assert_equal ~printer:Fn.id (Expression.show expression) "int"
  | None -> assert_unreached ());
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
    LanguageServer.Protocol.TextDocumentDefinitionResponse.create_empty ~id:(int_request_id 3)
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
    let get_error_strings { State.errors; environment; _ } =
      let ast_environment =
        TypeEnvironment.read_only environment |> TypeEnvironment.ReadOnly.ast_environment
      in
      Hashtbl.to_alist errors
      |> List.map ~f:snd
      |> List.concat
      |> List.map ~f:(fun error ->
             AnalysisError.instantiate
               ~show_error_traces:false
               ~lookup:
                 (AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment)
               error
             |> AnalysisError.Instantiated.description)
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
    [
      "server_exits_on_directory_removal", test_server_exits_on_directory_removal;
      "connect", test_connect;
      "stop_handles_unix_errors", test_stop_handles_unix_errors;
      "protocol_type_check", test_protocol_type_check;
      "protocol_language_server_protocol", test_protocol_language_server_protocol;
      "protocol_persistent", test_protocol_persistent;
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
