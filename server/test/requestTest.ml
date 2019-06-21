(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Pyre
open Server
open Test

let int_request_id id = LanguageServer.Types.RequestId.Int id

let string_request_id id = LanguageServer.Types.RequestId.String id

let mock_server_state
    ?(persistent_clients = []) ?(sources = []) ?(errors = File.Handle.Table.create ()) ()
  =
  let configuration = Test.mock_configuration in
  let environment =
    let environment =
      let environment = Analysis.Environment.Builder.create () in
      Test.populate
        ~configuration
        (Analysis.Environment.handler environment)
        (typeshed_stubs () @ sources);
      environment
    in
    Analysis.Environment.handler environment
  in
  let persistent_clients = Network.Socket.Map.of_alist_exn persistent_clients in
  add_defaults_to_environment ~configuration environment;
  { State.environment;
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
              persistent_clients;
              file_notifiers = []
            }
      };
    scheduler = Scheduler.mock ();
    open_documents = Path.Set.empty
  }


let mock_server_configuration ~configuration =
  let mock_path = Path.create_relative ~root:(Path.create_absolute "/tmp") ~relative:"mock" in
  let mock_socket_path : Configuration.Server.socket_path =
    { path = mock_path; link = mock_path }
  in
  { Configuration.Server.socket = mock_socket_path;
    json_socket = mock_socket_path;
    daemonize = false;
    saved_state_action = None;
    lock_path = mock_path;
    pid_path = mock_path;
    log_path = mock_path;
    configuration
  }


let initialize ?configuration sources =
  let configuration =
    match configuration with
    | Some configuration -> configuration
    | None -> Configuration.Analysis.create ~project_root:(Path.current_working_directory ()) ()
  in
  let state =
    let files = List.map sources ~f:write_file in
    let scheduler = Scheduler.mock () in
    (* Clear and re-populate ASTs in shared memory. *)
    let handles = List.map files ~f:(File.handle ~configuration) in
    SharedMemory.Sources.remove ~handles;
    SharedMemory.Modules.remove
      ~qualifiers:(List.map handles ~f:(fun handle -> Source.qualifier ~handle));
    Service.Parser.parse_sources ~configuration ~scheduler ~preprocessing_state:None ~files
    |> ignore;
    let add_module handle =
      match SharedMemory.Sources.get handle with
      | Some { Ast.Source.handle; statements; metadata = { local_mode; _ }; _ } ->
          SharedMemory.Modules.add
            ~qualifier:(Source.qualifier ~handle)
            ~ast_module:
              (Module.create
                 ~qualifier:(Source.qualifier ~handle)
                 ~local_mode
                 ~handle
                 ~stub:false
                 statements)
      | None -> ()
    in
    List.iter handles ~f:add_module;

    (* Initialize dependency map. *)
    let source (path, content) =
      let handle = File.Handle.create_for_testing path in
      parse ~qualifier:(Source.qualifier ~handle) ~handle:path content
      |> Analysis.Preprocessing.qualify
    in
    mock_server_state ~sources:(List.map sources ~f:source) ()
  in
  configuration, state


let test_generate_lsp_response _ =
  let open LanguageServer.Types in
  let module MockResponse = struct
    module MockResult = struct
      type t = int [@@deriving yojson]
    end

    module MockError = ResponseError.Make (Null)
    include ResponseMessage.Make (MockResult) (MockError)

    let create ~id payload = { jsonrpc = "2.0"; id; result = payload; error = None }
  end
  in
  let assert_response id payload expected_response =
    let actual_response =
      MockResponse.create ~id payload |> MockResponse.to_yojson |> Yojson.Safe.to_string
    in
    let expected_response =
      expected_response |> Yojson.Safe.from_string |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  assert_response (int_request_id 1) (Some 1337) {|{"jsonrpc":"2.0","id":1,"result":1337}|};
  assert_response (string_request_id "abcd") None {|{"jsonrpc":"2.0","id":"abcd","result":null}|}


let test_process_client_shutdown_request _ =
  let assert_response id expected_response =
    let state = mock_server_state () in
    let actual_response =
      Request.process_client_shutdown_request ~state ~id
      |> function
      | { Request.response = Some (Protocol.LanguageServerProtocolResponse response); _ } ->
          response
      | _ -> failwith "Unexpected response."
    in
    let expected_response =
      expected_response |> Yojson.Safe.from_string |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  assert_response (int_request_id 0) {|{"jsonrpc":"2.0","id":0,"result":null}|};
  assert_response (string_request_id "xyz") {|{"jsonrpc":"2.0","id":"xyz","result":null}|}


let test_process_type_query_request _ =
  let assert_response request expected_response =
    let state = mock_server_state () in
    let configuration =
      Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ()
    in
    let actual_response =
      Request.process_type_query_request ~state ~configuration ~request
      |> function
      | { Request.response = Some (Protocol.TypeQueryResponse response); _ } ->
          Protocol.TypeQuery.response_to_yojson response |> Yojson.Safe.to_string
      | _ -> failwith "Unexpected response."
    in
    let expected_response =
      expected_response |> Yojson.Safe.from_string |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  let coverage_check_file =
    "test.py", {|
        def foo(a: int) -> int:
          return a
      |}
  in
  assert_response
    (Protocol.TypeQuery.CoverageInFile (write_file coverage_check_file))
    {| {"response":{"types":[]}}|};
  assert_response
    (Protocol.TypeQuery.Join (parse_single_expression "int", parse_single_expression "float"))
    {|{"response":{"type":"float"}}|};
  assert_response
    (Protocol.TypeQuery.NormalizeType (parse_single_expression "yerp"))
    {|{"error":"Type `yerp` was not found in the type order."}|}


let assert_errors_equal ~actual_errors ~expected_errors =
  let actual_errors =
    let errors (handle, errors) =
      ( File.Handle.show handle,
        List.map errors ~f:(Analysis.Error.description ~show_error_traces:false) )
    in
    List.map actual_errors ~f:errors
  in
  let equal =
    let equal left right =
      String.equal (fst left) (fst right)
      && List.equal
           (snd left |> List.sort ~compare:String.compare)
           (snd right |> List.sort ~compare:String.compare)
           ~equal:String.equal
    in
    List.equal ~equal
  in
  let printer errors =
    let show (path, errors) = Format.asprintf "%s: [%s]" path (String.concat errors ~sep:", ") in
    List.map errors ~f:show |> String.concat ~sep:"\n"
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
                { Analysis.Error.location;
                  kind = Analysis.Error.UndefinedName (Reference.create undefined);
                  signature = +mock_signature
                }
              in
              List.map errors ~f:error
            in
            File.Handle.create_for_testing path, errors
          in
          List.map errors ~f:entry
        in
        mock_server_state ~errors:(serialized_errors errors |> File.Handle.Table.of_alist_exn) ()
      in
      let configuration =
        Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ()
      in
      let files = List.map paths ~f:File.create in
      Request.process_display_type_errors_request ~state ~configuration ~files
      |> function
      | { Request.response = Some (Protocol.TypeCheckResponse response); _ } -> response
      | _ -> failwith "Unexpected response."
    in
    let expected_errors =
      let expected_error (path, undefined_globals) =
        let undefined_global global =
          Format.asprintf
            "Undefined name [18]: Global name `%s` is not defined, or there is at least one \
             control flow path that doesn't define `%s`."
            global
            global
        in
        path, List.map undefined_globals ~f:undefined_global
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
    ~paths:[mock_path "one.py"]
    ~errors:["one.py", ["one"]; "two.py", ["two"]]
    ~expected_errors:["one.py", ["one"]; "two.py", []];
  assert_response
    ~paths:[Path.create_relative ~root:(Path.create_absolute "/tmp") ~relative:"nonexistent.py"]
    ~errors:["one.py", ["one"]]
    ~expected_errors:["one.py", []]


let test_process_type_check_request context =
  let assert_response
      ?(sources = [])
      ~check
      ~expected_errors
      ?temporary_directory
      ?(incremental_transitive_dependencies = false)
      ()
    =
    let assert_response _ =
      let actual_errors =
        let configuration =
          Configuration.Analysis.create
            ~project_root:(Path.current_working_directory ())
            ~incremental_transitive_dependencies
            ()
        in
        let configuration, state = initialize ~configuration sources in
        let check = List.map check ~f:write_file in
        Request.process_type_check_request ~state ~configuration ~files:check
        |> function
        | { Request.response = Some (Protocol.TypeCheckResponse response); _ } -> response
        | _ -> failwith "Unexpected response."
      in
      assert_errors_equal ~actual_errors ~expected_errors
    in
    let temporary_directory =
      match temporary_directory with
      | Some temporary_directory -> temporary_directory
      | None -> OUnit2.bracket_tmpdir context
    in
    OUnit2.with_bracket_chdir context temporary_directory assert_response
  in
  assert_response ~check:[] ~expected_errors:[] ();
  assert_response
    ~check:["test.py", {|
        def foo() -> int:
          return 'asdf'
      |}]
    ~expected_errors:["test.py", ["Incompatible return type [7]: Expected `int` but got `str`."]]
    ();

  (* Absolute paths *)
  let temporary_directory = OUnit2.bracket_tmpdir context in
  let absolute_path =
    Path.create_absolute temporary_directory
    |> (fun root -> Path.append root ~element:"test.py")
    |> Path.absolute
  in
  assert_response
    ~check:[absolute_path, {|
        def foo() -> int:
          return 'asdf'
      |}]
    ~expected_errors:["test.py", ["Incompatible return type [7]: Expected `int` but got `str`."]]
    ~temporary_directory
    ();

  (* Check deferred requests for dependencies. *)
  assert_response
    ~sources:
      ["library.py", "def function() -> int: ..."; "client.py", "from library import function"]
    ~check:["library.py", "def function() -> int: ..."] (* Unchanged. *)
    ~expected_errors:["library.py", []]
    ();

  (* Single dependency. *)
  assert_response
    ~sources:
      ["library.py", "def function() -> int: ..."; "client.py", "from library import function"]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:["client.py", []; "library.py", []]
    ();

  (* Multiple depedencies. *)
  assert_response
    ~sources:
      [ "library.py", "def function() -> int: ...";
        "client.py", "from library import function";
        "other.py", "from library import function" ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:["client.py", []; "library.py", []; "other.py", []]
    ();

  (* Indirect dependency. *)
  assert_response
    ~incremental_transitive_dependencies:true
    ~sources:
      [ "library.py", "def function() -> int: ...";
        ( "client.py",
          {|
        from library import function
        def function() -> int: ...
      |} );
        "indirect.py", "from client import function" ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:["client.py", []; "indirect.py", []; "library.py", []]
    ();

  (* When multiple files match a qualifier, the existing file has priority. *)
  assert_response
    ~sources:["first.pyi", "def function() -> str: ..."]
    ~check:["first.py", "def function() -> int: ..."]
    ~expected_errors:[]
    ();

  (* Starred imports. *)
  assert_response
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["a.py", "var = 1337"]
    ~expected_errors:["a.py", []; "b.py", []]
    ();
  assert_response
    ~incremental_transitive_dependencies:true
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["a.py", "var = 1337"]
    ~expected_errors:["a.py", []; "b.py", []; "c.py", []]
    ();
  assert_response
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["b.py", "from a import *"]
    ~expected_errors:["b.py", []]
    ();
  assert_response
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["a.py", "var = ''"]
    ~expected_errors:["a.py", []; "b.py", []]
    ();
  assert_response
    ~sources:[]
    ~check:
      ["a.py", "def foo() -> int: return ''"; "a.pyi", "def foo() -> int: ..."]
      (* No errors due to getting shadowed by the stub. *)
      (* TODO(T44669208): We should not get any results for a.py here. *)
    ~expected_errors:["a.py", []; "a.pyi", []]
    ();

  (* Check nonexistent handles. *)
  let configuration, state = initialize [] in
  let files =
    Path.create_relative ~root:(Path.create_absolute "/tmp") ~relative:"nonexistent.py"
    |> File.create ~content:"def function() -> int: return ''"
    |> fun file -> [file]
  in
  let { Request.response; _ } = Request.process_type_check_request ~state ~configuration ~files in
  assert_equal (Some (Protocol.TypeCheckResponse [])) response;

  (* Ensure we don't raise an exception when a untracked files is passed in. *)
  Request.process_type_check_request
    ~state
    ~configuration
    ~files:
      [File.create (Path.create_absolute ~follow_symbolic_links:false "/nonexistent_root/a.py")]
  |> ignore


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
        { Protocol.DefinitionRequest.id = int_request_id 0; file; position }
      in
      let actual_response =
        let actual_response =
          Request.process_get_definition_request ~state ~configuration ~request
        in
        match actual_response with
        | { Request.response = Some (Protocol.LanguageServerProtocolResponse response); _ } ->
            Yojson.Safe.from_string response
        | _ -> failwith "Unexpected response."
      in
      let expected_response =
        let open LanguageServer.Types in
        let result =
          let response_location
              { Ast.Location.path;
                start = { Ast.Location.line = start_line; column = start_column };
                stop = { Ast.Location.line = stop_line; column = stop_column }
              }
            =
            { (* Temporary paths are OS-dependent. *)
              Location.uri =
                Path.uri
                  (Path.create_relative
                     ~root:(Path.create_absolute temporary_directory)
                     ~relative:path);
              range =
                { start = { Position.line = start_line; character = start_column };
                  end_ = { Position.line = stop_line; character = stop_column }
                }
            }
          in
          response >>| response_location |> Option.to_list
        in
        { TextDocumentDefinitionResponse.jsonrpc = "2.0";
          id = int_request_id 0;
          result = Some result;
          error = None
        }
        |> TextDocumentDefinitionResponse.to_yojson
      in
      let json_diff_printer format json =
        Yojson.Safe.pretty_to_string json |> Format.fprintf format "%s\n"
      in
      assert_equal
        ~printer:Yojson.Safe.pretty_to_string
        ~pp_diff:(diff ~print:json_diff_printer)
        expected_response
        actual_response
    in
    let sources =
      [ "library.py", "def function() -> int: ...";
        ( "client.py",
          {|
        from library import function
        def foo() -> int:
          return function()
        |}
        ) ]
    in
    (* Invalid request for an invalid file (no exception raised). *)
    assert_response ~sources ~line:0 ~column:0 None;

    (* Invalid request for a valid file. *)
    assert_response ~sources ~filename:"client.py" ~line:0 ~column:0 None;

    (* Valid request for a valid file. *)
    assert_response
      ~sources
      ~filename:"client.py"
      ~line:4
      ~column:9
      (Some
         { Location.path = "library.py";
           start = { Location.line = 0; column = 0 };
           stop = { Location.line = 0; column = 26 }
         })
  in
  OUnit2.with_bracket_chdir context temporary_directory run_test


let test_create_annotation_edit _ =
  let mock_missing_annotation : Analysis.Error.missing_annotation =
    { name = Reference.create "x";
      annotation = Some (Type.Literal (Integer 1));
      given_annotation = None;
      evidence_locations = [Location.Instantiated.any];
      thrown_at_source = true
    }
  in
  let mock_mismatch : Analysis.Error.mismatch =
    { actual = Type.integer;
      actual_expressions = [];
      expected = Type.string;
      due_to_invariance = false
    }
  in
  let location = { Location.Instantiated.any with start = { line = 0; column = 0 } } in
  let assert_edit ~source ~error ~expected_text ~expected_range =
    let file = write_file ("test.py", source) in
    let edit = Request.AnnotationEdit.create ~file ~error in
    assert_is_some edit;
    edit
    >>| Request.AnnotationEdit.new_text
    >>| assert_equal expected_text
    |> Option.value ~default:();
    edit
    >>| Request.AnnotationEdit.range
    >>| assert_equal expected_range
    |> Option.value ~default:()
  in
  assert_edit
    ~source:{|
        def foo():
          return 1
      |}
    ~expected_text:" -> int"
    ~expected_range:
      { LanguageServer.Types.Range.start = { line = 0; character = 9 };
        end_ = { line = 0; character = 9 }
      }
    ~error:
      (Some
         { Analysis.Error.location;
           kind = Analysis.Error.MissingReturnAnnotation mock_missing_annotation;
           signature = +mock_signature
         });
  assert_edit
    ~source:{|
      x = foo()
    |}
    ~expected_text:": int"
    ~expected_range:
      { LanguageServer.Types.Range.start = { line = 0; character = 1 };
        end_ = { line = 0; character = 1 }
      }
    ~error:
      (Some
         { Analysis.Error.location;
           kind = Analysis.Error.MissingGlobalAnnotation mock_missing_annotation;
           signature = +mock_signature
         });
  assert_edit
    ~source:{|
      def foo(x) -> int:
        return 1
    |}
    ~expected_text:": int"
    ~expected_range:
      { LanguageServer.Types.Range.start = { line = 0; character = 9 };
        end_ = { line = 0; character = 9 }
      }
    ~error:
      (Some
         { Analysis.Error.location;
           kind = Analysis.Error.MissingParameterAnnotation mock_missing_annotation;
           signature = +mock_signature
         });
  assert_edit
    ~source:{|
        Class A:
            x = foo()
    |}
    ~expected_text:": int"
    ~expected_range:
      { LanguageServer.Types.Range.start = { line = 1; character = 5 };
        end_ = { line = 1; character = 5 }
      }
    ~error:
      (Some
         { Analysis.Error.location;
           kind =
             Analysis.Error.MissingAttributeAnnotation
               { parent = Type.Any; missing_annotation = mock_missing_annotation };
           signature = +mock_signature
         });
  assert_edit
    ~source:{|
      def foo(x) -> str:
        return 1234
    |}
    ~expected_text:"-> int:"
    ~expected_range:
      { LanguageServer.Types.Range.start = { line = 0; character = 11 };
        end_ = { line = 0; character = 18 }
      }
    ~error:
      (Some
         { Analysis.Error.location;
           kind =
             Analysis.Error.IncompatibleReturnType
               { mismatch = mock_mismatch;
                 is_implicit = false;
                 is_unimplemented = false;
                 define_location = { Location.Reference.any with start = { line = 0; column = 0 } }
               };
           signature = +mock_signature
         });
  assert_edit
    ~source:{|
          x: str = 1234
      |}
    ~expected_text:": int "
    ~expected_range:
      { LanguageServer.Types.Range.start = { line = 0; character = 1 };
        end_ = { line = 0; character = 7 }
      }
    ~error:
      (Some
         { Analysis.Error.location;
           kind =
             Analysis.Error.IncompatibleVariableType
               { name = !&"x"; mismatch = mock_mismatch; declare_location = location };
           signature = +mock_signature
         })


let test_open_document_state _ =
  let create_file name =
    Path.create_relative ~root:(Path.create_absolute "/tmp") ~relative:name
    |> File.create ~content:""
  in
  let mock_set name =
    let set = Path.Set.empty in
    let file = create_file name in
    Path.Set.add set (File.path file)
  in
  let configuration, state = initialize [] in
  let assert_open_documents ~start ~request ~expected =
    let state = { state with open_documents = start } in
    let configuration = mock_server_configuration ~configuration in
    let ({ state = { open_documents; _ }; _ } : Request.response) =
      Request.process ~configuration ~state ~request
    in
    assert_true (Path.Set.equal open_documents expected)
  in
  assert_open_documents
    ~start:Path.Set.empty
    ~request:(Protocol.Request.OpenDocument (create_file "a.py"))
    ~expected:(mock_set "a.py");
  assert_open_documents
    ~start:(mock_set "a.py")
    ~request:(Protocol.Request.CloseDocument (create_file "a.py"))
    ~expected:Path.Set.empty


let test_resolution_shared_memory_added_for_open_documents _ =
  let test_file_a = write_file ("a.py", "def foo() -> int: return 3") in
  let test_file_b = write_file ("b.py", "def foo() -> int: return 3") in
  let files = [test_file_a; test_file_b] in
  let configuration, state = initialize [] in
  let configuration = { configuration with store_type_check_resolution = false } in
  let state = { state with open_documents = Path.Set.singleton (File.path test_file_a) } in
  let contains_resolution_shared_memory_reference key_string =
    key_string |> Reference.create |> Analysis.ResolutionSharedMemory.get |> Option.is_some
  in
  (* Before type checking request, shared memory does not have a.foo and b.foo *)
  assert_false (contains_resolution_shared_memory_reference "a.foo");
  assert_false (contains_resolution_shared_memory_reference "b.foo");
  let _ = Request.process_type_check_request ~state ~configuration ~files in
  (* Before type checking request, shared memory only has a.foo because a.py is open. *)
  assert_true (contains_resolution_shared_memory_reference "a.foo");
  assert_false (contains_resolution_shared_memory_reference "b.foo")


let () =
  "request"
  >::: [ "generate_lsp_response" >:: test_generate_lsp_response;
         "process_client_shutdown_request" >:: test_process_client_shutdown_request;
         "process_type_query_request" >:: test_process_type_query_request;
         "process_display_type_errors_request" >:: test_process_display_type_errors_request;
         "process_type_check_request" >:: test_process_type_check_request;
         "process_get_definition_request" >:: test_process_get_definition_request;
         "open_document_state" >:: test_open_document_state;
         "create_annotation_edit" >:: test_create_annotation_edit;
         "test_resolution_shared_memory_added_for_open_documents"
         >:: test_resolution_shared_memory_added_for_open_documents ]
  |> Test.run
