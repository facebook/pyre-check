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
open CommandTest
module Error = Analysis.AnalysisError

let int_request_id id = LanguageServer.Types.RequestId.Int id

let string_request_id id = LanguageServer.Types.RequestId.String id

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
    let expected_response = expected_response |> Yojson.Safe.from_string |> Yojson.Safe.to_string in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  assert_response (int_request_id 1) (Some 1337) {|{"jsonrpc":"2.0","id":1,"result":1337}|};
  assert_response (string_request_id "abcd") None {|{"jsonrpc":"2.0","id":"abcd","result":null}|}


let test_process_client_shutdown_request context =
  let assert_response id expected_response =
    let { ScratchServer.state; _ } = ScratchServer.start ~context [] in
    let actual_response =
      Request.process_client_shutdown_request ~state ~id
      |> function
      | { Request.response = Some (Protocol.LanguageServerProtocolResponse response); _ } ->
          response
      | _ -> failwith "Unexpected response."
    in
    let expected_response = expected_response |> Yojson.Safe.from_string |> Yojson.Safe.to_string in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  assert_response (int_request_id 0) {|{"jsonrpc":"2.0","id":0,"result":null}|};
  assert_response (string_request_id "xyz") {|{"jsonrpc":"2.0","id":"xyz","result":null}|}


let test_process_type_query_request context =
  let { ScratchServer.configuration; state; _ } =
    ScratchServer.start
      ~context
      ~show_error_traces:true
      [
        "test.py", {|
        def foo(a: int) -> int:
          return a
      |};
        ( "await.py",
          {|
        async def await_me() -> int: ...
        async def bar():
          await_me()
       |}
        );
        ( "classy.py",
          {|
         from typing import Generic, TypeVar
         T = TypeVar("T")
         class C(Generic[T]):
           def foo(self, x: T) -> None: ...
         def not_in_c() -> int: ...
        |}
        );
        ( "define_test.py",
          {|
            def with_var( *args): ...
            def with_kwargs( **kwargs): ...
          |}
        );
      ]
  in
  let assert_response request expected_response =
    let actual_response =
      Request.process_type_query_request ~state ~configuration ~request
      |> function
      | { Request.response = Some (Protocol.TypeQueryResponse response); _ } ->
          Protocol.TypeQuery.response_to_yojson response |> Yojson.Safe.to_string
      | _ -> failwith "Unexpected response."
    in
    let expected_response = expected_response |> Yojson.Safe.from_string |> Yojson.Safe.to_string in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let path relative = Path.create_relative ~root:local_root ~relative in
  assert_response
    (Protocol.TypeQuery.Join (parse_single_expression "int", parse_single_expression "float"))
    {|{"response":{"type":"float"}}|};
  assert_response
    (Protocol.TypeQuery.NormalizeType (parse_single_expression "yerp"))
    {|{"error":"Type `yerp` was not found in the type order."}|};
  assert_response
    (Protocol.TypeQuery.RunCheck { check_name = "awaitable"; paths = [path "test.py"] })
    {|
    {
        "response": {"errors": []}
    }
    |};

  assert_response
    (Protocol.TypeQuery.RunCheck { check_name = "awaitable"; paths = [path "await.py"] })
    {|
    {
        "response": {
            "errors": [
                {
                    "line": 4,
                    "column": 2,
                    "stop_line": 4,
                    "stop_column": 12,
                    "path": "await.py",
                    "code": 1001,
                    "name": "Unawaited awaitable",
                    "description": "Unawaited awaitable [1001]: `await.await_me()` is never awaited. `await.await_me()` is defined on line 4",
                    "long_description": "Unawaited awaitable [1001]: `await.await_me()` is never awaited.\n`await.await_me()` is defined on line 4",
                    "concise_description": "Unawaited awaitable [1001]: `await.await_me()` is never awaited.\n`await.await_me()` is defined on line 4",
                    "inference": {},
                    "define": "await.bar"
                }
            ]
        }
    }
    |};
  assert_response
    (Protocol.TypeQuery.CalleesWithLocation (Reference.create "await.bar"))
    {|
    {
        "response": {
            "callees": [
                {
                    "locations": [
                        {
                            "path":"await.py",
                            "start": {
                                "line": 4,
                                "column": 2
                            },
                            "stop": {
                                "line": 4,
                                "column": 10
                            }
                        }
                    ],
                    "kind": "function",
                    "target": "await.await_me"
                }
            ]
        }
    }
    |};
  assert_response
    Protocol.TypeQuery.DumpCallGraph
    {|
    {
        "response": {
            "typing.Iterable.__iter__": [],
            "contextlib.ContextManager.__enter__": [],
            "await.bar": [
                {
                    "locations": [
                        {
                            "path": "await.py",
                            "start": {
                                "line": 4,
                                "column": 2
                            },
                            "stop": {
                                "line": 4,
                                "column": 10
                            }
                        }
                    ],
                    "kind": "function",
                    "target": "await.await_me"
                }
            ],
            "dict.items": [],
            "dict.add_both": [],
            "dict.add_value": [],
            "dict.add_key": [],
            "str.substr": [],
            "str.lower": [],
            "str.format": [],
            "test.foo": []
        }
    }
    |};
  assert_response
    (Protocol.TypeQuery.Defines [Reference.create "test"])
    {|
    {
      "response": [
        {
          "name": "test.foo",
          "parameters": [
            {
              "name": "a",
              "annotation": "int"
            }
          ],
          "return_annotation": "int"
        }
      ]
    }
    |};
  assert_response
    (Protocol.TypeQuery.Defines [Reference.create "classy"])
    {|
    {
      "response": [
        {
          "name": "classy.not_in_c",
          "parameters": [],
          "return_annotation":"int"
        },
        {
          "name": "classy.C.foo",
          "parameters": [
            {
              "name": "self",
              "annotation": null
            },
            {
              "name": "x",
              "annotation": "T"
            }
          ],
          "return_annotation": "None"
        }
      ]
    }
    |};
  assert_response
    (Protocol.TypeQuery.Defines [Reference.create "classy.C"])
    {|
    {
      "response": [
        {
          "name": "classy.C.foo",
          "parameters": [
            {
              "name": "self",
              "annotation": null
            },
            {
              "name": "x",
              "annotation": "T"
            }
          ],
          "return_annotation": "None"
        }
      ]
    }
    |};

  assert_response
    (Protocol.TypeQuery.Defines [Reference.create "define_test"])
    {|
    {
      "response": [
        {
          "name": "define_test.with_kwargs",
          "parameters": [
            {
              "name": "**kwargs",
              "annotation": null
            }
          ],
          "return_annotation": null
        },
        {
          "name": "define_test.with_var",
          "parameters": [
            {
              "name": "*args",
              "annotation": null
            }
          ],
          "return_annotation": null
        }
      ]
    }
    |};
  assert_response
    (Protocol.TypeQuery.Defines [Reference.create "nonexistent"])
    {|
    {
      "response": []
    }
    |};
  assert_response
    (Protocol.TypeQuery.Defines [Reference.create "test"; Reference.create "classy"])
    {|
    {
      "response": [
        {
          "name": "test.foo",
          "parameters": [
            {
              "name": "a",
              "annotation": "int"
            }
          ],
          "return_annotation": "int"
        },
        {
          "name": "classy.not_in_c",
          "parameters": [],
          "return_annotation":"int"
        },
        {
          "name": "classy.C.foo",
          "parameters": [
            {
              "name": "self",
              "annotation": null
            },
            {
              "name": "x",
              "annotation": "T"
            }
          ],
          "return_annotation": "None"
        }
      ]
    }
    |};
  assert_response
    (Protocol.TypeQuery.Methods (parse_single_expression "classy.C"))
    {|
    {
      "response": {
        "methods": [
          {
            "name": "foo",
            "parameters": [
              "classy.C[typing.Any]",
              "typing.Any"
            ],
            "return_annotation": "None"
          }
        ]
      }
    }
    |}


let assert_errors_equal ~actual_errors ~expected_errors =
  let actual_errors = List.map actual_errors ~f:Error.Instantiated.description in
  let equal left right =
    List.equal
      String.equal
      (List.sort ~compare:String.compare left)
      (List.sort ~compare:String.compare right)
  in
  let printer errors = Format.asprintf "%s" (String.concat errors ~sep:", ") in
  assert_equal ~cmp:equal ~printer expected_errors actual_errors


let test_process_display_type_errors_request context =
  let assert_response ~paths ~errors ~expected_errors =
    let actual_errors =
      let { ScratchServer.configuration; state; _ } =
        let sources =
          List.map errors ~f:(fun (handle, global) -> handle, Format.sprintf "test_%s()" global)
        in
        ScratchServer.start ~context sources
      in
      let { Configuration.Analysis.local_root; _ } = configuration in
      let paths =
        List.map paths ~f:(fun relative -> Path.create_relative ~root:local_root ~relative)
      in
      Request.process_display_type_errors_request ~state ~configuration paths
      |> function
      | { Request.response = Some (Protocol.TypeCheckResponse response); _ } -> response
      | _ -> failwith "Unexpected response."
    in
    let expected_errors =
      let undefined_global global =
        Format.asprintf
          "Unbound name [10]: Name `test_%s` is used but not defined in the current scope."
          global
      in
      List.map expected_errors ~f:undefined_global
    in
    assert_errors_equal ~actual_errors ~expected_errors
  in
  assert_response ~paths:[] ~errors:[] ~expected_errors:[];

  (* Empty request returns all errors. *)
  assert_response
    ~paths:[]
    ~errors:["one.py", "one"; "two.py", "two"]
    ~expected_errors:["one"; "two"];
  assert_response
    ~paths:["one.py"]
    ~errors:["one.py", "one"; "two.py", "two"]
    ~expected_errors:["one"];
  assert_response ~paths:["nonexistent.py"] ~errors:["one.py", "one"] ~expected_errors:[]


let test_process_type_check_request context =
  let assert_response
      ?(sources = [])
      ~check
      ~expected_errors
      ?(incremental_style = Configuration.Analysis.Shallow)
      ()
    =
    let actual_errors =
      (* Start with empty files *)
      let { ScratchServer.configuration; state; _ } =
        let check = List.map check ~f:(fun (relative, _) -> relative, "") in
        ScratchServer.start ~incremental_style ~context ~external_sources:sources check
      in
      let paths =
        let { Configuration.Analysis.local_root; _ } = configuration in
        (* Overwrite the files with real contents *)
        List.map check ~f:(fun (relative, content) ->
            let path = Path.create_relative ~root:local_root ~relative in
            let file = File.create path ~content:(trim_extra_indentation content) in
            File.write file;
            path)
      in
      Request.process_type_check_request ~state ~configuration paths
      |> function
      | { Request.response = Some (Protocol.TypeCheckResponse response); _ } -> response
      | _ -> failwith "Unexpected response."
    in
    assert_errors_equal ~actual_errors ~expected_errors
  in
  assert_response ~check:[] ~expected_errors:[] ();
  assert_response
    ~check:["test.py", {|
        def foo() -> int:
          return 'asdf'
      |}]
    ~expected_errors:["Incompatible return type [7]: Expected `int` but got `str`."]
    ();

  (* Check deferred requests for dependencies. *)
  assert_response
    ~sources:
      ["library.py", "def function() -> int: ..."; "client.py", "from library import function"]
    ~check:["library.py", "def function() -> int: ..."] (* Unchanged. *)
    ~expected_errors:[]
    ();

  (* Single dependency. *)
  assert_response
    ~sources:
      ["library.py", "def function() -> int: ..."; "client.py", "from library import function"]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:[]
    ();

  (* Multiple depedencies. *)
  assert_response
    ~sources:
      [
        "library.py", "def function() -> int: ...";
        "client.py", "from library import function";
        "other.py", "from library import function";
      ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:[]
    ();

  (* Indirect dependency. *)
  assert_response
    ~incremental_style:FineGrained
    ~sources:
      [
        "library.py", "def function() -> int: ...";
        ( "client.py",
          {|
        from library import function
        def function() -> int: ...
      |} );
        "indirect.py", "from client import function";
      ]
    ~check:["library.py", "def function() -> str: ..."]
    ~expected_errors:[]
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
    ~expected_errors:[]
    ();
  assert_response
    ~incremental_style:FineGrained
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["a.py", "var = 1337"]
    ~expected_errors:[]
    ();
  assert_response
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["b.py", "from a import *"]
    ~expected_errors:[]
    ();
  assert_response
    ~sources:["a.py", "var = 42"; "b.py", "from a import *"; "c.py", "from b import *"]
    ~check:["a.py", "var = ''"]
    ~expected_errors:[]
    ();
  assert_response
    ~sources:[]
    ~check:
      ["a.py", "def foo() -> int: return ''"; "a.pyi", "def foo() -> int: ..."]
      (* No errors due to getting shadowed by the stub. *)
      (* TODO(T44669208): We should not get any results for a.py here. *)
    ~expected_errors:[]
    ();

  (* Check nonexistent handles. *)
  let { ScratchServer.configuration; state; _ } = ScratchServer.start ~context [] in
  let paths =
    let root = bracket_tmpdir context |> Path.create_absolute in
    [Path.create_relative ~root ~relative:"nonexistent.py"]
  in
  let { Request.response; _ } = Request.process_type_check_request ~state ~configuration paths in
  assert_equal (Some (Protocol.TypeCheckResponse [])) response;

  (* Ensure we don't raise an exception when a untracked files is passed in. *)
  Request.process_type_check_request
    ~state
    ~configuration
    [Path.create_absolute ~follow_symbolic_links:false "/nonexistent_root/a.py"]
  |> ignore


let test_process_get_definition_request context =
  let {
    ScratchServer.configuration = { Configuration.Analysis.local_root; _ } as configuration;
    state;
    _;
  }
    =
    ScratchServer.start
      ~context
      [
        "library.py", "def function() -> int: ...";
        ( "client.py",
          {|
        from library import function
        def foo() -> int:
          return function()
        |}
        );
      ]
  in
  let assert_response ?filename ~line ~column response =
    let position = { Location.line; column } in
    let path =
      match filename with
      | Some valid_filename -> Path.create_relative ~root:local_root ~relative:valid_filename
      | _ ->
          (* Create a bogus filename entry. *)
          Path.create_relative
            ~relative:"bogusfile.py"
            ~root:(Path.create_absolute ~follow_symbolic_links:false "/bogus/dir")
    in
    let request = { Protocol.DefinitionRequest.id = int_request_id 0; path; position } in
    let actual_response =
      let actual_response = Request.process_get_definition_request ~state ~configuration ~request in
      match actual_response with
      | { Request.response = Some (Protocol.LanguageServerProtocolResponse response); _ } ->
          Yojson.Safe.from_string response
      | _ -> failwith "Unexpected response."
    in
    let expected_response =
      let open LanguageServer.Types in
      let result =
        let response_location
            {
              Ast.Location.start = { Ast.Location.line = start_line; column = start_column };
              stop = { Ast.Location.line = stop_line; column = stop_column };
            }
          =
          {
            (* Temporary paths are OS-dependent. *)
            Location.uri = Path.uri path;
            range =
              {
                start = { Position.line = start_line; character = start_column };
                end_ = { Position.line = stop_line; character = stop_column };
              };
          }
        in
        match response with
        | Some _ -> response >>| response_location |> Option.to_list |> Option.some
        | None -> None
      in
      {
        TextDocumentDefinitionResponse.jsonrpc = "2.0";
        id = int_request_id 0;
        result;
        error = None;
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
  (* Invalid request for an invalid file (no exception raised). *)
  assert_response ~line:0 ~column:0 None;

  (* Invalid request for a valid file. *)
  assert_response ~filename:"client.py" ~line:0 ~column:0 None;

  (* Valid request for a valid file. *)
  assert_response
    ~filename:"client.py"
    ~line:4
    ~column:9
    (Some
       {
         Location.start = { Location.line = 0; column = 0 };
         stop = { Location.line = 0; column = 26 };
       })


let test_create_annotation_edit context =
  let root = bracket_tmpdir context |> Path.create_absolute in
  let mock_missing_annotation : Error.missing_annotation =
    {
      name = Reference.create "x";
      annotation = Some (Type.Literal (Integer 1));
      given_annotation = None;
      evidence_locations = [Location.WithPath.any];
      thrown_at_source = true;
    }
  in
  let mock_mismatch : Error.mismatch =
    { actual = Type.integer; expected = Type.string; due_to_invariance = false }
  in
  let location = { Location.WithModule.any with start = { line = 0; column = 0 } } in
  let instantiated_location = { Location.WithPath.any with start = { line = 0; column = 0 } } in
  let assert_edit ~source ~error ~expected_text ~expected_range =
    let file =
      let path = Path.create_relative ~root ~relative:"test.py" in
      File.create ~content:(trim_extra_indentation source) path
    in
    File.write file;
    let edit = Request.AnnotationEdit.create ~file ~error in
    assert_is_some edit;
    let edit = Option.value_exn edit in
    assert_equal
      ~cmp:String.equal
      ~printer:Fn.id
      expected_text
      (Request.AnnotationEdit.new_text edit);
    assert_equal
      ~cmp:LanguageServer.Types.Range.equal
      ~printer:LanguageServer.Types.Range.show
      expected_range
      (Request.AnnotationEdit.range edit)
  in
  assert_edit
    ~source:{|
        def foo():
          return 1
      |}
    ~expected_text:" -> int"
    ~expected_range:
      {
        LanguageServer.Types.Range.start = { line = 0; character = 9 };
        end_ = { line = 0; character = 9 };
      }
    ~error:
      (Some
         {
           Error.location;
           kind = Error.MissingReturnAnnotation mock_missing_annotation;
           signature = +mock_signature;
         });
  assert_edit
    ~source:{|
      x = foo()
    |}
    ~expected_text:": int"
    ~expected_range:
      {
        LanguageServer.Types.Range.start = { line = 0; character = 1 };
        end_ = { line = 0; character = 2 };
      }
    ~error:
      (Some
         {
           Error.location;
           kind = Error.MissingGlobalAnnotation mock_missing_annotation;
           signature = +mock_signature;
         });
  assert_edit
    ~source:{|
      def foo(x) -> int:
        return 1
    |}
    ~expected_text:": int"
    ~expected_range:
      {
        LanguageServer.Types.Range.start = { line = 0; character = 9 };
        end_ = { line = 0; character = 9 };
      }
    ~error:
      (Some
         {
           Error.location;
           kind = Error.MissingParameterAnnotation mock_missing_annotation;
           signature = +mock_signature;
         });
  assert_edit
    ~source:{|
        Class A:
            x = foo()
    |}
    ~expected_text:": int"
    ~expected_range:
      {
        LanguageServer.Types.Range.start = { line = 1; character = 5 };
        end_ = { line = 1; character = 6 };
      }
    ~error:
      (Some
         {
           Error.location;
           kind =
             Error.MissingAttributeAnnotation
               { parent = Type.Any; missing_annotation = mock_missing_annotation };
           signature = +mock_signature;
         });
  assert_edit
    ~source:{|
      def foo(x) -> str:
        return 1234
    |}
    ~expected_text:"-> int:"
    ~expected_range:
      {
        LanguageServer.Types.Range.start = { line = 0; character = 11 };
        end_ = { line = 0; character = 18 };
      }
    ~error:
      (Some
         {
           Error.location;
           kind =
             Error.IncompatibleReturnType
               {
                 mismatch = mock_mismatch;
                 is_implicit = false;
                 is_unimplemented = false;
                 define_location = { Location.any with start = { line = 0; column = 0 } };
               };
           signature = +mock_signature;
         });
  assert_edit
    ~source:{|
          x: str = 1234
      |}
    ~expected_text:": int "
    ~expected_range:
      {
        LanguageServer.Types.Range.start = { line = 0; character = 1 };
        end_ = { line = 0; character = 7 };
      }
    ~error:
      (Some
         {
           Error.location;
           kind =
             Error.IncompatibleVariableType
               {
                 incompatible_type = { name = !&"x"; mismatch = mock_mismatch };
                 declare_location = instantiated_location;
               };
           signature = +mock_signature;
         })


let test_open_document_state context =
  let {
    ScratchServer.configuration = { Configuration.Analysis.local_root; _ };
    server_configuration;
    state;
    _;
  }
    =
    ScratchServer.start ~context ["a.py", ""; "b.py", ""]
  in
  let create_path name = Path.create_relative ~root:local_root ~relative:name in
  let assert_open_documents ~start ~request ~expected =
    let state = { state with open_documents = start } in
    let ({ state = { open_documents; _ }; _ } : Request.response) =
      Request.process ~configuration:server_configuration ~state ~request
    in
    assert_true (Reference.Table.equal String.equal open_documents expected)
  in
  assert_open_documents
    ~start:(Reference.Table.create ())
    ~request:(Protocol.Request.OpenDocument (create_path "a.py"))
    ~expected:(Reference.Table.of_alist_exn [!&"a", ""]);
  assert_open_documents
    ~start:(Reference.Table.of_alist_exn [!&"a", ""])
    ~request:(Protocol.Request.CloseDocument (create_path "a.py"))
    ~expected:(Reference.Table.create ())


let () =
  "request"
  >::: [
         "generate_lsp_response" >:: test_generate_lsp_response;
         "process_client_shutdown_request" >:: test_process_client_shutdown_request;
         "process_type_query_request" >:: test_process_type_query_request;
         "process_display_type_errors_request" >:: test_process_display_type_errors_request;
         "process_type_check_request" >:: test_process_type_check_request;
         "process_get_definition_request" >:: test_process_get_definition_request;
         "open_document_state" >:: test_open_document_state;
         "create_annotation_edit" >:: test_create_annotation_edit;
       ]
  |> Test.run
