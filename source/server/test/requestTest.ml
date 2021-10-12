(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Pyre
open Server
open Test
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


let test_create_annotation_edit context =
  let root = bracket_tmpdir context |> Path.create_absolute ~follow_symbolic_links:true in
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


let () =
  "request"
  >::: [
         "generate_lsp_response" >:: test_generate_lsp_response;
         "create_annotation_edit" >:: test_create_annotation_edit;
       ]
  |> Test.run
