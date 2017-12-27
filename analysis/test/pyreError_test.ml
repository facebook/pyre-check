(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Expression
open Test


module Error = PyreError


let mock_define =
  +{
    Statement.Define.name = Access.create "foo";
    parameters = [];
    body = [];
    decorators = [];
    docstring = None;
    return_annotation = None;
    async = false;
    generated = false;
    parent = None;
  }


let mock_parent =
  Annotated.Class.create {
    Statement.Class.name = Access.create "foo";
    bases = [];
    body = [];
    decorators = [];
    docstring = None;
  }


let error kind =
  { Error.location = Location.any; kind; define = mock_define }


let test_due_to_analysis_limitations _ =

  (* Immutable Type. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleType {
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              parent = Some mock_parent;
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.Top;
              };
              declare_location = Location.any;
            })));
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleType {
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              parent = Some mock_parent;
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.string;
              };
              declare_location = Location.any;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleType {
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              parent = Some mock_parent;
              mismatch = {
                Error.actual = Type.string;
                expected = Type.Top;
              };
              declare_location = Location.any;
            })));

  (* Initialization *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.UninitializedField{
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              parent = mock_parent;
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.Optional Type.Top;
              };
            })));

  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.UninitializedField {
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              parent = mock_parent;
              mismatch = {
                Error.actual = Type.string;
                expected = Type.Optional Type.string;
              };
            })));

  (* MissingParameterAnnotation. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingParameterAnnotation {
              Error.name = ~~"";
              annotation = Type.Top;
              due_to_any = false;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingParameterAnnotation {
              Error.name = ~~"";
              annotation = Type.string;
              due_to_any = false;
            })));

  (* MissingReturnAnnotation. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingReturnAnnotation {
              Error.annotation = Type.Top;
              evidence_locations = [];
              due_to_any = false;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingReturnAnnotation {
              Error.annotation = Type.string;
              evidence_locations = [];
              due_to_any = false;
            })));

  (* MissingAnnotation *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingAnnotation {
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              annotation = Type.Top;
              parent = Some mock_parent;
              due_to_any = false;
              evidence_locations = [];
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingAnnotation {
              Error.name = [Expression.Record.Access.Identifier (~~"")];
              annotation = Type.string;
              parent = Some mock_parent;
              due_to_any = false;
              evidence_locations = [];
            })));

  (* Parameter. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleParameterType {
              Error.name = ~~"";
              position = 1;
              callee = mock_define.Node.value;
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.Top;
              };
            })));
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleParameterType {
              Error.name = ~~"";
              position = 1;
              callee = mock_define.Node.value;
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.string;
              };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleParameterType {
              Error.name = ~~"";
              position = 1;
              callee = mock_define.Node.value;
              mismatch = {
                Error.actual = Type.string;
                expected = Type.Top;
              };
            })));

  (* Return. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleReturnType {
              Error.actual = Type.Top;
              expected = Type.Top;
            })));
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleReturnType {
              Error.actual = Type.Top;
              expected = Type.string;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleReturnType {
              Error.actual = Type.string;
              expected = Type.Top;
            })));

  (* UndefinedMethod. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.UndefinedMethod {
              Error.annotation = Type.Top;
              call =
                Annotated.Call.create
                  ~kind:Annotated.Call.Method
                  { Expression.Call.name = !""; arguments = [] };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.UndefinedMethod {
              Error.annotation = Type.string;
              call =
                Annotated.Call.create
                  ~kind:Annotated.Call.Method
                  { Expression.Call.name = !""; arguments = [] };
            })));

  (* UndefinedType. *)
  assert_true
    (Error.due_to_analysis_limitations (error (Error.UndefinedType Type.Top)));
  assert_false
    (Error.due_to_analysis_limitations (error (Error.UndefinedType Type.string)))


let test_join _ =
  let assert_join left right expected =
    let environment = Environment.reader (Environment.Builder.create ()) in
    let resolution = Environment.resolution environment () in
    let result = Error.join ~resolution left right in
    assert_equal ~cmp:Error.equal result expected
  in
  assert_join
    (error (Error.IncompatibleType {
         Error.name = [Expression.Record.Access.Identifier (~~"")];
         parent = Some mock_parent;
         mismatch = {
           Error.actual = Type.Top;
           expected = Type.Top;
         };
         declare_location = Location.any;
       }))
    (error
       (Error.UndefinedMethod {
           Error.annotation = Type.string;
           call =
             Annotated.Call.create
               ~kind:Annotated.Call.Method
               { Expression.Call.name = !""; arguments = [] };
         }))
    (error Error.Top);
  assert_join
    (error (Error.IncompatibleType {
         Error.name = [Expression.Record.Access.Identifier (~~"")];
         parent = Some mock_parent;
         mismatch = {
           Error.actual = Type.Top;
           expected = Type.Top;
         };
         declare_location = Location.any;
       }))
    (error (Error.IncompatibleType {
         Error.name = [Expression.Record.Access.Identifier (~~"")];
         parent = None;
         mismatch = {
           Error.actual = Type.Top;
           expected = Type.Top;
         };
         declare_location = Location.any;
       }))
    (error Error.Top);
  assert_join
    (error
       (Error.IncompatibleParameterType {
           Error.name = ~~"";
           position = 1;
           callee = mock_define.Node.value;
           mismatch = {
             Error.actual = Type.integer;
             expected = Type.string;
           };
         }))
    (error
       (Error.IncompatibleParameterType {
           Error.name = ~~"";
           position = 1;
           callee = mock_define.Node.value;
           mismatch = {
             Error.actual = Type.float;
             expected = Type.string;
           };
         }))
    (error
       (Error.IncompatibleParameterType {
           Error.name = ~~"";
           position = 1;
           callee = mock_define.Node.value;
           mismatch = {
             Error.actual = Type.float;
             expected = Type.string;
           };
         }))


let () =
  "pyreError">:::[
    "due_to_analysis_limitations">::test_due_to_analysis_limitations;
    "join">::test_join;
  ]
  |> run_test_tt_main
