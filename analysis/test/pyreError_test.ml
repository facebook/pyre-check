(** Copyright 2016-present Facebook. All rights reserved. **)

open OUnit2

open Ast
open Test


module Error = PyreError


let mock_define =
  +{
    Statement.Define.name = Instantiated.Access.create "foo";
    parameters = [];
    body = [];
    decorators = [];
    docstring = None;
    return_annotation = None;
    async = false;
    parent = None;
  }


let mock_parent =
  Annotated.Class.create {
    Statement.Class.name = Instantiated.Access.create "foo";
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
              Error.name = [Expression.Access.Identifier (~~"")];
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
              Error.name = [Expression.Access.Identifier (~~"")];
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
              Error.name = [Expression.Access.Identifier (~~"")];
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
              Error.name = [Expression.Access.Identifier (~~"")];
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
              Error.name = [Expression.Access.Identifier (~~"")];
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
              Error.type_annotation = Type.Top;
              return_locations = Core.Int.Set.empty;
              due_to_any = false;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingReturnAnnotation {
              Error.type_annotation = Type.string;
              return_locations = Core.Int.Set.empty;
              due_to_any = false;
            })));

  (* MissingAnnotation *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingAnnotation {
              Error.name = [Expression.Access.Identifier (~~"")];
              annotation = Type.Top;
              parent = Some mock_parent;
              due_to_any = false;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingAnnotation {
              Error.name = [Expression.Access.Identifier (~~"")];
              annotation = Type.string;
              parent = Some mock_parent;
              due_to_any = false;
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
              call = Annotated.Call.create { Expression.Call.name = !""; arguments = [] };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.UndefinedMethod {
              Error.annotation = Type.string;
              call = Annotated.Call.create { Expression.Call.name = !""; arguments = [] };
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
         Error.name = [Expression.Access.Identifier (~~"")];
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
           call = Annotated.Call.create { Expression.Call.name = !""; arguments = [] };
         }))
    (error Error.Top);
  assert_join
    (error (Error.IncompatibleType {
         Error.name = [Expression.Access.Identifier (~~"")];
         parent = Some mock_parent;
         mismatch = {
           Error.actual = Type.Top;
           expected = Type.Top;
         };
         declare_location = Location.any;
       }))
    (error (Error.IncompatibleType {
         Error.name = [Expression.Access.Identifier (~~"")];
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
