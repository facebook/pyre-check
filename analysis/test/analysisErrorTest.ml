(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Test


let define ?(body = []) () =
  +{
    Statement.Define.name = Access.create "foo";
    parameters = [];
    body;
    decorators = [];
    docstring = None;
    return_annotation = Some !"int";
    async = false;
    generated = false;
    parent = None;
  }


let mock_define =
  define ()


let mock_parent =
  {
    Statement.Class.name = Access.create "foo";
    bases = [];
    body = [];
    decorators = [];
    docstring = None;
  }
  |> Node.create_with_default_location
  |> Annotated.Class.create


let create_mock_location path =
  let start = { Location.line = 1; column = 1 } in
  let stop = { Location.line = 1; column = 1 } in
  { Location.path; start; stop; }


let error ?(define = mock_define) kind =
  { Error.location = Location.any; kind; define }


let missing_return annotation =
  Error.MissingReturnAnnotation {
    Error.annotation;
    evidence_locations = [];
    due_to_any = false;
  }


let incompatible_return_type actual expected =
  Error.IncompatibleReturnType {
    Error.actual;
    expected;
  }


let undefined_attribute actual =
  Error.UndefinedAttribute {
    Error.annotation = actual;
    attribute = Access.create "foo";
    class_attribute = false;
  }


let configuration = Configuration.create ()


let test_due_to_analysis_limitations _ =
  (* IncompatibleAttributeType. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleAttributeType {
              Error.parent = mock_parent;
              incompatible_type = {
                Error.name = [Expression.Access.Identifier (~~"")];
                mismatch = {
                  Error.actual = Type.Top;
                  expected = Type.Top;
                };
                declare_location = Location.any;
              };
            })));
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleAttributeType {
              Error.parent = mock_parent;
              incompatible_type = {
                Error.name = [Expression.Access.Identifier (~~"")];
                mismatch = {
                  Error.actual = Type.Top;
                  expected = Type.string;
                };
                declare_location = Location.any;
              };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleAttributeType {
              Error.parent = mock_parent;
              incompatible_type = {
                Error.name = [Expression.Access.Identifier (~~"")];
                mismatch = {
                  Error.actual = Type.string;
                  expected = Type.Top;
                };
                declare_location = Location.any;
              };
            })));

  (* Initialization *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.UninitializedAttribute{
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
          (Error.UninitializedAttribute {
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

  (* MissingAttributeAnnotation *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingAttributeAnnotation {
              Error.parent = mock_parent;
              Error.missing_annotation = {
                Error.name = [Expression.Access.Identifier (~~"")];
                annotation = Type.Top;
                due_to_any = false;
                evidence_locations = [];
              };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingAttributeAnnotation {
              Error.parent = mock_parent;
              Error.missing_annotation = {
                Error.name = [Expression.Access.Identifier (~~"")];
                annotation = Type.string;
                due_to_any = false;
                evidence_locations = [];
              };
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

  (* UndefinedFunction. *)
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.UndefinedFunction {
              Error.annotation = Some Type.Top;
              call =
                Annotated.Call.create
                  ~kind:Annotated.Call.Method
                  { Call.name = !""; arguments = [] };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.UndefinedFunction {
              Error.annotation = Some Type.string;
              call =
                Annotated.Call.create
                  ~kind:Annotated.Call.Method
                  { Call.name = !""; arguments = [] };
            })));

  (* UndefinedType. *)
  assert_true
    (Error.due_to_analysis_limitations (error (Error.UndefinedType Type.Top)));
  assert_false
    (Error.due_to_analysis_limitations (error (Error.UndefinedType Type.string)))


let test_join _ =
  let assert_join left right expected =
    let environment =
      Environment.handler ~configuration (Environment.Builder.create ~configuration ())
    in
    let resolution = Environment.resolution environment () in
    let result = Error.join ~resolution left right in
    assert_equal ~cmp:Error.equal result expected
  in
  assert_join
    (error
       (Error.IncompatibleAttributeType {
           Error.parent = mock_parent;
           incompatible_type = {
             Error.name = [Expression.Access.Identifier (~~"")];
             mismatch = {
               Error.actual = Type.Top;
               expected = Type.Top;
             };
             declare_location = Location.any;
           };
         }))
    (error
       (Error.UndefinedFunction {
           Error.annotation = Some Type.string;
           call =
             Annotated.Call.create
               ~kind:Annotated.Call.Method
               { Call.name = !""; arguments = [] };
         }))
    (error Error.Top);
  assert_join
    (error
       (Error.IncompatibleAttributeType {
           Error.parent = mock_parent;
           incompatible_type = {
             Error.name = [Expression.Access.Identifier (~~"")];
             mismatch = {
               Error.actual = Type.Top;
               expected = Type.Top;
             };
             declare_location = Location.any;
           };
         }))
    (error (Error.IncompatibleVariableType {
         Error.name = [Expression.Access.Identifier (~~"")];
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
         }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Expression.Access.Identifier (~~"")];
           annotation = Type.integer;
           evidence_locations = [create_mock_location "derp.py"];
           due_to_any = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Expression.Access.Identifier (~~"")];
           annotation = Type.float;
           evidence_locations = [create_mock_location "durp.py"];
           due_to_any = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Expression.Access.Identifier (~~"")];
           annotation = Type.float;
           evidence_locations = [create_mock_location "derp.py"; create_mock_location "durp.py"];
           due_to_any = false;
         }))


let test_filter _ =
  let environment =
    Environment.handler ~configuration (Environment.Builder.create ~configuration ())
  in
  add_defaults_to_environment ~configuration environment;
  Environment.populate
    environment
    ~configuration
    [
      parse {|
        class Foo: ...
        class MockChild(unittest.mock.Mock): ...
        class NonMockChild(Foo): ...
      |};
    ];
  let resolution = Environment.resolution environment () in
  let assert_filtered ?(define = mock_define) ~configuration kind =
    assert_equal
      []
      (Error.filter ~resolution ~configuration [error ~define kind])
  in
  let assert_not_filtered ?(define = mock_define) ~configuration kind =
    let errors = [error ~define kind] in
    assert_equal
      ~cmp:(List.equal ~equal:Error.equal)
      errors
      (Error.filter ~resolution ~configuration errors)
  in

  (* Test different modes. *)
  let debug = Configuration.create ~debug:true () in
  assert_not_filtered ~configuration:debug (Error.UndefinedType Type.Object);

  let infer = Configuration.create ~infer:true () in
  assert_filtered ~configuration:infer (missing_return Type.Top);
  assert_filtered ~configuration:infer (missing_return Type.Object);
  assert_not_filtered ~configuration:infer (missing_return Type.integer);
  assert_filtered ~configuration:infer (Error.UndefinedType Type.integer);

  let strict = Configuration.create ~strict:true () in
  assert_not_filtered ~configuration:strict (missing_return Type.Top);
  assert_filtered ~configuration:strict (Error.IncompatibleAwaitableType Type.Top);
  assert_not_filtered ~configuration:strict (missing_return Type.Object);

  let default = Configuration.create () in
  assert_filtered ~configuration:default (missing_return Type.integer);
  assert_not_filtered ~configuration:default (incompatible_return_type Type.integer Type.float);
  assert_filtered ~configuration:default (incompatible_return_type Type.integer Type.Object);

  (* Suppress mock errors. *)
  assert_filtered
    ~configuration:default
    (incompatible_return_type (Type.primitive "unittest.mock.Mock") Type.integer);
  assert_not_filtered
    ~configuration:default
    (incompatible_return_type Type.integer (Type.primitive "unittest.mock.Mock"));
  assert_filtered
    ~configuration:default
    (undefined_attribute (Type.primitive "MockChild"));
  assert_not_filtered
    ~configuration:default
    (undefined_attribute (Type.primitive "NonMockChild"));

  (* Suppress callable errors. *)
  assert_filtered
    ~configuration:default
    (incompatible_return_type (Type.parametric "typing.Callable" []) Type.integer);
  assert_filtered
    ~configuration:default
    (incompatible_return_type Type.integer (Type.parametric "typing.Callable" []));

  (* Suppress return errors in unimplemented defines. *)
  assert_not_filtered
    ~configuration:default
    (incompatible_return_type Type.integer Type.float);
  assert_filtered
    ~define:(define ~body:[+Statement.Pass; +Statement.Return None] ())
    ~configuration:default
    (incompatible_return_type Type.integer Type.float)


let () =
  "error">:::[
    "due_to_analysis_limitations">::test_due_to_analysis_limitations;
    "join">::test_join;
    "filter">::test_filter;
  ]
  |> run_test_tt_main
