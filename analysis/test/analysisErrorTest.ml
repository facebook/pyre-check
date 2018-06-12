(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Statement
open Test


let define_value ?(return_annotation = Some !"int") ?(body = []) ?(name = "foo") () =
  {
    Statement.Define.name = Access.create name;
    parameters = [];
    body;
    decorators = [];
    docstring = None;
    return_annotation;
    async = false;
    generated = false;
    parent = None;
  }


let untyped_define =
  +(define_value ~return_annotation:None ~body:[] ())


let define ?(body = []) () =
  +(define_value ~body ())


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


let revealed_type access annotation =
  Error.RevealedType {
    Error.expression = Node.create_with_default_location (Access (Access.create access));
    annotation;
  }


let missing_return annotation =
  Error.MissingReturnAnnotation {
    Error.annotation;
    evidence_locations = [];
    due_to_any = false;
  }


let incompatible_return_type actual expected =
  Error.IncompatibleReturnType {
    Error.mismatch = { Error.actual; expected };
    is_implicit = false;
  }


let undefined_attribute actual =
  Error.UndefinedAttribute {
    Error.attribute = Access.create "foo";
    origin = Error.Class {
        Error.annotation = actual;
        class_attribute = false;
      };
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
                Error.name = [Access.Identifier (~~"")];
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
                Error.name = [Access.Identifier (~~"")];
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
                Error.name = [Access.Identifier (~~"")];
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
              Error.name = [Access.Identifier (~~"")];
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
              Error.name = [Access.Identifier (~~"")];
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
              Error.name = (Access.create "");
              annotation = Type.Top;
              due_to_any = false;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.MissingParameterAnnotation {
              Error.name = (Access.create "");
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
                Error.name = [Access.Identifier ~~""];
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
                Error.name = [Access.Identifier ~~""];
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
              Error.name = Some ((Access.create ""));
              position = 1;
              callee = Some (Access.create "callee");
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.Top;
              };
            })));
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleParameterType {
              Error.name = Some ((Access.create ""));
              position = 1;
              callee = Some (Access.create "callee");
              mismatch = {
                Error.actual = Type.Top;
                expected = Type.string;
              };
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleParameterType {
              Error.name = Some ((Access.create ""));
              position = 1;
              callee = Some (Access.create "callee");
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
              Error.mismatch = {
                Error.actual = Type.Top;
                expected = Type.Top;
              };
              is_implicit = false;
            })));
  assert_true
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleReturnType {
              Error.mismatch = {
                Error.actual = Type.Top;
                expected = Type.string;
              };
              is_implicit = false;
            })));
  assert_false
    (Error.due_to_analysis_limitations
       (error
          (Error.IncompatibleReturnType {
              Error.mismatch = {
                Error.actual = Type.string;
                expected = Type.Top;
              };
              is_implicit = false;
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
    let resolution =
      Environment.resolution
        environment
        ~define:(Statement.Define.create_toplevel ~qualifier:[] ~statements:[])
        ()
    in
    let result = Error.join ~resolution left right in
    assert_equal ~cmp:Error.equal result expected
  in
  assert_join
    (error
       (Error.IncompatibleAttributeType {
           Error.parent = mock_parent;
           incompatible_type = {
             Error.name = [Access.Identifier (~~"")];
             mismatch = {
               Error.actual = Type.Top;
               expected = Type.Top;
             };
             declare_location = Location.any;
           };
         }))
    (error (Error.IncompatibleVariableType {
         Error.name = [Access.Identifier (~~"")];
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
           Error.name = Some (Access.create "");
           position = 1;
           callee = Some (Access.create "callee");
           mismatch = {
             Error.actual = Type.integer;
             expected = Type.string;
           };
         }))
    (error
       (Error.IncompatibleParameterType {
           Error.name = Some (Access.create "");
           position = 1;
           callee = Some (Access.create "callee");
           mismatch = {
             Error.actual = Type.float;
             expected = Type.string;
           };
         }))
    (error
       (Error.IncompatibleParameterType {
           Error.name = Some (Access.create "");
           position = 1;
           callee = Some (Access.create "callee");
           mismatch = {
             Error.actual = Type.float;
             expected = Type.string;
           };
         }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier (~~"")];
           annotation = Type.integer;
           evidence_locations = [create_mock_location "derp.py"];
           due_to_any = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier (~~"")];
           annotation = Type.float;
           evidence_locations = [create_mock_location "durp.py"];
           due_to_any = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier (~~"")];
           annotation = Type.float;
           evidence_locations = [create_mock_location "derp.py"; create_mock_location "durp.py"];
           due_to_any = false;
         }));

  assert_join
    (error (revealed_type "a" Type.integer))
    (error (revealed_type "a" Type.float))
    (error (revealed_type "a" Type.float));
  assert_join
    (error (revealed_type "a" Type.integer))
    (error (revealed_type "b" Type.float))
    (error (Error.Top))



let test_filter _ =
  let open Error in
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
        class NonCallableChild(unittest.mock.NonCallableMock): ...
        class NonMockChild(Foo): ...
      |};
    ];
  let resolution =
    Environment.resolution
      environment
      ~define:(Statement.Define.create_toplevel ~qualifier:[] ~statements:[])
      ()
  in
  let assert_filtered ?(define = mock_define) kind =
    let errors = [error ~define kind] in
    assert_equal
      []
      (filter ~configuration ~resolution errors)
  in
  let assert_unfiltered ?(define = mock_define) kind =
    let errors = [error ~define kind] in
    assert_equal
      ~cmp:(List.equal ~equal)
      errors
      (filter ~configuration ~resolution errors)
  in
  (* Suppress mock errors. *)
  assert_filtered (incompatible_return_type (Type.primitive "unittest.mock.Mock") Type.integer);
  assert_unfiltered (incompatible_return_type Type.integer (Type.primitive "unittest.mock.Mock"));
  assert_filtered (undefined_attribute (Type.primitive "MockChild"));
  assert_filtered (undefined_attribute (Type.primitive "NonCallableChild"));
  assert_unfiltered (undefined_attribute (Type.primitive "NonMockChild"));
  assert_filtered (undefined_attribute (Type.Optional (Type.primitive "NonCallableChild")));
  assert_unfiltered (incompatible_return_type (Type.Optional Type.Bottom) Type.integer);

  (* Suppress callable errors. *)
  assert_filtered
    (incompatible_return_type (Type.callable ~annotation:Type.integer ()) Type.integer);
  assert_filtered
    (incompatible_return_type Type.integer (Type.callable ~annotation:Type.integer ()));

  (* Suppress return errors in unimplemented defines. *)
  assert_unfiltered (incompatible_return_type Type.integer Type.float);
  assert_filtered
    ~define:(define ~body:[+Statement.Pass;
                           +Statement.Return { Return.expression = None; is_implicit = false }] ())
    (incompatible_return_type Type.integer Type.float);

  let inconsistent_override name override =
    InconsistentOverride {
      overridden_method =
        Annotated.Class.Method.create ~define:(define_value ~name ()) ~parent:(mock_parent);
      override;
    }
  in
  (* Suppress parameter errors on override of dunder methods *)
  assert_unfiltered
    (inconsistent_override
       "foo"
       (StrengthenedPrecondition (NotFound (Access.create "x"))));
  assert_unfiltered
    (inconsistent_override
       "__foo__"
       (WeakenedPostcondition { actual = Type.Top; expected = Type.integer }));
  assert_unfiltered
    (inconsistent_override
       "__foo__"
       (StrengthenedPrecondition (Found { actual = Type.none; expected = Type.integer })));
  assert_filtered
    (inconsistent_override
       "__foo__"
       (StrengthenedPrecondition (NotFound (Access.create "x"))))


let test_suppress _ =
  let assert_suppressed mode ?(define = mock_define) kind =
    assert_equal
      true
      (Error.suppress ~mode (error ~define kind))
  in
  let assert_not_suppressed mode ?(define = mock_define) kind =
    assert_equal
      false
      (Error.suppress ~mode (error ~define kind))
  in

  (* Test different modes. *)
  assert_suppressed Source.Infer (missing_return Type.Top);
  assert_suppressed Source.Infer (missing_return Type.Object);
  assert_not_suppressed Source.Infer (missing_return Type.integer);
  assert_suppressed Source.Infer (Error.UndefinedType Type.integer);

  assert_not_suppressed Source.Strict (missing_return Type.Top);
  assert_suppressed Source.Strict (Error.IncompatibleAwaitableType Type.Top);
  assert_not_suppressed Source.Strict (missing_return Type.Object);

  assert_suppressed Source.Default (missing_return Type.integer);
  assert_not_suppressed Source.Default (incompatible_return_type Type.integer Type.float);
  assert_suppressed Source.Default (incompatible_return_type Type.integer Type.Object);
  assert_not_suppressed Source.Default (revealed_type "a" Type.integer);
  assert_not_suppressed ~define:untyped_define Source.Default (revealed_type "a" Type.integer);
  assert_suppressed Source.Default (Error.UndefinedName (Access.create "reveal_type"))


let () =
  "error">:::[
    "due_to_analysis_limitations">::test_due_to_analysis_limitations;
    "join">::test_join;
    "filter">::test_filter;
    "suppress">::test_suppress;
  ]
  |> run_test_tt_main
