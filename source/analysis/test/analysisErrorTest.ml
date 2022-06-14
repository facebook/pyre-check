(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Statement
open Test
module Error = Analysis.AnalysisError

let signature_value ?(return_annotation = Some !"int") ?(name = "foo") () =
  {
    Define.Signature.name = Reference.create name;
    parameters = [];
    decorators = [];
    return_annotation;
    async = false;
    generator = false;
    parent = None;
    nesting_define = None;
  }


let define_value ?(return_annotation = Some !"int") ?(body = []) ?(name = "foo") () =
  {
    Define.signature = signature_value ~return_annotation ~name ();
    captures = [];
    unbound_names = [];
    body;
  }


let untyped_signature = +signature_value ~return_annotation:None ()

let signature () = +signature_value ()

let define ?(body = []) () = +define_value ~body ()

let mock_signature = signature ()

let mock_define = define ()

let mock_parent = Type.Primitive "foo"

let error
    ?(qualifier = Reference.empty)
    ?(signature = mock_signature)
    ?(location = Location.any)
    kind
  =
  { Error.location = Location.with_module ~module_reference:qualifier location; kind; signature }


let revealed_type expression annotation =
  Error.RevealedType
    { expression = parse_single_expression expression; annotation; qualify = false }


let missing_return annotation =
  Error.MissingReturnAnnotation
    {
      name = !&"$return_annotation";
      annotation = Some annotation;
      given_annotation = None;
      evidence_locations = [];
      thrown_at_source = true;
    }


let incompatible_return_type
    ?(is_unimplemented = false)
    ?(due_to_invariance = false)
    actual
    expected
  =
  Error.IncompatibleReturnType
    {
      mismatch = { Error.actual; expected; due_to_invariance };
      is_implicit = false;
      is_unimplemented;
      define_location = Node.location mock_define;
    }


let undefined_attribute actual =
  Error.UndefinedAttribute
    {
      attribute = "foo";
      origin = Error.Class { class_origin = ClassType actual; parent_module_path = None };
    }


let unexpected_keyword name callee =
  Error.UnexpectedKeyword { name; callee = callee >>| Reference.create }


let test_due_to_analysis_limitations _ =
  let assert_due_to_analysis_limitations kind =
    assert_true (Error.due_to_analysis_limitations (error kind))
  in
  let assert_not_due_to_analysis_limitations kind =
    assert_false (Error.due_to_analysis_limitations (error kind))
  in
  (* IncompatibleAttributeType. *)
  assert_due_to_analysis_limitations
    (Error.IncompatibleAttributeType
       {
         parent = mock_parent;
         incompatible_type =
           {
             Error.name = !&"";
             mismatch = { Error.actual = Type.Top; expected = Type.Top; due_to_invariance = false };
           };
       });
  assert_due_to_analysis_limitations
    (Error.IncompatibleAttributeType
       {
         parent = mock_parent;
         incompatible_type =
           {
             Error.name = !&"";
             mismatch =
               { Error.actual = Type.Top; expected = Type.string; due_to_invariance = false };
           };
       });
  assert_not_due_to_analysis_limitations
    (Error.IncompatibleAttributeType
       {
         parent = mock_parent;
         incompatible_type =
           {
             Error.name = !&"";
             mismatch =
               { Error.actual = Type.string; expected = Type.Top; due_to_invariance = false };
           };
       });

  (* Initialization *)
  assert_not_due_to_analysis_limitations
    (Error.UninitializedAttribute
       {
         name = "";
         parent = mock_parent;
         mismatch =
           {
             Error.actual = Type.string;
             expected = Type.optional Type.string;
             due_to_invariance = false;
           };
         kind = Class;
       });

  (* MissingParameterAnnotation. *)
  assert_not_due_to_analysis_limitations
    (Error.MissingParameterAnnotation
       {
         name = !&"";
         annotation = Some Type.Top;
         given_annotation = None;
         evidence_locations = [];
         thrown_at_source = true;
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingParameterAnnotation
       {
         name = !&"";
         annotation = None;
         given_annotation = Some Type.Top;
         evidence_locations = [];
         thrown_at_source = true;
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingParameterAnnotation
       {
         name = !&"";
         annotation = Some Type.string;
         given_annotation = None;
         evidence_locations = [];
         thrown_at_source = true;
       });

  (* MissingReturnAnnotation. *)
  assert_not_due_to_analysis_limitations
    (Error.MissingReturnAnnotation
       {
         name = !&"$return_annotation";
         annotation = Some Type.Top;
         given_annotation = None;
         evidence_locations = [];
         thrown_at_source = true;
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingReturnAnnotation
       {
         name = !&"$return_annotation";
         annotation = None;
         given_annotation = Some Type.Top;
         evidence_locations = [];
         thrown_at_source = true;
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingReturnAnnotation
       {
         name = !&"$return_annotation";
         annotation = Some Type.string;
         given_annotation = None;
         evidence_locations = [];
         thrown_at_source = true;
       });

  (* MissingAttributeAnnotation *)
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation
       {
         parent = mock_parent;
         missing_annotation =
           {
             Error.name = !&"";
             annotation = Some Type.Top;
             given_annotation = None;
             evidence_locations = [];
             thrown_at_source = true;
           };
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation
       {
         parent = mock_parent;
         missing_annotation =
           {
             Error.name = !&"";
             annotation = None;
             given_annotation = Some Type.Top;
             evidence_locations = [];
             thrown_at_source = true;
           };
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation
       {
         parent = mock_parent;
         missing_annotation =
           {
             Error.name = !&"";
             annotation = Some Type.string;
             given_annotation = None;
             evidence_locations = [];
             thrown_at_source = true;
           };
       });
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation
       {
         parent = mock_parent;
         missing_annotation =
           {
             Error.name = !&"";
             annotation = None;
             given_annotation = None;
             evidence_locations = [];
             thrown_at_source = true;
           };
       });

  (* Parameter. *)
  assert_due_to_analysis_limitations
    (Error.IncompatibleParameterType
       {
         name = Some "";
         position = 1;
         callee = Some !&"callee";
         mismatch = { Error.actual = Type.Top; expected = Type.Top; due_to_invariance = false };
       });
  assert_due_to_analysis_limitations
    (Error.IncompatibleParameterType
       {
         name = Some "";
         position = 1;
         callee = Some !&"callee";
         mismatch = { Error.actual = Type.Top; expected = Type.string; due_to_invariance = false };
       });
  assert_not_due_to_analysis_limitations
    (Error.IncompatibleParameterType
       {
         name = Some "";
         position = 1;
         callee = Some !&"callee";
         mismatch = { Error.actual = Type.string; expected = Type.Top; due_to_invariance = false };
       });
  assert_due_to_analysis_limitations
    (Error.IncompatibleParameterType
       {
         name = Some "";
         position = 1;
         callee = Some !&"callee";
         mismatch =
           {
             Error.actual = Type.Primitive "typing_extensions.TypeAlias";
             expected = Type.Top;
             due_to_invariance = false;
           };
       });

  (* Return. *)
  assert_due_to_analysis_limitations
    (Error.IncompatibleReturnType
       {
         mismatch = { Error.actual = Type.Top; expected = Type.Top; due_to_invariance = false };
         is_implicit = false;
         is_unimplemented = false;
         define_location = Node.location mock_define;
       });
  assert_due_to_analysis_limitations
    (Error.IncompatibleReturnType
       {
         mismatch = { Error.actual = Type.Top; expected = Type.string; due_to_invariance = false };
         is_implicit = false;
         is_unimplemented = false;
         define_location = Node.location mock_define;
       });
  assert_not_due_to_analysis_limitations
    (Error.IncompatibleReturnType
       {
         mismatch = { Error.actual = Type.string; expected = Type.Top; due_to_invariance = false };
         is_implicit = false;
         is_unimplemented = false;
         define_location = Node.location mock_define;
       });

  (* UndefinedType. *)
  assert_not_due_to_analysis_limitations (Error.UndefinedType Type.Top);
  assert_not_due_to_analysis_limitations (Error.UndefinedType Type.string);

  (* Unpack. *)
  assert_not_due_to_analysis_limitations
    (Error.Unpack { expected_count = 2; unpack_problem = CountMismatch 3 });
  assert_not_due_to_analysis_limitations
    (Error.Unpack { expected_count = 2; unpack_problem = UnacceptableType Type.integer });
  assert_due_to_analysis_limitations
    (Error.Unpack { expected_count = 2; unpack_problem = UnacceptableType Type.Top })


let test_join context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  let assert_join left right expected =
    let result = Error.join ~resolution left right in
    assert_equal ~printer:Error.show ~cmp:[%compare.equal: Error.t] expected result
  in
  assert_join
    (error
       (Error.IncompatibleAttributeType
          {
            parent = mock_parent;
            incompatible_type =
              {
                Error.name = !&"";
                mismatch =
                  { Error.actual = Type.Top; expected = Type.Top; due_to_invariance = false };
              };
          }))
    (error
       (Error.IncompatibleVariableType
          {
            incompatible_type =
              {
                Error.name = !&"";
                mismatch =
                  { Error.actual = Type.Top; expected = Type.Top; due_to_invariance = false };
              };
            declare_location = Location.WithPath.any;
          }))
    (error Error.Top);
  assert_join
    (error
       (Error.IncompatibleParameterType
          {
            name = Some "";
            position = 1;
            callee = Some !&"callee";
            mismatch =
              { Error.actual = Type.integer; expected = Type.string; due_to_invariance = false };
          }))
    (error
       (Error.IncompatibleParameterType
          {
            name = Some "";
            position = 1;
            callee = Some !&"callee";
            mismatch =
              { Error.actual = Type.float; expected = Type.string; due_to_invariance = false };
          }))
    (error
       (Error.IncompatibleParameterType
          {
            name = Some "";
            position = 1;
            callee = Some !&"callee";
            mismatch =
              { Error.actual = Type.float; expected = Type.string; due_to_invariance = false };
          }));
  let create_mock_location path =
    {
      Location.WithPath.path;
      start = { Location.line = 1; column = 1 };
      stop = { Location.line = 1; column = 1 };
    }
  in
  assert_join
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.integer;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.float;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.float;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.integer;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = None;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.integer;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = None;
            given_annotation = None;
            evidence_locations = [];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.float;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.float;
            given_annotation = None;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.float;
            given_annotation = Some Type.Any;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = true;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.integer;
            given_annotation = Some Type.Any;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = false;
          }))
    (error
       (Error.MissingGlobalAnnotation
          {
            Error.name = !&"";
            annotation = Some Type.float;
            given_annotation = Some Type.Any;
            evidence_locations = [create_mock_location "derp.py"];
            thrown_at_source = true;
          }));
  assert_join
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }))
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }))
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }));
  assert_join
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }))
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 4 }))
    (error Error.Top);
  assert_join
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }))
    (error (Error.Unpack { expected_count = 3; unpack_problem = Error.CountMismatch 3 }))
    (error Error.Top);
  assert_join
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer }))
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }))
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }));
  assert_join
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }))
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer }))
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }));
  assert_join
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }))
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }))
    (error Error.Top);
  assert_join
    (error
       (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }))
    (error
       (Error.Unpack { expected_count = 3; unpack_problem = Error.UnacceptableType Type.float }))
    (error Error.Top);
  assert_join
    (error (Error.UndefinedType (Type.Primitive "derp")))
    (error (Error.UndefinedType (Type.Primitive "derp")))
    (error (Error.UndefinedType (Type.Primitive "derp")));
  assert_join
    (error (Error.UndefinedType (Type.Primitive "derp")))
    (error (Error.UndefinedType (Type.Primitive "herp")))
    (error Error.Top);
  assert_join
    (error (Error.AnalysisFailure (UnexpectedUndefinedType "derp")))
    (error (Error.AnalysisFailure (UnexpectedUndefinedType "derp")))
    (error (Error.AnalysisFailure (UnexpectedUndefinedType "derp")));
  assert_join
    (error (Error.AnalysisFailure (UnexpectedUndefinedType "derp")))
    (error (Error.AnalysisFailure (UnexpectedUndefinedType "herp")))
    (error Error.Top);
  assert_join
    (error (Error.AnalysisFailure (FixpointThresholdReached { define = !&"foo.bar" })))
    (error (Error.AnalysisFailure (FixpointThresholdReached { define = !&"foo.bar" })))
    (error (Error.AnalysisFailure (FixpointThresholdReached { define = !&"foo.bar" })));
  assert_join
    (error (Error.AnalysisFailure (FixpointThresholdReached { define = !&"foo.bar" })))
    (error (Error.AnalysisFailure (FixpointThresholdReached { define = !&"foo.not_bar" })))
    (error Error.Top);
  assert_join
    (error (revealed_type "a" (Annotation.create_mutable Type.string)))
    (error (revealed_type "a" (Annotation.create_mutable Type.float)))
    (error (revealed_type "a" (Annotation.create_mutable (Type.union [Type.string; Type.float]))));
  assert_join
    (error (revealed_type "a" (Annotation.create_immutable Type.string)))
    (error (revealed_type "a" (Annotation.create_immutable Type.float)))
    (error (revealed_type "a" (Annotation.create_immutable (Type.union [Type.string; Type.float]))));
  assert_join
    (error (revealed_type "a" (Annotation.create_immutable Type.integer)))
    (error (revealed_type "a" (Annotation.create_immutable Type.integer)))
    (error (revealed_type "a" (Annotation.create_immutable Type.integer)));
  assert_join
    (error (revealed_type "a" (Annotation.create_mutable Type.integer)))
    (error (revealed_type "b" (Annotation.create_mutable Type.float)))
    (error Error.Top);
  let stop = { Location.line = 42; column = 42 } in
  assert_join
    (error
       ~location:{ Location.start = { Location.line = 1; column = 0 }; stop }
       (revealed_type "a" (Annotation.create_mutable Type.integer)))
    (error
       ~location:{ Location.start = { Location.line = 2; column = 1 }; stop }
       (revealed_type "a" (Annotation.create_mutable Type.float)))
    (error
       ~location:{ Location.start = { Location.line = 1; column = 0 }; stop }
       (revealed_type "a" (Annotation.create_mutable Type.float)))


let test_less_or_equal context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  assert_true
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer }))
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer })));
  assert_true
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer }))
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.float }))
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack { expected_count = 3; unpack_problem = Error.UnacceptableType Type.integer }))
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer })));
  assert_true
    (Error.less_or_equal
       ~resolution
       (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 2 }))
       (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 2 })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 2 }))
       (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 2 }))
       (error
          (Error.Unpack { expected_count = 2; unpack_problem = Error.UnacceptableType Type.integer })));
  assert_true
    (Error.less_or_equal
       ~resolution
       (error (revealed_type "a" (Annotation.create_immutable Type.integer)))
       (error (revealed_type "a" (Annotation.create_immutable Type.integer))));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error (revealed_type "a" (Annotation.create_immutable Type.float)))
       (error (revealed_type "a" (Annotation.create_immutable Type.integer))))


let test_filter context =
  let open Error in
  let resolution =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
            class Foo: ...
            class MockChild(unittest.mock.Mock): ...
            class NonCallableChild(unittest.mock.NonCallableMock): ...
            class NonMockChild(Foo): ...
          |}
        );
      ]
    |> ScratchProject.build_global_resolution
  in
  let assert_filtered ?(location = Location.any) ?(signature = mock_signature) kind =
    let errors = [error ~signature ~location kind] in
    assert_equal [] (filter ~resolution errors)
  in
  let assert_unfiltered ?qualifier ?(location = Location.any) ?(signature = mock_signature) kind =
    let errors = [error ?qualifier ~signature ~location kind] in
    assert_equal ~cmp:(List.equal [%compare.equal: t]) errors (filter ~resolution errors)
  in
  (* Suppress stub errors. *)
  assert_unfiltered
    ~location:Location.any
    ~qualifier:!&"stub"
    (undefined_attribute (Type.Primitive "Foo"));
  assert_unfiltered ~location:Location.any (undefined_attribute (Type.Primitive "Foo"));

  (* Suppress mock errors. *)
  assert_filtered (incompatible_return_type (Type.Primitive "unittest.mock.Mock") Type.integer);
  assert_unfiltered (incompatible_return_type Type.integer (Type.Primitive "unittest.mock.Mock"));
  assert_filtered (undefined_attribute (Type.Primitive "test.MockChild"));
  assert_filtered (undefined_attribute (Type.Primitive "test.NonCallableChild"));
  assert_unfiltered (undefined_attribute (Type.Primitive "test.NonMockChild"));
  assert_filtered (undefined_attribute (Type.optional (Type.Primitive "test.NonCallableChild")));
  assert_unfiltered (incompatible_return_type Type.NoneType Type.integer);
  assert_filtered (unexpected_keyword "foo" (Some "unittest.mock.call"));
  assert_unfiltered (unexpected_keyword "foo" None);

  (* Return errors are not suppressed for unimplemented defines because they are shown in strict
     mode. *)
  assert_unfiltered (incompatible_return_type Type.integer Type.float);
  assert_unfiltered (incompatible_return_type Type.integer Type.float ~is_unimplemented:true);

  let abstract_class_instantiation name =
    InvalidClassInstantiation
      (AbstractClassInstantiation { class_name = !&name; abstract_methods = [] })
  in
  (* Suppress errors due to typeshed inconsistencies. *)
  assert_filtered (abstract_class_instantiation "int");
  assert_filtered (abstract_class_instantiation "float");
  assert_filtered (abstract_class_instantiation "bool");
  assert_unfiltered (abstract_class_instantiation "str");

  assert_unfiltered (undefined_attribute (Type.Callable.create ~annotation:Type.integer ()));
  assert_filtered
    (undefined_attribute (Type.Callable.create ~annotation:(Type.Primitive "test.MockChild") ()));

  assert_unfiltered
    (UndefinedAttribute
       {
         attribute = "something";
         origin =
           Class
             {
               class_origin = ClassType (Type.Callable.create ~annotation:Type.integer ());
               parent_module_path = None;
             };
       });
  assert_filtered
    (UndefinedAttribute
       {
         attribute = "assert_not_called";
         origin =
           Class
             {
               class_origin = ClassType (Type.Callable.create ~annotation:Type.integer ());
               parent_module_path = None;
             };
       });
  assert_unfiltered
    (UndefinedAttribute
       {
         attribute = "something";
         origin =
           Class
             {
               class_origin =
                 ClassType
                   (Type.parametric
                      "BoundMethod"
                      [
                        Single (Type.Callable.create ~annotation:Type.integer ());
                        Single Type.integer;
                      ]);
               parent_module_path = None;
             };
       });
  assert_filtered
    (UndefinedAttribute
       {
         attribute = "assert_not_called";
         origin =
           Class
             {
               class_origin =
                 ClassType
                   (Type.parametric
                      "BoundMethod"
                      [
                        Single (Type.Callable.create ~annotation:Type.integer ());
                        Single Type.integer;
                      ]);
               parent_module_path = None;
             };
       });

  ()


let test_suppress _ =
  let assert_suppressed mode ?(ignore_codes = []) ?(signature = mock_signature) ?location kind =
    assert_equal true (Error.suppress ~mode ~ignore_codes (error ~signature ?location kind))
  in
  let assert_not_suppressed mode ?(ignore_codes = []) ?(signature = mock_signature) kind =
    assert_equal false (Error.suppress ~mode ~ignore_codes (error ~signature kind))
  in
  (* Test different modes. *)
  assert_not_suppressed Source.Debug (missing_return Type.Top);
  assert_not_suppressed Source.Debug (missing_return Type.Any);
  assert_not_suppressed Source.Debug (Error.UndefinedType Type.integer);
  assert_not_suppressed Source.Debug (Error.AnalysisFailure (UnexpectedUndefinedType "derp"));
  assert_not_suppressed Source.Strict (missing_return Type.Top);
  assert_suppressed Source.Strict (Error.IncompatibleAwaitableType Type.Top);
  assert_not_suppressed Source.Strict (missing_return Type.Any);
  assert_not_suppressed Source.Strict (Error.AnalysisFailure (UnexpectedUndefinedType "int"));
  assert_suppressed Source.Unsafe (missing_return Type.Top);
  assert_not_suppressed Source.Strict (Error.InvalidDecoration (CouldNotResolve !"test"));
  assert_suppressed Source.Unsafe (Error.InvalidDecoration (CouldNotResolve !"test"));

  (* Should not be made *)
  assert_not_suppressed Source.Unsafe (incompatible_return_type Type.integer Type.Any);
  assert_not_suppressed Source.Unsafe (revealed_type "a" (Annotation.create_mutable Type.integer));
  assert_not_suppressed
    ~signature:untyped_signature
    Source.Unsafe
    (revealed_type "a" (Annotation.create_mutable Type.integer));
  assert_not_suppressed Source.Unsafe (Error.AnalysisFailure (UnexpectedUndefinedType "int"));
  assert_suppressed
    Source.Unsafe
    (Error.InvalidTypeParameters
       {
         name = "dict";
         kind =
           IncorrectNumberOfParameters
             { expected = 2; actual = 0; can_accept_more_parameters = false };
       });
  assert_not_suppressed
    Source.Unsafe
    (Error.InvalidTypeParameters
       {
         name = "dict";
         kind =
           IncorrectNumberOfParameters
             { expected = 2; actual = 1; can_accept_more_parameters = false };
       });
  assert_not_suppressed
    Source.Strict
    (Error.InvalidTypeParameters
       {
         name = "dict";
         kind =
           IncorrectNumberOfParameters
             { expected = 2; actual = 0; can_accept_more_parameters = false };
       });
  let suppress_missing_return = [Error.code (error (missing_return Type.Any))] in
  assert_suppressed
    Source.Unsafe
    ~ignore_codes:suppress_missing_return
    (missing_return Type.integer);
  assert_suppressed
    Source.Strict
    ~ignore_codes:suppress_missing_return
    (missing_return Type.integer);
  assert_suppressed Source.Unsafe ~ignore_codes:suppress_missing_return (missing_return Type.Any);

  (* Defer to Default policy if not specifically suppressed *)
  assert_not_suppressed
    Source.Unsafe
    ~ignore_codes:suppress_missing_return
    (incompatible_return_type Type.integer Type.float);

  assert_suppressed
    Source.Declare
    (incompatible_return_type (Type.Primitive "donotexist") (Type.Primitive "meneither"));
  assert_not_suppressed
    Source.Unsafe
    (incompatible_return_type (Type.Primitive "donotexist") (Type.Primitive "meneither"));
  assert_not_suppressed
    Source.Strict
    (incompatible_return_type (Type.Primitive "donotexist") (Type.Primitive "meneither"));
  assert_suppressed
    Source.Strict
    (Error.TypedDictionaryInvalidOperation
       {
         typed_dictionary_name = "Movie";
         field_name = "name";
         method_name = "__setitem__";
         mismatch = { Error.actual = Type.Top; expected = Type.string; due_to_invariance = false };
       });

  assert_not_suppressed
    Source.Strict
    (undefined_attribute (Type.Callable.create ~annotation:Type.integer ()));
  assert_suppressed
    Source.Strict
    (undefined_attribute (Type.Callable.create ~annotation:Type.Top ()));
  ()


let test_namespace_insensitive_set _ =
  let no_namespace_variable = Type.Variable.Unary.create "A" in
  let namespaced_variable_1 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable { no_namespace_variable with namespace }
  in
  let namespaced_variable_2 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable { no_namespace_variable with namespace }
  in
  let error_1 = error (Error.NotCallable (Type.list namespaced_variable_1)) in
  let error_2 = error (Error.NotCallable (Type.list namespaced_variable_2)) in
  assert_true (Error.compare error_1 error_2 == 0);
  let set_containing_error_1 = Error.Set.add Error.Set.empty error_1 in
  assert_true (Error.Set.mem set_containing_error_1 error_2)


let test_description _ =
  let assert_messages error expected =
    let actual =
      Error.instantiate
        ~show_error_traces:false
        ~lookup:(fun _ -> None)
        {
          kind = error;
          location = Location.WithModule.any;
          signature =
            Node.create_with_default_location
              (Ast.Statement.Define.Signature.create_toplevel ~qualifier:None);
        }
      |> Error.Instantiated.description
    in
    assert_equal ~printer:Fn.id expected actual
  in
  assert_messages
    (UndefinedAttribute
       {
         attribute = "at";
         origin = Class { class_origin = ClassType Type.integer; parent_module_path = None };
       })
    "Undefined attribute [16]: `int` has no attribute `at`.";
  assert_messages
    (UndefinedAttribute
       {
         attribute = "at";
         origin =
           Class
             {
               class_origin = ClassType (Type.Callable.create ~annotation:Type.integer ());
               parent_module_path = None;
             };
       })
    "Undefined attribute [16]: Anonymous callable has no attribute `at`.";
  assert_messages
    (UndefinedAttribute
       {
         attribute = "at";
         origin =
           Class
             {
               class_origin =
                 ClassType
                   (Type.Callable.create
                      ~name:(Reference.create "named")
                      ~annotation:Type.integer
                      ());
               parent_module_path = None;
             };
       })
    "Undefined attribute [16]: Callable `named` has no attribute `at`.";
  (* TODO(T64161566): Don't pretend these are just Callables *)
  assert_messages
    (UndefinedAttribute
       {
         attribute = "at";
         origin =
           Class
             {
               class_origin =
                 ClassType
                   (Type.parametric
                      "BoundMethod"
                      [
                        Single (Type.Callable.create ~annotation:Type.integer ());
                        Single Type.integer;
                      ]);
               parent_module_path = None;
             };
       })
    "Undefined attribute [16]: Anonymous callable has no attribute `at`.";
  assert_messages
    (UndefinedAttribute
       {
         attribute = "at";
         origin =
           Class
             {
               class_origin =
                 ClassType
                   (Type.parametric
                      "BoundMethod"
                      [
                        Single
                          (Type.Callable.create
                             ~name:(Reference.create "named")
                             ~annotation:Type.integer
                             ());
                        Single Type.integer;
                      ]);
               parent_module_path = None;
             };
       })
    "Undefined attribute [16]: Callable `named` has no attribute `at`.";
  ()


let test_weaken_literals _ =
  let assert_weakened error expected =
    assert_equal ~printer:Error.show_kind expected (Error.weaken_literals error)
  in
  assert_weakened
    (Error.IncompatibleVariableType
       {
         incompatible_type =
           {
             Error.name = !&"";
             mismatch =
               {
                 Error.actual = Type.parametric "Foo" [Single (Type.literal_integer 42)];
                 expected = Type.parametric "Foo" [Single Type.integer];
                 due_to_invariance = false;
               };
           };
         declare_location = Location.WithPath.any;
       })
    (Error.IncompatibleVariableType
       {
         incompatible_type =
           {
             Error.name = !&"";
             mismatch =
               {
                 Error.actual = Type.parametric "Foo" [Single (Type.literal_integer 42)];
                 expected = Type.parametric "Foo" [Single Type.integer];
                 due_to_invariance = false;
               };
           };
         declare_location = Location.WithPath.any;
       });
  ()


let test_simplification_map _ =
  let assert_simplification_map names expected_simplifications =
    let references = List.map ~f:Reference.create names in
    let computed = Error.SimplificationMap.create references in
    let expected =
      expected_simplifications
      |> List.map ~f:(fun (x, y) -> Reference.create x, Reference.create y)
      |> Reference.Map.of_alist_exn
    in
    assert_equal
      ~cmp:(Map.equal Reference.equal)
      ~printer:Error.SimplificationMap.show
      expected
      computed
  in
  assert_simplification_map [] [];
  assert_simplification_map [""] [];
  assert_simplification_map ["a"] [];
  assert_simplification_map ["a.b"] ["a.b", "b"];
  assert_simplification_map ["a.b.c"] ["a.b.c", "c"];
  assert_simplification_map ["a"; "a"] [];
  assert_simplification_map ["a"; "b"] [];
  assert_simplification_map ["Foo.a"; "Foo.b"] ["Foo.a", "a"; "Foo.b", "b"];
  assert_simplification_map ["Foo.a"; "Foo.a"] ["Foo.a", "a"];
  assert_simplification_map ["Foo.a"; "Bar.a"] [];
  assert_simplification_map ["Foo.Bar.a"; "Foo.Bar.b"] ["Foo.Bar.a", "a"; "Foo.Bar.b", "b"];
  assert_simplification_map ["X.Foo.Bar.a"; "X.Foo.Bar.b"] ["X.Foo.Bar.a", "a"; "X.Foo.Bar.b", "b"];
  assert_simplification_map ["a"; "Foo.a"] [];
  assert_simplification_map ["a"; "Foo.a"; "Bar.a"] [];
  assert_simplification_map ["a"; "X.Foo.a"; "X.Bar.a"] ["X.Foo.a", "Foo.a"; "X.Bar.a", "Bar.a"];
  assert_simplification_map ["Foo.Bar"; "Bar.Foo"] ["Foo.Bar", "Bar"; "Bar.Foo", "Foo"];
  assert_simplification_map
    [
      "typing.Tuple";
      "str";
      "typing.Union";
      "None";
      "typing.Mapping";
      "typing.Sequence";
      "bool";
      "bytes";
      "datetime.date";
      "datetime.datetime";
      "datetime.time";
      "datetime.timedelta";
      "decimal.Decimal";
      "float";
      "int";
    ]
    [
      "typing.Tuple", "Tuple";
      "typing.Union", "Union";
      "typing.Mapping", "Mapping";
      "typing.Sequence", "Sequence";
      "datetime.date", "date";
      "datetime.datetime", "datetime";
      "datetime.time", "time";
      "datetime.timedelta", "timedelta";
      "decimal.Decimal", "Decimal";
    ];
  ()


let test_simplify_mismatch _ =
  let create_mismatch actual expected =
    let create_type s =
      Type.create
        ~aliases:(fun ?replace_unbound_parameters_with_any:_ _ -> None)
        (parse_single_expression ~preprocess:true s)
      |> Type.dequalify Reference.Map.empty
    in
    {
      Error.actual = create_type actual;
      expected = create_type expected;
      due_to_invariance = false;
    }
  in
  let show_mismatch { Error.actual; expected; _ } = Type.show actual ^ " " ^ Type.show expected in
  let original = create_mismatch "Foo.a" "Foo.b" in
  let simplified = create_mismatch "a" "b" in
  assert_equal ~printer:show_mismatch simplified (Error.simplify_mismatch original);
  let original =
    create_mismatch "typing.Union[Foo.a, builtins.bool]" "typing.Union[Foo.b, builtins.bool]"
  in
  let simplified = create_mismatch "Union[a, bool]" "Union[b, bool]" in
  assert_equal ~printer:show_mismatch simplified (Error.simplify_mismatch original);
  ()


let () =
  "error"
  >::: [
         "due_to_analysis_limitations" >:: test_due_to_analysis_limitations;
         "join" >:: test_join;
         "less_or_equal" >:: test_less_or_equal;
         "filter" >:: test_filter;
         "suppress" >:: test_suppress;
         "namespace_insensitive_set" >:: test_namespace_insensitive_set;
         "messages" >:: test_description;
         "weaken_literals" >:: test_weaken_literals;
         "test_simplification_map" >:: test_simplification_map;
         "test_simplify_mismatch" >:: test_simplify_mismatch;
       ]
  |> Test.run
