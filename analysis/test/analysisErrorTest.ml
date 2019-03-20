(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Pyre
open Statement
open Test


let define_value ?(return_annotation = Some !"int") ?(body = []) ?(name = "foo") () =
  {
    Statement.Define.name = Reference.create name;
    parameters = [];
    body;
    decorators = [];
    docstring = None;
    return_annotation;
    async = false;
    parent = None;
  }


let untyped_define =
  +(define_value ~return_annotation:None ~body:[] ())


let define ?(body = []) () =
  +(define_value ~body ())


let mock_define =
  define ()


let mock_parent = Type.Primitive "foo"


let error ?(define = mock_define) ?(location = Location.Instantiated.any) kind =
  { Error.location; kind; define }


let revealed_type access annotation =
  Error.RevealedType {
    expression = Access.expression (Access.create access);
    annotation;
  }


let missing_return annotation =
  Error.MissingReturnAnnotation {
    name = Access.create "$return_annotation";
    annotation = Some annotation;
    given_annotation = None;
    evidence_locations = [];
    thrown_at_source = true;
  }


let incompatible_return_type ?(due_to_invariance = false) actual expected =
  Error.IncompatibleReturnType {
    mismatch = { Error.actual; expected; due_to_invariance };
    is_implicit = false;
  }


let undefined_attribute actual =
  Error.UndefinedAttribute {
    attribute = "foo";
    origin = Error.Class {
        annotation = actual;
        class_attribute = false;
      };
  }

let unexpected_keyword name callee =
  Error.UnexpectedKeyword {
    name = name;
    callee = callee >>| Access.create;
  }


let configuration = Configuration.Analysis.create ()


let test_due_to_analysis_limitations _ =
  let assert_due_to_analysis_limitations kind =
    assert_true (Error.due_to_analysis_limitations (error kind))
  in
  let assert_not_due_to_analysis_limitations kind =
    assert_false (Error.due_to_analysis_limitations (error kind))
  in

  (* IncompatibleAttributeType. *)
  assert_due_to_analysis_limitations
    (Error.IncompatibleAttributeType {
        parent = mock_parent;
        incompatible_type = {
          Error.name = [Access.Identifier ""];
          mismatch = {
            Error.actual = Type.Top;
            expected = Type.Top;
            due_to_invariance = false;
          };
          declare_location = Location.Instantiated.any;
        };
      });
  assert_due_to_analysis_limitations
    (Error.IncompatibleAttributeType {
        parent = mock_parent;
        incompatible_type = {
          Error.name = [Access.Identifier ""];
          mismatch = {
            Error.actual = Type.Top;
            expected = Type.string;
            due_to_invariance = false;
          };
          declare_location = Location.Instantiated.any;
        };
      });
  assert_not_due_to_analysis_limitations
    (Error.IncompatibleAttributeType {
        parent = mock_parent;
        incompatible_type = {
          Error.name = [Access.Identifier ""];
          mismatch = {
            Error.actual = Type.string;
            expected = Type.Top;
            due_to_invariance = false;
          };
          declare_location = Location.Instantiated.any;
        };
      });

  (* Initialization *)
  assert_due_to_analysis_limitations
    (Error.UninitializedAttribute {
        name = "";
        parent = mock_parent;
        mismatch = {
          Error.actual = Type.Top;
          expected = Type.Optional Type.Top;
          due_to_invariance = false;
        };
      });

  assert_not_due_to_analysis_limitations
    (Error.UninitializedAttribute {
        name = "";
        parent = mock_parent;
        mismatch = {
          Error.actual = Type.string;
          expected = Type.Optional Type.string;
          due_to_invariance = false;
        };
      });

  (* MissingParameterAnnotation. *)
  assert_not_due_to_analysis_limitations
    (Error.MissingParameterAnnotation {
        name = (Access.create "");
        annotation = Some Type.Top;
        given_annotation = None;
        evidence_locations = [];
        thrown_at_source = true;
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingParameterAnnotation {
        name = (Access.create "");
        annotation = None;
        given_annotation = Some Type.Top;
        evidence_locations = [];
        thrown_at_source = true;
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingParameterAnnotation {
        name = (Access.create "");
        annotation = Some Type.string;
        given_annotation = None;
        evidence_locations = [];
        thrown_at_source = true;
      });

  (* MissingReturnAnnotation. *)
  assert_not_due_to_analysis_limitations
    (Error.MissingReturnAnnotation {
        name = (Access.create "$return_annotation");
        annotation = Some Type.Top;
        given_annotation = None;
        evidence_locations = [];
        thrown_at_source = true;
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingReturnAnnotation {
        name = (Access.create "$return_annotation");
        annotation = None;
        given_annotation = Some Type.Top;
        evidence_locations = [];
        thrown_at_source = true;
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingReturnAnnotation {
        name = (Access.create "$return_annotation");
        annotation = Some Type.string;
        given_annotation = None;
        evidence_locations = [];
        thrown_at_source = true;
      });

  (* MissingAttributeAnnotation *)
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation {
        parent = mock_parent;
        missing_annotation = {
          Error.name = [Access.Identifier ""];
          annotation = Some Type.Top;
          given_annotation = None;
          evidence_locations = [];
          thrown_at_source = true;
        };
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation {
        parent = mock_parent;
        missing_annotation = {
          Error.name = [Access.Identifier ""];
          annotation = None;
          given_annotation = Some Type.Top;
          evidence_locations = [];
          thrown_at_source = true;
        };
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation {
        parent = mock_parent;
        missing_annotation = {
          Error.name = [Access.Identifier ""];
          annotation = Some Type.string;
          given_annotation = None;
          evidence_locations = [];
          thrown_at_source = true;
        };
      });
  assert_not_due_to_analysis_limitations
    (Error.MissingAttributeAnnotation {
        parent = mock_parent;
        missing_annotation = {
          Error.name = [Access.Identifier ""];
          annotation = None;
          given_annotation = None;
          evidence_locations = [];
          thrown_at_source = true;
        };
      });

  (* Parameter. *)
  assert_due_to_analysis_limitations
    (Error.IncompatibleParameterType {
        name = Some "";
        position = 1;
        callee = Some (Access.create "callee");
        mismatch = {
          Error.actual = Type.Top;
          expected = Type.Top;
          due_to_invariance = false;
        };
      });
  assert_due_to_analysis_limitations
    (Error.IncompatibleParameterType {
        name = Some "";
        position = 1;
        callee = Some (Access.create "callee");
        mismatch = {
          Error.actual = Type.Top;
          expected = Type.string;
          due_to_invariance = false;
        };
      });
  assert_not_due_to_analysis_limitations
    (Error.IncompatibleParameterType {
        name = Some "";
        position = 1;
        callee = Some (Access.create "callee");
        mismatch = {
          Error.actual = Type.string;
          expected = Type.Top;
          due_to_invariance = false;
        };
      });

  assert_due_to_analysis_limitations
    (Error.IncompatibleParameterType {
        name = Some "";
        position = 1;
        callee = Some (Access.create "callee");
        mismatch = {
          Error.actual = Type.Primitive "typing.TypeAlias";
          expected = Type.Top;
          due_to_invariance = false;
        };
      });

  (* Return. *)
  assert_due_to_analysis_limitations
    (Error.IncompatibleReturnType {
        mismatch = {
          Error.actual = Type.Top;
          expected = Type.Top;
          due_to_invariance = false;
        };
        is_implicit = false;
      });
  assert_due_to_analysis_limitations
    (Error.IncompatibleReturnType {
        mismatch = {
          Error.actual = Type.Top;
          expected = Type.string;
          due_to_invariance = false;
        };
        is_implicit = false;
      });
  assert_not_due_to_analysis_limitations
    (Error.IncompatibleReturnType {
        mismatch = {
          Error.actual = Type.string;
          expected = Type.Top;
          due_to_invariance = false;
        };
        is_implicit = false;
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


let test_due_to_mismatch_with_any _ =
  let resolution = Test.resolution () in
  let assert_due_to_mismatch_with_any kind =
    assert_true (Error.due_to_mismatch_with_any resolution (error kind))
  in
  let assert_not_due_to_mismatch_with_any kind =
    assert_false (Error.due_to_mismatch_with_any resolution (error kind))
  in
  (* ImpossibleIsinstance *)
  assert_due_to_mismatch_with_any
    (Error.ImpossibleIsinstance {
        expression = !"expression";
        mismatch = {
          Error.actual = Type.Any;
          expected = Type.Any;
          due_to_invariance = false;
        };
      });

  (* IncompatibleAttributeType. *)
  assert_due_to_mismatch_with_any
    (Error.IncompatibleAttributeType {
        parent = mock_parent;
        incompatible_type = {
          Error.name = [Access.Identifier ""];
          mismatch = {
            Error.actual = Type.Any;
            expected = Type.Any;
            due_to_invariance = false;
          };
          declare_location = Location.Instantiated.any;
        };
      });

  (* IncompatibleAwaitableType *)
  assert_due_to_mismatch_with_any (Error.IncompatibleAwaitableType Type.Any);

  (* IncompatibleParameterType *)
  assert_due_to_mismatch_with_any
    (Error.IncompatibleParameterType {
        name = Some "";
        position = 1;
        callee = Some (Access.create "callee");
        mismatch = {
          Error.actual = Type.Any;
          expected = Type.Any;
          due_to_invariance = false;
        };
      });
  assert_due_to_mismatch_with_any
    (Error.IncompatibleParameterType {
        name = Some "";
        position = 1;
        callee = Some (Access.create "callee");
        mismatch = {
          Error.actual = Type.string;
          expected = Type.Any;
          due_to_invariance = false;
        };
      });

  (* IncompatibleReturnType *)
  assert_due_to_mismatch_with_any
    (Error.IncompatibleReturnType {
        mismatch = {
          Error.actual = Type.Any;
          expected = Type.Any;
          due_to_invariance = false;
        };
        is_implicit = false;
      });
  assert_due_to_mismatch_with_any
    (Error.IncompatibleReturnType {
        mismatch = {
          Error.actual = Type.Any;
          expected = Type.string;
          due_to_invariance = false;
        };
        is_implicit = false;
      });
  assert_not_due_to_mismatch_with_any
    (Error.IncompatibleReturnType {
        mismatch = {
          Error.actual = Type.string;
          expected = Type.integer;
          due_to_invariance = false;
        };
        is_implicit = false;
      });

  (* IncompatibleVariableType *)
  assert_due_to_mismatch_with_any
    (Error.IncompatibleVariableType {
        name = [Expression.Access.Identifier "name"];
        mismatch = {
          Error.actual = Type.string;
          expected = Type.Any;
          due_to_invariance = false;
        };
        declare_location = Location.Instantiated.any;
      });

  (* InconsistentOverride *)
  assert_not_due_to_mismatch_with_any
    (InconsistentOverride {
        overridden_method = "foo";
        parent = Access.create (Type.show mock_parent);
        override = (StrengthenedPrecondition (NotFound "x"));
      });
  assert_not_due_to_mismatch_with_any
    (InconsistentOverride {
        overridden_method = "foo";
        parent = Access.create (Type.show mock_parent);
        override = (WeakenedPostcondition {
            actual = Type.Top;
            expected = Type.integer;
            due_to_invariance = false;
          });
      });
  assert_due_to_mismatch_with_any
    (InconsistentOverride {
        overridden_method = "foo";
        parent = Access.create (Type.show mock_parent);
        override = (WeakenedPostcondition {
            actual = Type.Any;
            expected = Type.integer;
            due_to_invariance = false;
          });
      });
  assert_not_due_to_mismatch_with_any
    (InconsistentOverride {
        overridden_method = "foo";
        parent = Access.create (Type.show mock_parent);
        override = (StrengthenedPrecondition (Found {
            actual = Type.none;
            expected = Type.integer;
            due_to_invariance = false;
          }));
      });
  assert_due_to_mismatch_with_any
    (InconsistentOverride {
        overridden_method = "foo";
        parent = Access.create (Type.show mock_parent);
        override = (StrengthenedPrecondition (Found {
            actual = Type.none;
            expected = Type.Any;
            due_to_invariance = false;
          }));
      });

  (* InvalidArgument *)
  assert_not_due_to_mismatch_with_any
    (InvalidArgument (
        Keyword {
          expression = !"name";
          annotation = Type.integer;
        }
      ));
  assert_not_due_to_mismatch_with_any
    (InvalidArgument (
        Variable {
          expression = !"name";
          annotation = Type.integer;
        }
      ));
  assert_due_to_mismatch_with_any
    (InvalidArgument (
        Keyword {
          expression = !"name";
          annotation = Type.Any;
        }
      ));
  assert_due_to_mismatch_with_any
    (InvalidArgument (
        Variable {
          expression = !"name";
          annotation = Type.Any;
        }
      ));

  (* NotCallable *)
  assert_due_to_mismatch_with_any (Error.NotCallable Type.Any);
  assert_not_due_to_mismatch_with_any (Error.NotCallable Type.Top);

  (* UndefinedAttribute *)
  assert_due_to_mismatch_with_any
    (Error.UndefinedAttribute {
        attribute = "foo";
        origin = Error.Class {
            annotation = Type.Any;
            class_attribute = false;
          };
      });

  assert_not_due_to_mismatch_with_any
    (Error.UndefinedAttribute {
        attribute = "foo";
        origin = Error.Module (Access.create "module");
      });

  (* Uninitialized Attribute *)
  assert_due_to_mismatch_with_any
    (Error.UninitializedAttribute {
        name = "";
        parent = mock_parent;
        mismatch = {
          Error.actual = Type.Any;
          expected = Type.Optional Type.integer;
          due_to_invariance = false;
        };
      });

  assert_not_due_to_mismatch_with_any
    (Error.UninitializedAttribute {
        name = "";
        parent = mock_parent;
        mismatch = {
          Error.actual = Type.string;
          expected = Type.Optional Type.string;
          due_to_invariance = false;
        };
      });

  (* Unpack *)
  assert_not_due_to_mismatch_with_any
    (Error.Unpack { expected_count = 2; unpack_problem = CountMismatch 3 });
  assert_not_due_to_mismatch_with_any
    (Error.Unpack { expected_count = 2; unpack_problem = UnacceptableType Type.integer });
  assert_due_to_mismatch_with_any
    (Error.Unpack { expected_count = 2; unpack_problem = UnacceptableType Type.Any });

  (* Missing X errors *)
  assert_not_due_to_mismatch_with_any
    (Error.MissingParameterAnnotation {
        name = (Access.create "");
        annotation = Some Type.Any;
        given_annotation = None;
        evidence_locations = [];
        thrown_at_source = true;
      });

  assert_not_due_to_mismatch_with_any
    (Error.MissingReturnAnnotation {
        name = (Access.create "$return_annotation");
        annotation = Some Type.Top;
        given_annotation = None;
        evidence_locations = [];
        thrown_at_source = true;
      });

  assert_not_due_to_mismatch_with_any
    (Error.MissingAttributeAnnotation {
        parent = mock_parent;
        missing_annotation = {
          Error.name = [Access.Identifier ""];
          annotation = Some Type.Any;
          given_annotation = None;
          evidence_locations = [];
          thrown_at_source = true;
        };
      })


let test_join _ =
  let assert_join left right expected =
    let environment =
      Environment.handler (Environment.Builder.create ())
    in
    let resolution = TypeCheck.resolution environment () in
    let result = Error.join ~resolution left right in
    assert_equal ~printer:Error.show ~cmp:Error.equal result expected
  in
  assert_join
    (error
       (Error.IncompatibleAttributeType {
           parent = mock_parent;
           incompatible_type = {
             Error.name = [Access.Identifier ""];
             mismatch = {
               Error.actual = Type.Top;
               expected = Type.Top;
               due_to_invariance = false;
             };
             declare_location = Location.Instantiated.any;
           };
         }))
    (error (Error.IncompatibleVariableType {
         Error.name = [Access.Identifier ""];
         mismatch = {
           Error.actual = Type.Top;
           expected = Type.Top;
           due_to_invariance = false;
         };
         declare_location = Location.Instantiated.any;
       }))
    (error Error.Top);
  assert_join
    (error
       (Error.IncompatibleParameterType {
           name = Some "";
           position = 1;
           callee = Some (Access.create "callee");
           mismatch = {
             Error.actual = Type.integer;
             expected = Type.string;
             due_to_invariance = false;
           };
         }))
    (error
       (Error.IncompatibleParameterType {
           name = Some "";
           position = 1;
           callee = Some (Access.create "callee");
           mismatch = {
             Error.actual = Type.float;
             expected = Type.string;
             due_to_invariance = false;
           };
         }))
    (error
       (Error.IncompatibleParameterType {
           name = Some "";
           position = 1;
           callee = Some (Access.create "callee");
           mismatch = {
             Error.actual = Type.float;
             expected = Type.string;
             due_to_invariance = false;
           };
         }));
  let create_mock_location path =
    {
      Location.path;
      start = { Location.line = 1; column = 1 };
      stop = { Location.line = 1; column = 1 };
    }
  in
  assert_join
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.integer;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.float;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.float;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.integer;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = None;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.integer;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = None;
           given_annotation = None;
           evidence_locations = [];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.float;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.float;
           given_annotation = None;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }));
  assert_join
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.float;
           given_annotation = Some Type.Any;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = true;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
           annotation = Some Type.integer;
           given_annotation = Some Type.Any;
           evidence_locations = [create_mock_location "derp.py"];
           thrown_at_source = false;
         }))
    (error
       (Error.MissingGlobalAnnotation {
           Error.name = [Access.Identifier ""];
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
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.integer;
         }))
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.float;
         }))
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.float;
         }));
  assert_join
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.float;
         }))
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.integer;
         }))
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.float;
         }));
  assert_join
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.float;
         }))
    (error (Error.Unpack { expected_count = 2; unpack_problem = Error.CountMismatch 3 }))
    (error Error.Top);
  assert_join
    (error
       (Error.Unpack {
           expected_count = 2;
           unpack_problem = Error.UnacceptableType Type.float;
         }))
    (error
       (Error.Unpack {
           expected_count = 3;
           unpack_problem = Error.UnacceptableType Type.float;
         }))
    (error Error.Top);

  assert_join
    (error (Error.UndefinedType (Type.Primitive "derp")))
    (error (Error.UndefinedType (Type.Primitive "derp")))
    (error (Error.UndefinedType (Type.Primitive "derp")));

  assert_join
    (error (Error.UndefinedType (Type.Primitive "derp")))
    (error (Error.UndefinedType (Type.Primitive "herp")))
    (error (Error.Top));

  assert_join
    (error (Error.AnalysisFailure (Type.Primitive "derp")))
    (error (Error.AnalysisFailure (Type.Primitive "derp")))
    (error (Error.AnalysisFailure (Type.Primitive "derp")));

  assert_join
    (error (Error.AnalysisFailure (Type.Primitive "derp")))
    (error (Error.AnalysisFailure (Type.Primitive "herp")))
    (error (Error.AnalysisFailure (Type.union [Type.Primitive "derp"; Type.Primitive "herp"])));

  assert_join
    (error (revealed_type "a" Type.integer))
    (error (revealed_type "a" Type.float))
    (error (revealed_type "a" Type.float));
  assert_join
    (error (revealed_type "a" Type.integer))
    (error (revealed_type "b" Type.float))
    (error (Error.Top))


let test_less_or_equal _ =
  let resolution =
    let environment = Environment.handler (Environment.Builder.create ()) in
    TypeCheck.resolution environment ()
  in
  assert_true
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.integer;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.integer;
            })));
  assert_true
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.integer;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.float
            })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.float;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.integer;
            })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 3;
              unpack_problem = Error.UnacceptableType Type.integer;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.integer;
            })));
  assert_true
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.CountMismatch 2;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.CountMismatch 2;
            })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.CountMismatch 2;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.CountMismatch 3;
            })));
  assert_false
    (Error.less_or_equal
       ~resolution
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.CountMismatch 2;
            }))
       (error
          (Error.Unpack {
              expected_count = 2;
              unpack_problem = Error.UnacceptableType Type.integer;
            })))


let test_filter _ =
  let open Error in
  let environment =
    Environment.handler (Environment.Builder.create ())
  in
  add_defaults_to_environment ~configuration environment;
  Service.Environment.populate
    ~configuration
    environment
    [
      parse {|
        class Foo: ...
        class MockChild(unittest.mock.Mock): ...
        class NonCallableChild(unittest.mock.NonCallableMock): ...
        class NonMockChild(Foo): ...
      |};
    ];
  let resolution = TypeCheck.resolution environment () in
  let assert_filtered ?(location = Location.Instantiated.any) ?(define = mock_define) kind =
    let errors = [error ~define ~location kind] in
    assert_equal
      []
      (filter ~configuration ~resolution errors)
  in
  let assert_unfiltered ?(location = Location.Instantiated.any) ?(define = mock_define) kind =
    let errors = [error ~define ~location kind] in
    assert_equal
      ~cmp:(List.equal ~equal)
      errors
      (filter ~configuration ~resolution errors)
  in
  (* Suppress stub errors. *)
  let stub = { Location.Instantiated.any with Location.path = "stub.pyi" } in
  assert_filtered ~location:stub (undefined_attribute (Type.Primitive "Foo"));
  assert_unfiltered
    ~location:Location.Instantiated.any
    (undefined_attribute (Type.Primitive "Foo"));

  (* Suppress mock errors. *)
  assert_filtered (incompatible_return_type (Type.Primitive "unittest.mock.Mock") Type.integer);
  assert_unfiltered (incompatible_return_type Type.integer (Type.Primitive "unittest.mock.Mock"));
  assert_filtered (undefined_attribute (Type.Primitive "MockChild"));
  assert_filtered (undefined_attribute (Type.Primitive "NonCallableChild"));
  assert_unfiltered (undefined_attribute (Type.Primitive "NonMockChild"));
  assert_filtered (undefined_attribute (Type.Optional (Type.Primitive "NonCallableChild")));
  assert_unfiltered (incompatible_return_type (Type.Optional Type.Bottom) Type.integer);
  assert_filtered (unexpected_keyword "foo" (Some "unittest.mock.call"));
  assert_unfiltered (unexpected_keyword "foo" None);

  (* Suppress return errors in unimplemented defines. *)
  assert_unfiltered (incompatible_return_type Type.integer Type.float);
  assert_filtered
    ~define:(define ~body:[+Statement.Pass;
                           +Statement.Return { Return.expression = None; is_implicit = false }] ())
    (incompatible_return_type Type.integer Type.float);

  (* Suppress errors due to importing builtins. *)
  let undefined_import import = UndefinedImport (Access.create import) in
  assert_filtered (undefined_import "builtins");
  assert_unfiltered (undefined_import "sys");

  let inconsistent_override name override =
    InconsistentOverride {
      overridden_method = name;
      parent = Access.create (Type.show mock_parent);
      override;
    }
  in
  (* Suppress parameter errors on override of dunder methods *)
  assert_unfiltered
    (inconsistent_override
       "foo"
       (StrengthenedPrecondition (NotFound "x")));
  assert_unfiltered
    (inconsistent_override
       "__foo__"
       (WeakenedPostcondition {
           actual = Type.Top;
           expected = Type.integer;
           due_to_invariance = false;
         }));
  assert_unfiltered
    (inconsistent_override
       "__foo__"
       (StrengthenedPrecondition (Found {
            actual = Type.none;
            expected = Type.integer;
            due_to_invariance = false;
          })));
  assert_filtered
    (inconsistent_override
       "__foo__"
       (StrengthenedPrecondition (NotFound "x")))


let test_suppress _ =
  let resolution = Test.resolution () in
  let assert_suppressed mode ?(define = mock_define) ?location kind =
    assert_equal
      true
      (Error.suppress ~mode ~resolution (error ~define ?location kind))
  in
  let assert_not_suppressed mode ?(define = mock_define) kind =
    assert_equal
      false
      (Error.suppress ~mode ~resolution (error ~define kind))
  in

  (* Test different modes. *)
  assert_suppressed Source.Infer (missing_return Type.Top);
  assert_suppressed Source.Infer (missing_return Type.Any);
  assert_not_suppressed Source.Infer (missing_return Type.integer);
  assert_suppressed Source.Infer (Error.UndefinedType Type.integer);
  assert_suppressed Source.Infer (Error.AnalysisFailure Type.integer);

  assert_not_suppressed Source.Strict (missing_return Type.Top);
  assert_suppressed Source.Strict (Error.IncompatibleAwaitableType Type.Top);
  assert_not_suppressed Source.Strict (missing_return Type.Any);
  assert_not_suppressed Source.Strict (Error.AnalysisFailure Type.integer);

  assert_suppressed Source.Default (missing_return Type.integer);
  assert_not_suppressed Source.Default (incompatible_return_type Type.integer Type.float);
  assert_suppressed Source.Default (incompatible_return_type Type.integer Type.Any);
  assert_not_suppressed Source.Default (revealed_type "a" Type.integer);
  assert_not_suppressed ~define:untyped_define Source.Default (revealed_type "a" Type.integer);
  assert_suppressed Source.Default (Error.UndefinedName (Access.create "reveal_type"));
  assert_not_suppressed Source.Default (Error.AnalysisFailure Type.integer);

  assert_suppressed
    Source.Default
    (Error.InvalidTypeParameters {
        annotation = Type.Primitive "dict";
        expected_number_of_parameters = 2;
        given_number_of_parameters = 0;
      });
  assert_not_suppressed
    Source.Default
    (Error.InvalidTypeParameters {
        annotation = Type.Primitive "dict";
        expected_number_of_parameters = 2;
        given_number_of_parameters = 1;
      });
  assert_not_suppressed
    Source.Strict
    (Error.InvalidTypeParameters {
        annotation = Type.Primitive "dict";
        expected_number_of_parameters = 2;
        given_number_of_parameters = 0;
      });

  let suppress_missing_return =
    Source.DefaultButDontCheck [Error.code (error (missing_return Type.Any))]
  in
  assert_suppressed suppress_missing_return (missing_return Type.integer);
  assert_suppressed suppress_missing_return (missing_return Type.Any);
  (* Defer to Default policy if not specifically suppressed *)
  assert_not_suppressed suppress_missing_return (incompatible_return_type Type.integer Type.float);
  assert_suppressed suppress_missing_return (Error.UndefinedName (Access.create "reveal_type"));

  (* Always suppress synthetic locations. *)
  assert_suppressed
    Source.Infer
    ~location:Location.Instantiated.synthetic
    (missing_return Type.integer);
  assert_suppressed
    Source.Declare
    ~location:Location.Instantiated.synthetic
    (missing_return Type.integer);
  assert_suppressed
    Source.Default
    ~location:Location.Instantiated.synthetic
    (missing_return Type.integer);
  assert_suppressed
    Source.Strict
    ~location:Location.Instantiated.synthetic
    (missing_return Type.integer)


let () =
  "error">:::[
    "due_to_analysis_limitations">::test_due_to_analysis_limitations;
    "due_to_mismatch_with_any">::test_due_to_mismatch_with_any;
    "join">::test_join;
    "less_or_equal">::test_less_or_equal;
    "filter">::test_filter;
    "suppress">::test_suppress;
  ]
  |> Test.run
