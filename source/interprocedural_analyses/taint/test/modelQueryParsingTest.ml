(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Target = Interprocedural.Target
module PyrePysaApi = Interprocedural.PyrePysaApi
open Taint
open ModelParseResult.ModelQuery

let get_stubs_and_definitions ~source_file_name ~project =
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let qualifier = Ast.Reference.create (String.chop_suffix_exn source_file_name ~suffix:".py") in
  let initial_callables =
    Interprocedural.FetchCallables.from_qualifier
      ~configuration:(Test.ScratchPyrePysaProject.configuration_of project)
      ~pyre_api
      ~qualifier
  in
  ( Interprocedural.FetchCallables.get_stubs initial_callables,
    Interprocedural.FetchCallables.get_definitions initial_callables )


let set_up_environment ?source ~context ~model_source ~validate () =
  let source =
    match source with
    | None -> ""
    | Some source -> source
  in
  let source_file_name = "test.py" in
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~requires_type_of_expressions:false
      [source_file_name, source]
  in
  let taint_configuration =
    let named name = { AnnotationParser.KindDefinition.name; kind = Named; location = None } in
    let sources = [named "Test"] in
    let sinks = [named "Test"] in
    let transforms = [TaintTransform.Named "TestTransform"] in
    TaintConfiguration.Heap.{ empty with sources; sinks; transforms }
  in
  let source = Test.trim_extra_indentation model_source in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in

  PyrePysaApi.ModelQueries.invalidate_cache pyre_api;
  let stubs, definitions = get_stubs_and_definitions ~source_file_name ~project in
  let ({ ModelParseResult.errors; _ } as parse_result) =
    ModelParser.parse
      ~pyre_api
      ~source
      ~taint_configuration
      ~source_sink_filter:(Some taint_configuration.source_sink_filter)
      ~definitions:(Some (Interprocedural.Target.HashSet.of_list definitions))
      ~stubs:
        (stubs
        |> Interprocedural.Target.HashsetSharedMemory.from_heap
        |> Interprocedural.Target.HashsetSharedMemory.read_only)
      ~python_version:(ModelParser.PythonVersion.create ())
      ()
  in
  if validate then
    assert_bool
      (Format.sprintf
         "Models have parsing errors: %s"
         (List.to_string errors ~f:ModelVerificationError.display))
      (List.is_empty errors);

  parse_result


let assert_queries ?source ~context ~model_source ~expect () =
  let { ModelParseResult.queries; _ } =
    set_up_environment ?source ~context ~model_source ~validate:true ()
  in
  assert_equal
    ~cmp:(List.equal ModelParseResult.ModelQuery.equal)
    ~printer:(List.to_string ~f:ModelParseResult.ModelQuery.show)
    expect
    queries


let assert_invalid_queries ?source ~context ~model_source ~expect () =
  let { ModelParseResult.errors; _ } =
    set_up_environment ?source ~context ~model_source ~validate:false ()
  in
  let error_message =
    if List.is_empty errors then
      "no failure"
    else if List.length errors = 1 then
      List.hd_exn errors |> ModelVerificationError.display
    else
      let error_strings = List.map errors ~f:ModelVerificationError.display in
      List.fold error_strings ~init:"Multiple errors:\n[" ~f:(fun sofar string ->
          sofar ^ "\n" ^ string)
      ^ "\n]"
  in
  assert_equal ~printer:Fn.id expect error_message


let test_query_parsing_find_functions context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = fully_qualified_name.matches("foo"),
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [FullyQualifiedNameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.equals("foo"),
     model = [Returns(TaintSource[Test])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Equals "foo")];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = fully_qualified_name.equals("test.foo"),
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [FullyQualifiedNameConstraint (Equals "test.foo")];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = [name.matches("foo"), name.matches("bar")],
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              NameConstraint (Matches (Re2.create_exn "foo"));
              NameConstraint (Matches (Re2.create_exn "bar"));
            ];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = fully_qualified_name.equals("test.foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [FullyQualifiedNameConstraint (Equals "test.foo")];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = return_annotation.fully_qualified.equals("int"),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where =
            [
              ReturnConstraint
                (AnnotationConstraint.FullyQualifiedConstraint (NameConstraint.Equals "int"));
            ];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = any_parameter.annotation.fully_qualified.equals("int"),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyParameterConstraint
                (AnnotationConstraint
                   (AnnotationConstraint.FullyQualifiedConstraint (NameConstraint.Equals "int")));
            ];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = AnyOf(
         any_parameter.annotation.fully_qualified.equals("int"),
         return_annotation.fully_qualified.equals("int"),
       ),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyOf
                [
                  AnyParameterConstraint
                    (AnnotationConstraint
                       (AnnotationConstraint.FullyQualifiedConstraint (NameConstraint.Equals "int")));
                  ReturnConstraint
                    (AnnotationConstraint.FullyQualifiedConstraint (NameConstraint.Equals "int"));
                ];
            ];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = AllOf(
         any_parameter.annotation.fully_qualified.equals("int"),
         return_annotation.fully_qualified.equals("int"),
       ),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where =
            [
              AllOf
                [
                  AnyParameterConstraint
                    (AnnotationConstraint
                       (AnnotationConstraint.FullyQualifiedConstraint (NameConstraint.Equals "int")));
                  ReturnConstraint
                    (AnnotationConstraint.FullyQualifiedConstraint (NameConstraint.Equals "int"));
                ];
            ];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = True,
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [Constant true];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = False,
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [Constant false];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  (* All parameters. *)
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [AllParameters(TaintSource[Test])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              AllParameters
                {
                  excludes = [];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [AllParameters(TaintSource[Test], exclude="self")]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              AllParameters
                {
                  excludes = ["self"];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [AllParameters(TaintSource[Test], exclude=["self", "other"])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              AllParameters
                {
                  excludes = ["self"; "other"];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_find_methods context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.name.equals("Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [ClassConstraint (NameConstraint (Equals "Foo"))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.fully_qualified_name.equals("test.Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [ClassConstraint (FullyQualifiedNameConstraint (Equals "test.Foo"))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.extends("Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (Extends { class_name = "Foo"; is_transitive = false; includes_self = true });
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.extends("Foo", is_transitive=False),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (Extends { class_name = "Foo"; is_transitive = false; includes_self = true });
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.extends("Foo", is_transitive=True),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (Extends { class_name = "Foo"; is_transitive = true; includes_self = true });
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.extends("Foo", includes_self=True),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (Extends { class_name = "Foo"; is_transitive = false; includes_self = true });
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.extends("Foo", includes_self=False),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (Extends { class_name = "Foo"; is_transitive = false; includes_self = false });
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.name.matches("Foo.*"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "Foo.*")))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.decorator(name.matches("foo.*")),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (DecoratorConstraint (NameConstraint (Matches (Re2.create_exn "foo.*"))));
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = cls.decorator(fully_qualified_callee.matches("foo.*")),
     model = [Returns([TaintSink[Test]])]
    )
  |}
    ~expect:"Unsupported `fully_qualified_callee` constraint within a class constraint"
    ();

  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(name.matches("foo")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [AnyDecoratorConstraint (NameConstraint (Matches (Re2.create_exn "foo")))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(fully_qualified_callee.matches("foo")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [AnyDecoratorConstraint (FullyQualifiedCallee (Matches (Re2.create_exn "foo")))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(fully_qualified_callee.matches("foo")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [AnyDecoratorConstraint (FullyQualifiedCallee (Matches (Re2.create_exn "foo")))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(name.matches("foo"), name.matches("bar")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyDecoratorConstraint
                (AllOf
                   [
                     NameConstraint (Matches (Re2.create_exn "foo"));
                     NameConstraint (Matches (Re2.create_exn "bar"));
                   ]);
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(arguments.contains(1)),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyDecoratorConstraint
                (ArgumentsConstraint
                   (Contains
                      [
                        {
                          Ast.Expression.Call.Argument.name = None;
                          value =
                            Ast.Node.create_with_default_location
                              (Ast.Expression.Expression.Constant
                                 (Ast.Expression.Constant.Integer 1));
                        };
                      ]));
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(arguments.contains(1), name.equals("foo")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyDecoratorConstraint
                (AllOf
                   [
                     ArgumentsConstraint
                       (Contains
                          [
                            {
                              Ast.Expression.Call.Argument.name = None;
                              value =
                                Ast.Node.create_with_default_location
                                  (Ast.Expression.Expression.Constant
                                     (Ast.Expression.Constant.Integer 1));
                            };
                          ]);
                     NameConstraint (Equals "foo");
                   ]);
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_model_parameters context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "get_POST_annotated_sources",
       find = "functions",
       where = [Decorator(name.matches("api_view"))],
       model = [
         Parameters(
           TaintSink[Test],
           where=[
              Not(AnyOf(
                name.matches("self"),
                name.matches("cls"),
                type_annotation.fully_qualified.matches("IGWSGIRequest"),
                type_annotation.fully_qualified.matches("HttpRequest"),
              ))
           ]
         )
       ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 19; column = 1 } };
          name = "get_POST_annotated_sources";
          logging_group_name = None;
          path = None;
          where = [AnyDecoratorConstraint (NameConstraint (Matches (Re2.create_exn "api_view")))];
          find = Function;
          models =
            [
              Parameter
                {
                  where =
                    [
                      Not
                        (AnyOf
                           [
                             ParameterConstraint.NameConstraint (Matches (Re2.create_exn "self"));
                             ParameterConstraint.NameConstraint (Matches (Re2.create_exn "cls"));
                             ParameterConstraint.AnnotationConstraint
                               (FullyQualifiedConstraint (Matches (Re2.create_exn "IGWSGIRequest")));
                             ParameterConstraint.AnnotationConstraint
                               (FullyQualifiedConstraint (Matches (Re2.create_exn "HttpRequest")));
                           ]);
                    ];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_expected_models context =
  let create_expected_model ?source ~model_source function_name =
    let { ModelParseResult.models; _ } =
      set_up_environment ?source ~context ~model_source ~validate:true ()
    in
    let model = Option.value_exn (Registry.get models (List.hd_exn (Registry.targets models))) in
    {
      ModelParseResult.ModelQuery.ExpectedModel.model;
      target = Target.create_function (Ast.Reference.create function_name);
      model_source;
    }
  in
  assert_queries
    ~source:"def food(): ..."
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test]),
     expected_models = [
       "def test.food() -> TaintSource[Test]: ..."
     ]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models =
            [
              create_expected_model
                ~source:"def food(): ..."
                ~model_source:"def test.food() -> TaintSource[Test]: ..."
                "test.food";
            ];
          unexpected_models = [];
        };
      ]
    ();

  assert_queries
    ~source:"def bar(): ..."
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test]),
     unexpected_models = [
       "def test.bar() -> TaintSource[Test]: ..."
     ]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models =
            [
              create_expected_model
                ~source:"def bar(): ..."
                ~model_source:"def test.bar() -> TaintSource[Test]: ..."
                "test.bar";
            ];
        };
      ]
    ();

  assert_queries
    ~source:"def food(): ...\n def foo(): ...\n def bar(): ..."
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test]),
     expected_models = [
       "def test.food() -> TaintSource[Test]: ...",
       "def test.foo() -> TaintSource[Test]: ..."
     ],
     unexpected_models = [
       "def test.bar() -> TaintSource[Test]: ..."
     ]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 14; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models =
            [
              create_expected_model
                ~source:"def food(): ...\n def foo(): ...\n def bar(): ..."
                ~model_source:"def test.food() -> TaintSource[Test]: ..."
                "test.food";
              create_expected_model
                ~source:"def food(): ...\n def foo(): ...\n def bar(): ..."
                ~model_source:"def test.foo() -> TaintSource[Test]: ..."
                "test.foo";
            ];
          unexpected_models =
            [
              create_expected_model
                ~source:"def food(): ...\n def foo(): ...\n def bar(): ..."
                ~model_source:"def test.bar() -> TaintSource[Test]: ..."
                "test.bar";
            ];
        };
      ]
    ();
  ()


let test_query_parsing_any_child context =
  assert_queries
    ~source:
      {|@d1
      class Foo:
        def __init__(self, a, b):
          ...
      @d1
      class Bar(Foo):
        def __init__(self, a, b):
          ...
      @d1
      class Baz:
        def __init__(self, a, b):
          ...|}
    ~context
    ~model_source:
      {|
    ModelQuery(
      name = "get_parent_of_d1_decorator_sources",
      find = "methods",
      where = [
        cls.any_child(
          AllOf(
            cls.decorator(
              name.matches("d1")
            ),
            AnyOf(
              Not(cls.name.matches("Foo")),
              cls.name.matches("Baz")
            )
          ),
          is_transitive=False
        ),
        name.matches("\.__init__$")
      ],
      model = [
        Parameters(TaintSource[Test], where=[
            Not(name.equals("self")),
            Not(name.equals("a"))
        ])
      ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 26; column = 1 } };
          name = "get_parent_of_d1_decorator_sources";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (ClassConstraint.AnyChildConstraint
                   {
                     class_constraint =
                       ClassConstraint.AllOf
                         [
                           ClassConstraint.DecoratorConstraint
                             (NameConstraint (Matches (Re2.create_exn "d1")));
                           ClassConstraint.AnyOf
                             [
                               ClassConstraint.Not
                                 (ClassConstraint.NameConstraint (Matches (Re2.create_exn "Foo")));
                               ClassConstraint.NameConstraint (Matches (Re2.create_exn "Baz"));
                             ];
                         ];
                     is_transitive = false;
                     includes_self = true;
                   });
              NameConstraint (Matches (Re2.create_exn "\\.__init__$"));
            ];
          find = Method;
          models =
            [
              Parameter
                {
                  where =
                    [
                      ParameterConstraint.Not (ParameterConstraint.NameConstraint (Equals "self"));
                      ParameterConstraint.Not (ParameterConstraint.NameConstraint (Equals "a"));
                    ];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~source:
      {|@d1
      class Foo:
        def __init__(self, a, b):
          ...
      @d1
      class Bar(Foo):
        def __init__(self, a, b):
          ...
      @d1
      class Baz:
        def __init__(self, a, b):
          ...|}
    ~context
    ~model_source:
      {|
    ModelQuery(
      name = "get_parent_of_d1_decorator_transitive_sources",
      find = "methods",
      where = [
        cls.any_child(
          AllOf(
            cls.decorator(
              name.matches("d1")
            ),
            AnyOf(
              Not(cls.name.matches("Foo")),
              cls.name.matches("Baz")
            )
          ),
          is_transitive=True,
          includes_self=False
        ),
        name.matches("\.__init__$")
      ],
      model = [
        Parameters(TaintSource[Test], where=[
            Not(name.equals("self")),
            Not(name.equals("a"))
        ])
      ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 27; column = 1 } };
          name = "get_parent_of_d1_decorator_transitive_sources";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (ClassConstraint.AnyChildConstraint
                   {
                     class_constraint =
                       ClassConstraint.AllOf
                         [
                           ClassConstraint.DecoratorConstraint
                             (NameConstraint (Matches (Re2.create_exn "d1")));
                           ClassConstraint.AnyOf
                             [
                               ClassConstraint.Not
                                 (ClassConstraint.NameConstraint (Matches (Re2.create_exn "Foo")));
                               ClassConstraint.NameConstraint (Matches (Re2.create_exn "Baz"));
                             ];
                         ];
                     is_transitive = true;
                     includes_self = false;
                   });
              NameConstraint (Matches (Re2.create_exn "\\.__init__$"));
            ];
          find = Method;
          models =
            [
              Parameter
                {
                  where =
                    [
                      ParameterConstraint.Not (ParameterConstraint.NameConstraint (Equals "self"));
                      ParameterConstraint.Not (ParameterConstraint.NameConstraint (Equals "a"));
                    ];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_any_parent context =
  assert_queries
    ~source:
      {|@d1
      class Foo:
        def __init__(self, a, b):
          ...
      @d1
      class Bar(Foo):
        def __init__(self, a, b):
          ...
      @d1
      class Baz:
        def __init__(self, a, b):
          ...|}
    ~context
    ~model_source:
      {|
    ModelQuery(
      name = "get_parent_of_d1_decorator_sources",
      find = "methods",
      where = [
        cls.any_parent(
          AllOf(
            cls.decorator(
              name.matches("d1")
            ),
            AnyOf(
              Not(cls.name.matches("Foo")),
              cls.name.matches("Baz")
            )
          ),
          is_transitive=False,
          includes_self=False
        ),
        name.matches("\.__init__$")
      ],
      model = [
        Parameters(TaintSource[Test], where=[
            Not(name.equals("self")),
            Not(name.equals("a"))
        ])
      ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 27; column = 1 } };
          name = "get_parent_of_d1_decorator_sources";
          logging_group_name = None;
          path = None;
          where =
            [
              ClassConstraint
                (ClassConstraint.AnyParentConstraint
                   {
                     class_constraint =
                       ClassConstraint.AllOf
                         [
                           ClassConstraint.DecoratorConstraint
                             (NameConstraint (Matches (Re2.create_exn "d1")));
                           ClassConstraint.AnyOf
                             [
                               ClassConstraint.Not
                                 (ClassConstraint.NameConstraint (Matches (Re2.create_exn "Foo")));
                               ClassConstraint.NameConstraint (Matches (Re2.create_exn "Baz"));
                             ];
                         ];
                     is_transitive = false;
                     includes_self = false;
                   });
              NameConstraint (Matches (Re2.create_exn "\\.__init__$"));
            ];
          find = Method;
          models =
            [
              Parameter
                {
                  where =
                    [
                      ParameterConstraint.Not (ParameterConstraint.NameConstraint (Equals "self"));
                      ParameterConstraint.Not (ParameterConstraint.NameConstraint (Equals "a"));
                    ];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_any_overriden_method context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
      name = "get_foo",
      find = "methods",
      where = [
        any_overriden_method(
          fully_qualified_name.equals("object.__add__")
        ),
      ],
      model = Returns(TaintSource[Test])
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 11; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [AnyOverridenMethod (FullyQualifiedNameConstraint (Equals "object.__add__"))];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
      name = "get_foo",
      find = "methods",
      where = [
        any_overriden_method(AllOf(
          cls.name.equals("MyClass"),
          name.equals("method_name")
        )),
      ],
      model = Returns(TaintSource[Test])
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 12; column = 1 } };
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyOverridenMethod
                (AllOf
                   [
                     ClassConstraint (NameConstraint (Equals "MyClass"));
                     NameConstraint (Equals "method_name");
                   ]);
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
      name = "get_foo",
      find = "functions",
      where = [
        any_overriden_method(True),
      ],
      model = Returns(TaintSource[Test])
    )
    |}
    ~expect:
      "`any_overriden_method` is not a valid constraint for model queries with find clause of kind \
       `functions`."
    ();
  ()


let test_query_parsing_find_globals context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "globals",
     where = name.matches("foo"),
     model = [GlobalModel([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Global;
          models =
            [
              Global
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "globals",
     where = name.matches("foo"),
     model = [GlobalModel([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Global;
          models =
            [
              Global
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_cache context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo",
     find = "methods",
     where = read_from_cache(kind="thrift", name="cache:name"),
     model = Returns([TaintSource[Test]])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo";
          logging_group_name = None;
          path = None;
          where = [ReadFromCache { kind = "thrift"; name = "cache:name" }];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo",
     find = "methods",
     where = AnyOf(
       read_from_cache(kind="thrift", name="one:name"),
       read_from_cache(kind="thrift", name="two:name"),
     ),
     model = Returns([TaintSource[Test]])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "foo";
          logging_group_name = None;
          path = None;
          where =
            [
              AnyOf
                [
                  ReadFromCache { kind = "thrift"; name = "one:name" };
                  ReadFromCache { kind = "thrift"; name = "two:name" };
                ];
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo",
     find = "methods",
     where = AllOf(
       read_from_cache(kind="thrift", name="one:name"),
       read_from_cache(kind="thrift", name="two:name"),
     ),
     model = Returns([TaintSource[Test]])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "foo";
          logging_group_name = None;
          path = None;
          where =
            [
              AllOf
                [
                  ReadFromCache { kind = "thrift"; name = "one:name" };
                  ReadFromCache { kind = "thrift"; name = "two:name" };
                ];
            ];
          find = Method;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo",
     find = "methods",
     where = name.matches("foo"),
     model = WriteToCache(kind="thrift", name=f"{class_name}:{method_name}")
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Method;
          models =
            [
              WriteToCache
                {
                  WriteToCache.kind = "thrift";
                  name =
                    [
                      FormatString.Substring.ClassName;
                      FormatString.Substring.Literal ":";
                      FormatString.Substring.MethodName;
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo",
     find = "functions",
     where = name.matches("foo"),
     model = WriteToCache(kind="thrift", name=f"{function_name}")
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              WriteToCache
                { WriteToCache.kind = "thrift"; name = [FormatString.Substring.FunctionName] };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo",
     find = "functions",
     where = name.matches("^foo(?P<x>.*)bar(?P<y>.*)$"),
     model = WriteToCache(kind="thrift", name=f"{capture(x)}:{capture(y)}")
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "^foo(?P<x>.*)bar(?P<y>.*)$"))];
          find = Function;
          models =
            [
              WriteToCache
                {
                  WriteToCache.kind = "thrift";
                  name =
                    [
                      FormatString.Substring.Capture "x";
                      FormatString.Substring.Literal ":";
                      FormatString.Substring.Capture "y";
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_logging_group context =
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     logging_group_name = "get_bar",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 8; column = 1 } };
          name = "get_foo";
          logging_group_name = Some "get_bar";
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              Return
                [
                  TaintAnnotation
                    (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                ];
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_captured_variables context =
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_captured_variables",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables
    )
  |}
    ~expect:"Unexpected model expression: `CapturedVariables`"
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "create_tito_for_captured_variables",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(TaintInTaintOut)
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "create_tito_for_captured_variables";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              CapturedVariables
                {
                  taint =
                    [TaintAnnotation (ModelParseResult.TaintAnnotation.from_tito Sinks.LocalReturn)];
                  generation_if_source = false;
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "taint_transform",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(TaintInTaintOut[Transform[TestTransform]])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "taint_transform";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              CapturedVariables
                {
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_tito
                           (Sinks.Transform
                              {
                                local =
                                  TaintTransforms.of_named_transforms
                                    [TaintTransform.Named "TestTransform"];
                                global = TaintTransforms.empty;
                                base = Sinks.LocalReturn;
                              }));
                    ];
                  generation_if_source = false;
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "create_tito_for_captured_variables",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(TaintSource[Test], generation=True)
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "create_tito_for_captured_variables";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Function;
          models =
            [
              CapturedVariables
                {
                  taint =
                    [
                      TaintAnnotation
                        (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
                    ];
                  generation_if_source = true;
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let test_query_parsing_cross_repository_taint_anchor context =
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], 1, 2])
    )
  |}
    ~expect:"Expected string for canonical name of CrossRepositoryTaintAnchor, got `1`"
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], "a", 2])
    )
  |}
    ~expect:"Expected string for canonical port of CrossRepositoryTaintAnchor, got `2`"
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[Foo, "a", "b"])
    )
  |}
    ~expect:"`Foo` is an invalid taint annotation: Failed to parse the given taint annotation."
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], f"{unknown_identifier}", "b"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: unknown identifier `unknown_identifier`|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], f"{method_name}", "b"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: invalid identifier `method_name` for find="functions"|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], f"{function_name}", f"{method_name}"])
    )
  |}
    ~expect:
      {|Invalid canonical port for CrossRepositoryTaintAnchor: invalid identifier `method_name` for find="functions"|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(CrossRepositoryTaintAnchor[TaintSink[Test], f"{parameter_name}", "formal(0)"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: identifier `parameter_name` is invalid in this context|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(CrossRepositoryTaintAnchor[TaintSink[Test], f"{parameter_position}", "formal(0)"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: identifier `parameter_position` is invalid in this context|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(CrossRepositoryTaintAnchor[TaintSink[Test], f"{parameter_name}", "formal(0)"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: identifier `parameter_name` is invalid in this context|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(CrossRepositoryTaintAnchor[TaintSink[Test], f"{parameter_position}", "formal(0)"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: identifier `parameter_position` is invalid in this context|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(CrossRepositoryTaintAnchor[TaintSink[Test], f"{function_name + 1}", "formal(0)"])
    )
  |}
    ~expect:
      {|Invalid canonical name for CrossRepositoryTaintAnchor: identifier `function_name` cannot be used in an integer expression|}
    ();
  assert_invalid_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "invalid_cross_repository_taint_anchor",
     find = "functions",
     where = name.matches("foo"),
     model = CapturedVariables(CrossRepositoryTaintAnchor[TaintSink[Test], f"{foo + 1}", "formal(0)"])
    )
  |}
    ~expect:{|Invalid canonical name for CrossRepositoryTaintAnchor: unknown identifier `foo`|}
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "cross_repository_taint_anchor",
     find = "methods",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], f"{class_name}:{method_name}", "formal(1)"])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "cross_repository_taint_anchor";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Method;
          models =
            [
              Parameter
                {
                  where = [];
                  taint =
                    [
                      CrossRepositoryTaintAnchor
                        {
                          annotation =
                            ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test");
                          canonical_name =
                            [
                              FormatString.Substring.ClassName;
                              FormatString.Substring.Literal ":";
                              FormatString.Substring.MethodName;
                            ];
                          canonical_port = [FormatString.Substring.Literal "formal(1)"];
                        };
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "cross_repository_taint_anchor",
     find = "methods",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[TaintSink[Test], f"{class_name}:{method_name}", f"formal(1).{parameter_name}"])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "cross_repository_taint_anchor";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Method;
          models =
            [
              Parameter
                {
                  where = [];
                  taint =
                    [
                      CrossRepositoryTaintAnchor
                        {
                          annotation =
                            ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test");
                          canonical_name =
                            [
                              FormatString.Substring.ClassName;
                              FormatString.Substring.Literal ":";
                              FormatString.Substring.MethodName;
                            ];
                          canonical_port =
                            [
                              FormatString.Substring.Literal "formal(1).";
                              FormatString.Substring.ParameterName;
                            ];
                        };
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "cross_repository_taint_anchor",
     find = "methods",
     where = name.matches("foo"),
     model = Parameters(CrossRepositoryTaintAnchor[
       TaintSink[Test],
       f"{class_name}:{method_name}",
       f"formal({parameter_position * 2 + 1})"
     ])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 11; column = 1 } };
          name = "cross_repository_taint_anchor";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          find = Method;
          models =
            [
              Parameter
                {
                  where = [];
                  taint =
                    [
                      CrossRepositoryTaintAnchor
                        {
                          annotation =
                            ModelParseResult.TaintAnnotation.from_sink (Sinks.NamedSink "Test");
                          canonical_name =
                            [
                              FormatString.Substring.ClassName;
                              FormatString.Substring.Literal ":";
                              FormatString.Substring.MethodName;
                            ];
                          canonical_port =
                            [
                              FormatString.Substring.Literal "formal(";
                              FormatString.Substring.Integer
                                (FormatString.IntegerExpression.Add
                                   {
                                     left =
                                       FormatString.IntegerExpression.Mul
                                         {
                                           left = FormatString.IntegerExpression.ParameterPosition;
                                           right = FormatString.IntegerExpression.Constant 2;
                                         };
                                     right = FormatString.IntegerExpression.Constant 1;
                                   });
                              FormatString.Substring.Literal ")";
                            ];
                        };
                    ];
                };
            ];
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ();
  ()


let () =
  "taint_model"
  >::: [
         "query_parsing_find_functions" >:: test_query_parsing_find_functions;
         "query_parsing_find_methods" >:: test_query_parsing_find_methods;
         "query_parsing_model_parameters" >:: test_query_parsing_model_parameters;
         "query_parsing_expected_models" >:: test_query_parsing_expected_models;
         "query_parsing_any_child" >:: test_query_parsing_any_child;
         "query_parsing_any_parent" >:: test_query_parsing_any_parent;
         "query_parsing_any_overriden_method" >:: test_query_parsing_any_overriden_method;
         "query_parsing_find_globals" >:: test_query_parsing_find_globals;
         "query_parsing_cache" >:: test_query_parsing_cache;
         "query_parsing_logging_group" >:: test_query_parsing_logging_group;
         "query_parsing_captured_variables" >:: test_query_parsing_captured_variables;
         "query_parsing_cross_repository_taint_anchor"
         >:: test_query_parsing_cross_repository_taint_anchor;
       ]
  |> Test.run
