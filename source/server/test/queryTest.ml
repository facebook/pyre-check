(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Ast
open Server
open ServerTest
open Analysis

let test_parse_query context =
  let assert_parses serialized query =
    let type_query_request_equal left right =
      let expression_equal left right = Expression.location_insensitive_compare left right = 0 in
      match left, right with
      | ( Query.Request.IsCompatibleWith (left_first, left_second),
          Query.Request.IsCompatibleWith (right_first, right_second) )
      | LessOrEqual (left_first, left_second), LessOrEqual (right_first, right_second) ->
          expression_equal left_first right_first && expression_equal left_second right_second
      | Superclasses left, Superclasses right ->
          List.for_all2_exn ~f:(fun left right -> Reference.equal left right) left right
      | Type left, Type right -> expression_equal left right
      | _ -> [%compare.equal: Query.Request.t] left right
    in
    match Query.parse_request serialized with
    | Result.Ok request ->
        assert_equal
          ~ctxt:context
          ~cmp:type_query_request_equal
          ~printer:(fun request -> Sexp.to_string_hum (Query.Request.sexp_of_t request))
          query
          request
    | Result.Error reason ->
        let message =
          Format.asprintf "Query parsing unexpectedly failed for '%s': %s" serialized reason
        in
        assert_failure message
  in
  let assert_fails_to_parse serialized =
    match Query.parse_request serialized with
    | Result.Error _ -> ()
    | Result.Ok request ->
        let message =
          Format.asprintf
            "Query parsing unexpectedly succeeded for '%s': %a"
            serialized
            Sexp.pp_hum
            (Query.Request.sexp_of_t request)
        in
        assert_failure message
  in
  let open Test in
  let ( ! ) name =
    let open Expression in
    Expression.Name (Name.Identifier name) |> Node.create_with_default_location
  in
  let open Query.Request in
  assert_parses "less_or_equal(int, bool)" (LessOrEqual (!"int", !"bool"));
  assert_parses "less_or_equal (int, bool)" (LessOrEqual (!"int", !"bool"));
  assert_parses "less_or_equal(  int, int)" (LessOrEqual (!"int", !"int"));
  assert_parses "Less_Or_Equal(  int, int)" (LessOrEqual (!"int", !"int"));
  assert_parses "is_compatible_with(int, bool)" (IsCompatibleWith (!"int", !"bool"));
  assert_parses "is_compatible_with (int, bool)" (IsCompatibleWith (!"int", !"bool"));
  assert_parses "is_compatible_with(  int, int)" (IsCompatibleWith (!"int", !"int"));
  assert_parses "Is_Compatible_With(  int, int)" (IsCompatibleWith (!"int", !"int"));
  assert_fails_to_parse "less_or_equal()";
  assert_fails_to_parse "less_or_equal(int, int, int)";
  assert_fails_to_parse "less_or_eq(int, bool)";
  assert_fails_to_parse "is_compatible_with()";
  assert_fails_to_parse "is_compatible_with(int, int, int)";
  assert_fails_to_parse "iscompatible(int, bool)";
  assert_fails_to_parse "IsCompatibleWith(int, bool)";
  assert_fails_to_parse "meet(int, int, int)";
  assert_fails_to_parse "meet(int)";
  assert_fails_to_parse "join(int)";
  assert_parses "superclasses(int)" (Superclasses [!&"int"]);
  assert_parses "superclasses(int, bool)" (Superclasses [!&"int"; !&"bool"]);
  assert_parses "type(C)" (Type !"C");
  assert_parses "type((C,B))" (Type (+Expression.Expression.Tuple [!"C"; !"B"]));
  assert_fails_to_parse "type(a.b, c.d)";
  assert_fails_to_parse "typecheck(1+2)";
  assert_parses "types(path='a.py')" (TypesInFiles ["a.py"]);
  assert_parses "types(path='a.pyi')" (TypesInFiles ["a.pyi"]);
  assert_parses "types('a.py')" (TypesInFiles ["a.py"]);
  assert_fails_to_parse "types(a.py:1:2)";
  assert_fails_to_parse "types(a.py)";
  assert_fails_to_parse "types('a.py', 1, 2)";
  assert_parses "attributes(C)" (Attributes !&"C");
  assert_fails_to_parse "attributes(C, D)";
  assert_parses "save_server_state('state')" (SaveServerState (PyrePath.create_absolute "state"));
  assert_fails_to_parse "save_server_state(state)";
  assert_parses "path_of_module(a.b.c)" (PathOfModule !&"a.b.c");
  assert_fails_to_parse "path_of_module('a.b.c')";
  assert_fails_to_parse "path_of_module(a.b, b.c)";
  assert_parses "validate_taint_models()" (ValidateTaintModels { path = None; verify_dsl = false });
  assert_parses
    "validate_taint_models('foo.py')"
    (ValidateTaintModels { path = Some "foo.py"; verify_dsl = false });
  assert_parses
    "validate_taint_models('foo.py', verify_dsl=True)"
    (ValidateTaintModels { path = Some "foo.py"; verify_dsl = true });
  assert_parses
    "validate_taint_models('foo.py', verify_dsl=False)"
    (ValidateTaintModels { path = Some "foo.py"; verify_dsl = false });
  assert_parses
    "validate_taint_models(verify_dsl=True)"
    (ValidateTaintModels { path = None; verify_dsl = true });
  assert_fails_to_parse "validate_taint_models(a)";
  assert_fails_to_parse "validate_taint_models('foo.py', 'foo.py')";
  assert_fails_to_parse "validate_taint_models(verify_dsl=False, verify_dsl=False)";
  assert_fails_to_parse "validate_taint_models('foo.py', verify_dsl='foo.py')";
  assert_parses "defines(a.b)" (Defines [Reference.create "a.b"]);
  assert_parses "batch()" (Batch []);
  assert_fails_to_parse "batch(batch())";
  assert_fails_to_parse "batch(defines(a.b), invalid(a))";
  assert_parses "batch(defines(a.b))" (Batch [Defines [Reference.create "a.b"]]);
  assert_parses
    "batch(defines(a.b), types(path='a.py'))"
    (Batch [Defines [Reference.create "a.b"]; TypesInFiles ["a.py"]]);
  assert_parses "inline_decorators(a.b.c)" (inline_decorators !&"a.b.c");
  assert_parses
    "inline_decorators(a.b.c, decorators_to_skip=[a.b.decorator1, a.b.decorator2])"
    (InlineDecorators
       {
         function_reference = !&"a.b.c";
         decorators_to_skip = [!&"a.b.decorator1"; !&"a.b.decorator2"];
       });
  assert_fails_to_parse "inline_decorators(a.b.c, a.b.d)";
  assert_fails_to_parse "inline_decorators(a.b.c, decorators_to_skip=a.b.decorator1)";
  assert_fails_to_parse "inline_decorators(a.b.c, decorators_to_skip=[a.b.decorator1, 1 + 1])";
  assert_parses
    "model_query('/a.py', 'model_query_name')"
    (ModelQuery { path = PyrePath.create_absolute "/a.py"; query_name = "model_query_name" });
  assert_parses
    "model_query(path='/a.py', query_name='model_query_name')"
    (ModelQuery { path = PyrePath.create_absolute "/a.py"; query_name = "model_query_name" });
  assert_fails_to_parse "model_query(/a.py, 'model_query_name')";
  assert_fails_to_parse "model_query('/a.py', model_query_name)";
  assert_parses "modules_of_path('/a.py')" (ModulesOfPath (PyrePath.create_absolute "/a.py"));
  assert_parses
    "location_of_definition(path='/foo.py', line=42, column=10)"
    (LocationOfDefinition
       { path = PyrePath.create_absolute "/foo.py"; position = Location.{ line = 42; column = 10 } });
  assert_fails_to_parse "location_of_definition(path='/foo.py', line=42)";
  assert_fails_to_parse "location_of_definition(path='/foo.py', column=10)";
  assert_fails_to_parse "location_of_definition(path=99, line=42, column=10)";
  assert_parses
    "find_references(path='/foo.py', line=42, column=10)"
    (FindReferences
       { path = PyrePath.create_absolute "/foo.py"; position = Location.{ line = 42; column = 10 } });
  assert_fails_to_parse "find_references(path='/foo.py', line=42)";
  assert_fails_to_parse "find_references(path='/foo.py', column=10)";
  assert_fails_to_parse "find_references(path=99, line=42, column=10)";
  assert_parses "expression_level_coverage(path='a.py')" (ExpressionLevelCoverage ["a.py"]);
  assert_parses "expression_level_coverage(path='a.pyi')" (ExpressionLevelCoverage ["a.pyi"]);
  assert_parses "expression_level_coverage('a.py')" (ExpressionLevelCoverage ["a.py"]);
  assert_parses
    "expression_level_coverage('a.py','b.py')"
    (ExpressionLevelCoverage ["a.py"; "b.py"]);
  assert_fails_to_parse "expression_level_coverage(a.py:1:2)";
  assert_fails_to_parse "expression_level_coverage(a.py)";
  assert_fails_to_parse "expression_level_coverage('a.py', 1, 2)";
  assert_parses
    "hover_info_for_position(path='/foo.py', line=42, column=10)"
    (HoverInfoForPosition
       { path = PyrePath.create_absolute "/foo.py"; position = Location.{ line = 42; column = 10 } });
  assert_fails_to_parse "hover_info_for_position(path='/foo.py', line=42)";
  assert_fails_to_parse "hover_info_for_position(path='/foo.py', column=10)";
  assert_fails_to_parse "hover_info_for_position(path=99, line=42, column=10)";
  assert_parses
    "global_leaks(path.to.my_function)"
    (GlobalLeaks { qualifiers = [!&"path.to.my_function"]; parse_errors = [] });
  assert_parses
    "global_leaks([path.to.my_function], my_valid.qualifier)"
    (GlobalLeaks
       {
         qualifiers = [!&"my_valid.qualifier"];
         parse_errors =
           ["Invalid qualifier provided, expected reference but got `[path.to.my_function]`"];
       });
  assert_parses
    "global_leaks(path.to.my_function, invalid.qualifier(), path.to.my_function2)"
    (GlobalLeaks
       {
         qualifiers = [!&"path.to.my_function"; !&"path.to.my_function2"];
         parse_errors =
           ["Invalid qualifier provided, expected reference but got `invalid.qualifier()`"];
       });
  assert_parses "global_leaks()" (GlobalLeaks { qualifiers = []; parse_errors = [] });
  ()


let assert_queries_with_local_root
    ?custom_source_root
    ?build_system_initializer
    ?no_validation_on_class_lookup_failure
    ~context
    ~sources
    queries_and_responses
  =
  let test_handle_query client =
    let handle_one_query (query, build_expected_response) =
      let open Lwt.Infix in
      Client.send_request client (Request.Query query)
      >>= fun actual_response ->
      let expected_response =
        Client.get_server_properties client
        (* NOTE: Relativizing against `local_root` in query response is discouraged. We should
           migrate away from it at some point. *)
        |> fun { ServerProperties.configuration = { Configuration.Analysis.local_root; _ }; _ } ->
        build_expected_response local_root
      in
      assert_equal
        ~ctxt:context
        ~cmp:String.equal
        ~printer:(fun x -> Yojson.Safe.pretty_to_string (Yojson.Safe.from_string x))
        expected_response
        actual_response;
      Lwt.return_unit
    in
    Lwt_list.iter_s handle_one_query queries_and_responses
  in
  ScratchProject.setup
    ?custom_source_root
    ?build_system_initializer
    ?no_validation_on_class_lookup_failure
    ~context
    ~include_helper_builtins:false
    sources
  |> ScratchProject.test_server_with ~f:test_handle_query


module QueryTestTypes = struct
  open Query.Response

  let assert_type_query_response_with_local_root
      ?custom_source_root
      ?(handle = "test.py")
      ?no_validation_on_class_lookup_failure
      ~source
      ~query
      ~context
      build_expected_response
    =
    let build_expected_response local_root =
      Response.Query (build_expected_response local_root)
      |> Response.to_yojson
      |> Yojson.Safe.to_string
    in
    assert_queries_with_local_root
      ?custom_source_root
      ?no_validation_on_class_lookup_failure
      ~context
      ~sources:[handle, source]
      [query, build_expected_response]


  let parse_annotation serialized =
    serialized
    |> (fun literal ->
         Expression.Expression.Constant
           (Expression.Constant.String (Expression.StringLiteral.create literal)))
    |> Node.create_with_default_location
    |> Type.create ~aliases:Type.empty_aliases


  let create_location start_line start_column stop_line stop_column =
    let start = { Location.line = start_line; column = start_column } in
    let stop = { Location.line = stop_line; column = stop_column } in
    { Location.start; stop }


  let create_types_at_locations types =
    let convert (start_line, start_column, end_line, end_column, annotation) =
      { Base.location = create_location start_line start_column end_line end_column; annotation }
    in
    List.map ~f:convert types
end

let test_handle_query_basic context =
  let open Query.Response in
  let assert_type_query_response_with_local_root =
    QueryTestTypes.assert_type_query_response_with_local_root ~context
  in
  let assert_type_query_response ?custom_source_root ?handle ~source ~query response =
    assert_type_query_response_with_local_root ?custom_source_root ?handle ~source ~query (fun _ ->
        response)
  in
  let assert_compatibility_response ~source ~query ~actual ~expected result =
    assert_type_query_response
      ~source
      ~query
      (Single (Base.Compatibility { actual; expected; result }))
  in
  let open Lwt.Infix in
  let open Test in
  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, str)"
    (Single (Base.Boolean false))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
        A = int
      |}
    ~query:"less_or_equal(int, test.A)"
    (Single (Base.Boolean true))
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~query:"less_or_equal(int, Unknown)"
    (Error "Type `Unknown` was not found in the type order.")
  >>= fun () ->
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"less_or_equal(list[test.C], list[int])"
    (Single (Base.Boolean false))
  >>= fun () ->
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"superclasses(test.C)"
    (Single
       (Base.Superclasses
          [
            {
              Base.class_name = !&"test.C";
              superclasses =
                [
                  !&"complex";
                  !&"float";
                  !&"int";
                  !&"numbers.Complex";
                  !&"numbers.Integral";
                  !&"numbers.Number";
                  !&"numbers.Rational";
                  !&"numbers.Real";
                  !&"object";
                ];
            };
          ]))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
    class C: pass
    class D(C): pass
  |}
    ~query:"superclasses(test.C, test.D)"
    (Single
       (Base.Superclasses
          [
            { Base.class_name = !&"test.C"; superclasses = [!&"object"] };
            { Base.class_name = !&"test.D"; superclasses = [!&"object"; !&"test.C"] };
          ]))
  >>= fun () ->
  assert_type_query_response ~source:"" ~query:"batch()" (Batch [])
  >>= fun () ->
  assert_type_query_response
    ~source:"class C(int): ..."
    ~query:"batch(less_or_equal(int, str), less_or_equal(int, int))"
    (Batch [Single (Base.Boolean false); Single (Base.Boolean true)])
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~query:"batch(less_or_equal(int, str), less_or_equal(int, Unknown))"
    (Batch [Single (Base.Boolean false); Error "Type `Unknown` was not found in the type order."])
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~handle:"Foo.java"
    ~query:"expression_level_coverage(path='Foo.java')"
    (Single
       (Base.ExpressionLevelCoverageResponse
          [
            ErrorAtPath
              {
                path = "Foo.java";
                error = "Not able to get lookups in: `Foo.java` (file not found)";
              };
          ]))
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~handle:"foo.pyi"
    ~query:"expression_level_coverage(path='foo.pyi')"
    (Single
       (Base.ExpressionLevelCoverageResponse
          [CoverageAtPath { Base.path = "foo.pyi"; total_expressions = 0; coverage_gaps = [] }]))
  >>= fun () ->
  assert_compatibility_response
    ~source:""
    ~query:"is_compatible_with(int, str)"
    ~actual:Type.integer
    ~expected:Type.string
    false
  >>= fun () ->
  assert_compatibility_response
    ~source:{|
        A = int
      |}
    ~query:"is_compatible_with(int, test.A)"
    ~actual:Type.integer
    ~expected:Type.integer
    true
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~query:"is_compatible_with(int, Unknown)"
    (Error "Type `Unknown` was not found in the type order.")
  >>= fun () ->
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(int, $unknown)"
    ~actual:Type.integer
    ~expected:Type.Top
    true
  >>= fun () ->
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(typing.List[int], typing.List[unknown])"
    ~actual:(Type.list Type.integer)
    ~expected:(Type.list Type.Top)
    true
  >>= fun () ->
  assert_compatibility_response
    ~source:"class unknown: ..."
    ~query:"is_compatible_with(int, typing.List[unknown])"
    ~actual:Type.integer
    ~expected:(Type.list Type.Top)
    false
  >>= fun () ->
  assert_compatibility_response
    ~source:""
    ~query:"is_compatible_with(int, typing.Coroutine[typing.Any, typing.Any, int])"
    ~actual:Type.integer
    ~expected:Type.integer
    true
  >>= fun () ->
  assert_compatibility_response
    ~source:""
    ~query:"is_compatible_with(int, typing.Coroutine[typing.Any, typing.Any, str])"
    ~actual:Type.integer
    ~expected:Type.string
    false
  >>= fun () ->
  assert_compatibility_response
    ~source:"A = int"
    ~query:"is_compatible_with(test.A, typing.Coroutine[typing.Any, typing.Any, test.A])"
    ~actual:Type.integer
    ~expected:Type.integer
    true
  >>= fun () ->
  assert_compatibility_response
    ~source:{|
         class A: ...
         class B(A): ...
      |}
    ~query:"is_compatible_with(test.B, typing.Coroutine[typing.Any, typing.Any, test.A])"
    ~actual:(Type.Primitive "test.B")
    ~expected:(Type.Primitive "test.A")
    true
  >>= fun () ->
  assert_compatibility_response
    ~source:{|
         class A: ...
         class B(A): ...
      |}
    ~query:
      ("is_compatible_with(typing.Type[test.B],"
      ^ "typing.Coroutine[typing.Any, typing.Any, typing.Type[test.A]])")
    ~actual:(Type.meta (Type.Primitive "test.B"))
    ~expected:(Type.meta (Type.Primitive "test.A"))
    true
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~query:"superclasses(Unknown)"
    (Single (Base.Superclasses []))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~handle:"test.py"
    ~source:"a = 2"
    ~query:"path_of_module(test)"
    (fun local_root ->
      Single
        (Base.FoundPath
           (PyrePath.create_relative ~root:local_root ~relative:"test.py" |> PyrePath.absolute)))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~handle:"test.pyi"
    ~source:"a = 2"
    ~query:"path_of_module(test)"
    (fun local_root ->
      Single
        (Base.FoundPath
           (PyrePath.create_relative ~root:local_root ~relative:"test.pyi" |> PyrePath.absolute)))
  >>= fun () ->
  assert_type_query_response
    ~source:"a = 2"
    ~query:"path_of_module(notexist)"
    (Error "No path found for module `notexist`")
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~handle:"Foo.java"
    ~query:"types(path='Foo.java')"
    (Error "Not able to get lookups in: `Foo.java` (file not found)")
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~query:"types(path='non_existent.py')"
    (Error "Not able to get lookups in: `non_existent.py` (file not found)")
  >>= fun () ->
  let temporary_directory = OUnit2.bracket_tmpdir context in
  assert_type_query_response
    ~source:""
    ~handle:(Format.sprintf "%s" temporary_directory)
    ~query:(Format.sprintf "types(path='%s')" temporary_directory)
    (Error (Format.sprintf "Not able to get lookups in: `%s` (file not found)" temporary_directory))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
      class C:
        x = 1
        y = ""
        def foo() -> int: ...
    |}
    ~query:"attributes(test.C)"
    (Single
       (Base.FoundAttributes
          [
            {
              Base.name = "foo";
              annotation =
                Type.parametric
                  "BoundMethod"
                  [
                    Single
                      (Type.Callable
                         {
                           Type.Callable.kind = Type.Callable.Named !&"test.C.foo";
                           implementation =
                             {
                               Type.Callable.annotation = Type.integer;
                               parameters = Type.Callable.Defined [];
                             };
                           overloads = [];
                         });
                    Single (Primitive "test.C");
                  ];
              kind = Base.Regular;
              final = false;
            };
            { Base.name = "x"; annotation = Type.integer; kind = Base.Regular; final = false };
            { Base.name = "y"; annotation = Type.string; kind = Base.Regular; final = false };
          ]))
  >>= fun () ->
  assert_type_query_response
    ~source:
      {|
      class C:
        @property
        def foo(self) -> int:
          return 0
    |}
    ~query:"attributes(test.C)"
    (Single
       (Base.FoundAttributes
          [{ Base.name = "foo"; annotation = Type.integer; kind = Base.Property; final = false }]))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
      foo: str = "bar"
    |}
    ~query:"type(test.foo)"
    (Single (Base.Type Type.string))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
      foo = 7
    |}
    ~query:"type(test.foo)"
    (Single (Base.Type Type.integer))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
    |}
    ~query:"type(8)"
    (Single (Base.Type (Type.literal_integer 8)))
  >>= fun () ->
  assert_type_query_response
    ~source:{|
      def foo(a: str) -> str:
        return a
      bar: str = "baz"
    |}
    ~query:"type(test.foo(test.bar))"
    (Single (Base.Type Type.string))
  >>= fun () ->
  (* TODO: Return some sort of error *)
  assert_type_query_response
    ~source:{|
      def foo(a: str) -> str:
        return a
      bar: int = 7
    |}
    ~query:"type(test.foo(test.bar))"
    (Single (Base.Type Type.string))
  >>= fun () ->
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let handle = "my_test_file.py" in
  assert_type_query_response
    ~custom_source_root
    ~handle
    ~source:""
    ~query:
      (Format.sprintf
         "modules_of_path('%s')"
         (PyrePath.append custom_source_root ~element:handle |> PyrePath.absolute))
    (Single (Base.FoundModules [Reference.create "my_test_file"]))
  >>= fun () ->
  assert_type_query_response
    ~source:""
    ~query:"modules_of_path('/non_existent_file.py')"
    (Single (Base.FoundModules []))
  >>= fun () ->
  let temporary_directory = OUnit2.bracket_tmpdir context in
  assert_type_query_response
    ~source:""
    ~query:(Format.sprintf "save_server_state('%s/state')" temporary_directory)
    (Single (Base.Success "Saved state."))
  >>= fun () ->
  assert_equal `Yes (Sys_unix.is_file (temporary_directory ^/ "state"));
  Lwt.return_unit


let test_handle_types_query context =
  let open Query.Response in
  let open Lwt.Infix in
  let open Test in
  let assert_type_query_response_with_local_root =
    QueryTestTypes.assert_type_query_response_with_local_root ~context
  in
  assert_type_query_response_with_local_root
    ~source:{|
      def foo(x: int = 10, y: str = "bar") -> None:
        a = 42
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.none;
                             parameters =
                               Type.Callable.Defined
                                 [
                                   Named
                                     {
                                       name = "$parameter$x";
                                       annotation = Type.integer;
                                       default = true;
                                     };
                                   Named
                                     {
                                       name = "$parameter$y";
                                       annotation = Type.string;
                                       default = true;
                                     };
                                 ];
                           };
                         overloads = [];
                       } );
                   2, 8, 2, 9, Type.integer;
                   2, 11, 2, 14, Type.meta Type.integer;
                   2, 17, 2, 19, Type.literal_integer 10;
                   2, 21, 2, 22, Type.string;
                   2, 24, 2, 27, Type.meta Type.string;
                   2, 30, 2, 35, Type.literal_string "bar";
                   2, 40, 2, 44, Type.meta Type.none;
                   3, 2, 3, 3, Type.literal_integer 42;
                   3, 6, 3, 8, Type.literal_integer 42;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
       def foo(x: int, y: str) -> str:
        x = 4
        y = 5
        return x
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.string;
                             parameters =
                               Type.Callable.Defined
                                 [
                                   Named
                                     {
                                       name = "$parameter$x";
                                       annotation = Type.integer;
                                       default = false;
                                     };
                                   Named
                                     {
                                       name = "$parameter$y";
                                       annotation = Type.string;
                                       default = false;
                                     };
                                 ];
                           };
                         overloads = [];
                       } );
                   2, 8, 2, 9, Type.integer;
                   2, 11, 2, 14, Type.meta Type.integer;
                   2, 16, 2, 17, Type.string;
                   2, 19, 2, 22, Type.meta Type.string;
                   2, 27, 2, 30, Type.meta Type.string;
                   3, 1, 3, 2, Type.integer;
                   3, 5, 3, 6, Type.literal_integer 4;
                   4, 1, 4, 2, Type.string;
                   4, 5, 4, 6, Type.literal_integer 5;
                   5, 8, 5, 9, Type.integer;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
        x = 4
        y = 3
     |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   2, 0, 2, 1, Type.integer;
                   2, 4, 2, 5, Type.literal_integer 4;
                   3, 0, 3, 1, Type.integer;
                   3, 4, 3, 5, Type.literal_integer 3;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
              def identity(a: int) -> int: ...
            |}
    ~handle:"test_stub.pyi"
    ~query:"types(path='test_stub.pyi')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test_stub.pyi";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     12,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test_stub.identity";
                         implementation =
                           {
                             Type.Callable.annotation = Type.integer;
                             parameters =
                               Type.Callable.Defined
                                 [
                                   Named
                                     {
                                       name = "$parameter$a";
                                       annotation = Type.integer;
                                       default = false;
                                     };
                                 ];
                           };
                         overloads = [];
                       } );
                   2, 16, 2, 19, Type.meta Type.integer;
                   2, 24, 2, 27, Type.meta Type.integer;
                   2, 29, 2, 32, Type.Any;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
      def foo():
        if True:
         x = 1
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.Any;
                             parameters = Type.Callable.Defined [];
                           };
                         overloads = [];
                       } );
                   (* TODO (T68817342): Should be `Literal (Boolean true)` *)
                   3, 5, 3, 9, Type.Literal (Boolean false);
                   4, 3, 4, 4, Type.literal_integer 1;
                   4, 7, 4, 8, Type.literal_integer 1;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       def foo():
         for x in [1, 2]:
          y = 1
     |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.Any;
                             parameters = Type.Callable.Defined [];
                           };
                         overloads = [];
                       } );
                   3, 6, 3, 7, Type.integer;
                   (* TODO(T124426942): We are mistakenly overwriting the type of the iterator
                      variable (list[int]) with the type of `<variable>.__iter__().__next__()`
                      (int), because they have the same location. *)
                   3, 11, 3, 17, Type.integer;
                   3, 12, 3, 13, Type.literal_integer 1;
                   3, 15, 3, 16, Type.literal_integer 2;
                   4, 3, 4, 4, Type.literal_integer 1;
                   4, 7, 4, 8, Type.literal_integer 1;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
        def foo() -> None:
          try:
            x = 1
          except Exception:
            y = 2
      |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.none;
                             parameters = Type.Callable.Defined [];
                           };
                         overloads = [];
                       } );
                   2, 13, 2, 17, Type.meta Type.none;
                   4, 4, 4, 5, Type.literal_integer 1;
                   4, 8, 4, 9, Type.literal_integer 1;
                   5, 9, 5, 18, Type.parametric "type" [Single (Type.Primitive "Exception")];
                   6, 4, 6, 5, Type.literal_integer 2;
                   6, 8, 6, 9, Type.literal_integer 2;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       with open() as x:
        y = 2
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   2, 5, 2, 11, Type.Any;
                   2, 15, 2, 16, Type.Any;
                   3, 1, 3, 2, Type.integer;
                   3, 5, 3, 6, Type.literal_integer 2;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
      while x is True:
        y = 1
   |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   2, 6, 2, 15, Type.bool;
                   2, 11, 2, 15, Type.Literal (Boolean true);
                   3, 2, 3, 3, Type.literal_integer 1;
                   3, 6, 3, 7, Type.literal_integer 1;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
       def foo(x: int) -> str:
         def bar(y: int) -> str:
           return y
         return x
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.string;
                             parameters =
                               Type.Callable.Defined
                                 [
                                   Named
                                     {
                                       name = "$parameter$x";
                                       annotation = Type.integer;
                                       default = false;
                                     };
                                 ];
                           };
                         overloads = [];
                       } );
                   2, 8, 2, 9, Type.integer;
                   2, 11, 2, 14, Type.meta Type.integer;
                   2, 19, 2, 22, Type.meta Type.string;
                   3, 10, 3, 11, Type.integer;
                   3, 13, 3, 16, Type.meta Type.integer;
                   3, 21, 3, 24, Type.meta Type.string;
                   4, 11, 4, 12, Type.integer;
                   5, 9, 5, 10, Type.integer;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       def foo(x: typing.List[int]) -> None:
        pass
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   ( 2,
                     4,
                     2,
                     7,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.none;
                             parameters =
                               Type.Callable.Defined
                                 [
                                   Named
                                     {
                                       name = "$parameter$x";
                                       annotation = Type.list Type.integer;
                                       default = false;
                                     };
                                 ];
                           };
                         overloads = [];
                       } );
                   2, 8, 2, 9, Type.list Type.integer;
                   2, 11, 2, 27, Type.meta (Type.list Type.integer);
                   2, 32, 2, 36, Type.meta Type.none;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       class Foo:
         x = 1
     |}
    ~query:"types('test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   {
                     Base.location = QueryTestTypes.create_location 2 6 2 9;
                     annotation = QueryTestTypes.parse_annotation "typing.Type[test.Foo]";
                   };
                   {
                     Base.location = QueryTestTypes.create_location 3 2 3 3;
                     annotation = Type.integer;
                   };
                   {
                     Base.location = QueryTestTypes.create_location 3 6 3 7;
                     annotation = Type.literal_integer 1;
                   };
                 ];
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
        # foo.py
        from other_module import Bar

        my_bar: Bar

        def my_foo(bar: Bar) -> None:
          x = bar
      |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   5, 0, 5, 6, Type.Any;
                   5, 8, 5, 11, Type.meta Type.Top;
                   ( 7,
                     4,
                     7,
                     10,
                     Type.Callable
                       {
                         Type.Callable.kind = Type.Callable.Named !&"test.my_foo";
                         implementation =
                           {
                             Type.Callable.annotation = Type.none;
                             parameters =
                               Type.Callable.Defined
                                 [Named { name = "bar"; annotation = Type.Top; default = false }];
                           };
                         overloads = [];
                       } );
                   7, 16, 7, 19, Type.meta Type.Top;
                   7, 24, 7, 28, Type.meta Type.none;
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
      # foo.py
      from other_module import Bar

      class Foo:
          foo_attribute: Bar

      f = Foo().foo_attribute
    |}
    ~query:"types(path='test.py')"
    (fun _ ->
      Single
        (Base.TypesByPath
           [
             {
               Base.path = "test.py";
               types =
                 [
                   5, 6, 5, 9, Type.meta (Type.Primitive "test.Foo");
                   6, 4, 6, 17, Type.Any;
                   6, 19, 6, 22, Type.meta Type.Top;
                   8, 4, 8, 7, Type.meta (Type.Primitive "test.Foo");
                   8, 4, 8, 9, Type.Primitive "test.Foo";
                 ]
                 |> QueryTestTypes.create_types_at_locations;
             };
           ]))


let test_handle_references_used_by_file_query context =
  let open Query.Response in
  let open Lwt.Infix in
  let open Test in
  let assert_type_query_response_with_local_root =
    QueryTestTypes.assert_type_query_response_with_local_root ~context
  in
  assert_type_query_response_with_local_root
    ~source:{|
      def foo(x: int = 10, y: str = "bar") -> None:
        a = 42
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.none;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "$parameter$x";
                                     annotation = Type.integer;
                                     default = true;
                                   };
                                 Named
                                   {
                                     name = "$parameter$y";
                                     annotation = Type.string;
                                     default = true;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 2, 8, 2, 9, Type.integer;
                 2, 11, 2, 14, Type.meta Type.integer;
                 2, 17, 2, 19, Type.literal_integer 10;
                 2, 21, 2, 22, Type.string;
                 2, 24, 2, 27, Type.meta Type.string;
                 2, 30, 2, 35, Type.literal_string "bar";
                 2, 40, 2, 44, Type.meta Type.none;
                 3, 2, 3, 3, Type.literal_integer 42;
                 3, 6, 3, 8, Type.literal_integer 42;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
       def foo(x: int, y: str) -> str:
        x = 4
        y = 5
        return x
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.string;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "$parameter$x";
                                     annotation = Type.integer;
                                     default = false;
                                   };
                                 Named
                                   {
                                     name = "$parameter$y";
                                     annotation = Type.string;
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 2, 8, 2, 9, Type.integer;
                 2, 11, 2, 14, Type.meta Type.integer;
                 2, 16, 2, 17, Type.string;
                 2, 19, 2, 22, Type.meta Type.string;
                 2, 27, 2, 30, Type.meta Type.string;
                 3, 1, 3, 2, Type.integer;
                 3, 5, 3, 6, Type.literal_integer 4;
                 4, 1, 4, 2, Type.string;
                 4, 5, 4, 6, Type.literal_integer 5;
                 5, 8, 5, 9, Type.integer;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
        x = 4
        y = 3
     |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 2, 0, 2, 1, Type.integer;
                 2, 4, 2, 5, Type.literal_integer 4;
                 3, 0, 3, 1, Type.integer;
                 3, 4, 3, 5, Type.literal_integer 3;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
              def identity(a: int) -> int: ...
            |}
    ~handle:"test_stub.pyi"
    ~query:"references_used_by_file(path='test_stub.pyi')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test_stub.pyi";
             types =
               [
                 ( 2,
                   4,
                   2,
                   12,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test_stub.identity";
                       implementation =
                         {
                           Type.Callable.annotation = Type.integer;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "$parameter$a";
                                     annotation = Type.integer;
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 2, 16, 2, 19, Type.meta Type.integer;
                 2, 24, 2, 27, Type.meta Type.integer;
                 2, 29, 2, 32, Type.Any;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
      def foo():
        if True:
         x = 1
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.Any;
                           parameters = Type.Callable.Defined [];
                         };
                       overloads = [];
                     } );
                 (* TODO (T68817342): Should be `Literal (Boolean true)` *)
                 3, 5, 3, 9, Type.Literal (Boolean false);
                 4, 3, 4, 4, Type.literal_integer 1;
                 4, 7, 4, 8, Type.literal_integer 1;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       def foo():
         for x in [1, 2]:
          y = 1
     |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.Any;
                           parameters = Type.Callable.Defined [];
                         };
                       overloads = [];
                     } );
                 3, 6, 3, 7, Type.integer;
                 (* TODO(T124426942): We are mistakenly overwriting the type of the iterator
                    variable (list[int]) with the type of `<variable>.__iter__().__next__()` (int),
                    because they have the same location. *)
                 3, 11, 3, 17, Type.integer;
                 3, 12, 3, 13, Type.literal_integer 1;
                 3, 15, 3, 16, Type.literal_integer 2;
                 4, 3, 4, 4, Type.literal_integer 1;
                 4, 7, 4, 8, Type.literal_integer 1;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
        def foo() -> None:
          try:
            x = 1
          except Exception:
            y = 2
      |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.none;
                           parameters = Type.Callable.Defined [];
                         };
                       overloads = [];
                     } );
                 2, 13, 2, 17, Type.meta Type.none;
                 4, 4, 4, 5, Type.literal_integer 1;
                 4, 8, 4, 9, Type.literal_integer 1;
                 5, 9, 5, 18, Type.parametric "type" [Single (Type.Primitive "Exception")];
                 6, 4, 6, 5, Type.literal_integer 2;
                 6, 8, 6, 9, Type.literal_integer 2;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       with open() as x:
        y = 2
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 2, 5, 2, 11, Type.Any;
                 2, 15, 2, 16, Type.Any;
                 3, 1, 3, 2, Type.integer;
                 3, 5, 3, 6, Type.literal_integer 2;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
      while x is True:
        y = 1
   |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 2, 6, 2, 15, Type.bool;
                 2, 11, 2, 15, Type.Literal (Boolean true);
                 3, 2, 3, 3, Type.literal_integer 1;
                 3, 6, 3, 7, Type.literal_integer 1;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
       def foo(x: int) -> str:
         def bar(y: int) -> str:
           return y
         return x
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.string;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "$parameter$x";
                                     annotation = Type.integer;
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 2, 8, 2, 9, Type.integer;
                 2, 11, 2, 14, Type.meta Type.integer;
                 2, 19, 2, 22, Type.meta Type.string;
                 3, 10, 3, 11, Type.integer;
                 3, 13, 3, 16, Type.meta Type.integer;
                 3, 21, 3, 24, Type.meta Type.string;
                 4, 11, 4, 12, Type.integer;
                 5, 9, 5, 10, Type.integer;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       def foo(x: typing.List[int]) -> None:
        pass
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 2,
                   4,
                   2,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.none;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "$parameter$x";
                                     annotation = Type.list Type.integer;
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 2, 8, 2, 9, Type.list Type.integer;
                 2, 11, 2, 27, Type.meta (Type.list Type.integer);
                 2, 32, 2, 36, Type.meta Type.none;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:{|
       class Foo:
         x = 1
     |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 {
                   Base.location = QueryTestTypes.create_location 2 6 2 9;
                   annotation = QueryTestTypes.parse_annotation "typing.Type[test.Foo]";
                 };
                 {
                   Base.location = QueryTestTypes.create_location 3 2 3 3;
                   annotation = Type.integer;
                 };
                 {
                   Base.location = QueryTestTypes.create_location 3 6 3 7;
                   annotation = Type.literal_integer 1;
                 };
               ];
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
      # foo.py
      from other_module import Bar

      my_bar: Bar

      def my_foo(bar: Bar) -> None:
        x = bar
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 5, 0, 5, 6, Type.Primitive "other_module.Bar";
                 5, 8, 5, 11, Type.meta (Type.Primitive "other_module.Bar");
                 ( 7,
                   4,
                   7,
                   10,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.my_foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.none;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "bar";
                                     annotation = QueryTestTypes.parse_annotation "other_module.Bar";
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 7, 11, 7, 14, Type.Primitive "other_module.Bar";
                 7, 16, 7, 19, Type.meta (Type.Primitive "other_module.Bar");
                 7, 24, 7, 28, Type.meta Type.none;
                 8, 2, 8, 3, Type.Primitive "other_module.Bar";
                 8, 6, 8, 9, Type.Primitive "other_module.Bar";
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
      # foo.py
      from other_module import Bar

      class Foo:
          foo_attribute: Bar

      f = Foo().foo_attribute
    |}
    ~query:"references_used_by_file(path='test.py')"
    ~no_validation_on_class_lookup_failure:true
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 5, 6, 5, 9, Type.meta (Type.Primitive "test.Foo");
                 6, 4, 6, 17, Type.Primitive "other_module.Bar";
                 6, 19, 6, 22, Type.meta (Type.Primitive "other_module.Bar");
                 8, 0, 8, 1, Type.Primitive "other_module.Bar";
                 8, 4, 8, 7, Type.meta (Type.Primitive "test.Foo");
                 8, 4, 8, 9, Type.Primitive "test.Foo";
                 8, 4, 8, 23, Type.Primitive "other_module.Bar";
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
        # foo.py
        from other_module import Bar

        def foo(x: str) -> Bar:
          return Bar()

        def baz(x: str) -> int:
          return foo(x)
    |}
    ~no_validation_on_class_lookup_failure:true
    ~query:"references_used_by_file(path='test.py')"
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 ( 5,
                   4,
                   5,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation =
                             QueryTestTypes.parse_annotation "other_module.Bar";
                           parameters =
                             Type.Callable.Defined
                               [Named { name = "x"; annotation = Type.string; default = false }];
                         };
                       overloads = [];
                     } );
                 5, 8, 5, 9, Type.string;
                 5, 11, 5, 14, Type.meta Type.string;
                 5, 19, 5, 22, Type.meta (QueryTestTypes.parse_annotation "other_module.Bar");
                 6, 9, 6, 14, Type.Any;
                 ( 8,
                   4,
                   8,
                   7,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.baz";
                       implementation =
                         {
                           Type.Callable.annotation = Type.integer;
                           parameters =
                             Type.Callable.Defined
                               [Named { name = "x"; annotation = Type.string; default = false }];
                         };
                       overloads = [];
                     } );
                 8, 8, 8, 9, Type.string;
                 8, 11, 8, 14, Type.meta Type.string;
                 8, 19, 8, 22, Type.meta Type.integer;
                 ( 9,
                   9,
                   9,
                   12,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.foo";
                       implementation =
                         {
                           Type.Callable.annotation =
                             QueryTestTypes.parse_annotation "other_module.Bar";
                           parameters =
                             Type.Callable.Defined
                               [Named { name = "x"; annotation = Type.string; default = false }];
                         };
                       overloads = [];
                     } );
                 9, 9, 9, 15, Type.Primitive "other_module.Bar";
                 9, 13, 9, 14, Type.string;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))
  >>= fun () ->
  (* Failure occurs because no_validation_on_class_lookup_failure must be true for
     references_used_by_file queries *)
  assert_type_query_response_with_local_root
    ~source:{|
        x = 4
        y = 3
     |}
    ~no_validation_on_class_lookup_failure:false
    ~query:"references_used_by_file(path='test.py')"
    (fun _ ->
      Error
        (Format.asprintf
           "Cannot run query references_used_by_file(path='test.py') because flag \
            'no_validation_on_class_lookup_failure' flag is false, and it is expected to be set to \
            true for all 'references_used_by_file queries'. Please set the value of \
            'no_validation_on_class_lookup_failure' to true."))
  >>= fun () ->
  assert_type_query_response_with_local_root
    ~source:
      {|
        from other_module import subscription
        class Bar:
            def foo(
                self, subscription_body: subscription.Body
            ) -> None:
                if isinstance(subscription_body, int):
                    y(self)
    |}
    ~no_validation_on_class_lookup_failure:true
    ~query:"references_used_by_file(path='test.py')"
    (fun _ ->
      Single
        (Base.ReferenceTypesInPath
           {
             Base.path = "test.py";
             types =
               [
                 3, 6, 3, 9, Type.meta (Type.Primitive "test.Bar");
                 ( 4,
                   8,
                   4,
                   11,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"test.Bar.foo";
                       implementation =
                         {
                           Type.Callable.annotation = Type.none;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "self";
                                     annotation = Type.Primitive "test.Bar";
                                     default = false;
                                   };
                                 Named
                                   {
                                     name = "subscription_body";
                                     annotation = Type.Primitive "other_module.subscription.Body";
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 5, 8, 5, 12, Type.Primitive "test.Bar";
                 5, 14, 5, 31, Type.Primitive "other_module.subscription.Body";
                 5, 33, 5, 50, Type.meta (Type.Primitive "other_module.subscription.Body");
                 6, 9, 6, 13, Type.meta Type.NoneType;
                 ( 7,
                   11,
                   7,
                   21,
                   Type.Callable
                     {
                       Type.Callable.kind = Type.Callable.Named !&"isinstance";
                       implementation =
                         {
                           Type.Callable.annotation = Type.bool;
                           parameters =
                             Type.Callable.Defined
                               [
                                 Named
                                   {
                                     name = "a";
                                     annotation = Type.object_primitive;
                                     default = false;
                                   };
                                 Named
                                   {
                                     name = "b";
                                     annotation =
                                       Type.Union
                                         [
                                           Type.Primitive "type";
                                           Type.tuple
                                             [
                                               Type.Union
                                                 [Type.Primitive "tuple"; Type.Primitive "type"];
                                               Type.Primitive "...";
                                             ];
                                         ];
                                     default = false;
                                   };
                               ];
                         };
                       overloads = [];
                     } );
                 7, 11, 7, 45, Type.bool;
                 7, 22, 7, 39, Type.Primitive "other_module.subscription.Body";
                 7, 41, 7, 44, Type.meta Type.integer;
                 8, 12, 8, 19, Type.Any;
               ]
               |> QueryTestTypes.create_types_at_locations;
           }))


let test_handle_query_with_build_system context =
  let custom_source_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let build_system_initializer =
    let initialize () =
      let lookup_artifact _ =
        [Test.relative_artifact_path ~root:custom_source_root ~relative:"redirected.py"]
      in
      Lwt.return (BuildSystem.create_for_testing ~lookup_artifact ())
    in
    let load () = failwith "saved state loading is not supported" in
    let cleanup () = Lwt.return_unit in
    BuildSystem.Initializer.create_for_testing ~initialize ~load ~cleanup ()
  in
  let test_query client =
    let open Lwt.Infix in
    Client.send_request client (Request.Query "types('original.py')")
    >>= fun actual_response ->
    let expected_response =
      Response.Query
        Query.Response.(Single (Base.TypesByPath [{ Base.path = "original.py"; types = [] }]))
      |> Response.to_yojson
      |> Yojson.Safe.to_string
    in
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected_response actual_response;
    Client.send_request client (Request.Query "modules_of_path('original.py')")
    >>= fun actual_response ->
    let expected_response =
      Response.Query Query.Response.(Single (Base.FoundModules [Reference.create "redirected"]))
      |> Response.to_yojson
      |> Yojson.Safe.to_string
    in
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected_response actual_response;
    Lwt.return_unit
  in
  ScratchProject.setup
    ~context
    ~include_helper_builtins:false
    ~build_system_initializer
    ~custom_source_root
    ["original.py", "x: int = 42"; "redirected.py", ""]
  |> ScratchProject.test_server_with ~f:test_query


let test_handle_query_pysa context =
  let queries_and_expected_responses =
    [
      ( "callees_with_location(wait.bar)",
        {|
        {
            "response": {
                "callees": [
                    {
                        "locations": [
                            {
                                "path":"wait.py",
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
                        "target": "wait.await_me"
                    }
                ]
            }
        }
        |}
      );
      ( "defines(test)",
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
        |}
      );
      ( "defines(classy)",
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
        |}
      );
      ( "defines(classy.C)",
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
        |}
      );
      ( "defines(define_test)",
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
        |}
      );
      "defines(nonexistent)", {|
        {
        "response": []
        }
        |};
      ( "defines(test, classy)",
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
        |}
      );
    ]
  in
  assert_queries_with_local_root
    ~context
    ~sources:
      [
        "test.py", {|
              def foo(a: int) -> int:
                return a
            |};
        ( "wait.py",
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
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_inline_decorators context =
  let queries_and_expected_responses =
    [
      ( "inline_decorators(test.foo)",
        {|
      {
      "response": {
        "definition": "def test.foo(a: int) -> int:
        def _original_function(a: int) -> int:
          return a|}
        ^ "\n        "
        ^ {|
        def _inlined_identity(a: int) -> int:
          _args = (a)
          _kwargs = { \"a\":a }
          return _original_function(a)|}
        ^ "\n        "
        ^ {|
        def _inlined_with_logging(a: int) -> int:
          _args = (a)
          _kwargs = { \"a\":a }
          print(_args, _kwargs)
          return _inlined_identity(a)|}
        ^ "\n        "
        ^ {|
        return _inlined_with_logging(a)
      "
      }
      }
    |} );
      ( "inline_decorators(test.non_existent)",
        {|
      {
        "error": "Could not find function `test.non_existent`"
      }
        |}
      );
      ( "inline_decorators(test.not_decorated)",
        {|
      {
        "response": {
          "definition": "def test.not_decorated(a: int) -> int:
        return a
      "  }
      }
        |}
      );
      ( "inline_decorators(test.foo, decorators_to_skip=[decorators.identity, \
         some.non_existent.decorator])",
        {|
      {
        "response": {
          "definition": "def test.foo(a: int) -> int:
        def _original_function(a: int) -> int:
          return a|}
        ^ "\n        "
        ^ {|
        def _inlined_with_logging(a: int) -> int:
          _args = (a)
          _kwargs = { \"a\":a }
          print(_args, _kwargs)
          return _original_function(a)|}
        ^ "\n        "
        ^ {|
        return _inlined_with_logging(a)
      "
        }
      }
        |} );
    ]
  in
  assert_queries_with_local_root
    ~context
    ~sources:
      [
        ( "test.py",
          {|
              from logging import with_logging
              from decorators import identity, not_inlinable

              @with_logging
              @not_inlinable
              @identity
              def foo(a: int) -> int:
                return a

              def not_decorated(a: int) -> int:
                return a
            |}
        );
        ( "logging.py",
          {|
              def with_logging(f):
                def inner( *args, **kwargs) -> int:
                  print(args, kwargs)
                  return f( *args, **kwargs)

                return inner
            |}
        );
        ( "decorators.py",
          {|
              def identity(f):
                def inner( *args, **kwargs) -> int:
                  return f( *args, **kwargs)

                return inner

              def not_inlinable(f):
                return f
            |}
        );
      ]
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             let indentation = 6 in
             response
             |> String.split ~on:'\n'
             |> List.map ~f:(fun s -> String.drop_prefix s indentation)
             |> String.concat ~sep:"\n"
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_location_of_definition context =
  let sources =
    [
      "stubbed_library.py", {|
              def identity(a):
                return a
            |};
      "stubbed_library.pyi", {|
              def identity(a: int) -> int: ...
            |};
      ( "foo.py",
        {|
              def foo(a: int) -> int:
                return a

              class Base:
                def __init__(self, x: int) -> None:
                  self.x = x
            |}
      );
      ( "bar.py",
        {|
              from foo import foo, Base
              from stubbed_library import identity

              class Child(Base): ...

              def bar() -> int:
                print(Child())
                print(Base())
                print(identity(42))
                return foo(42) + 1
            |}
      );
      ( "parse_error_example.py",
        {|
              this block of text will result in a parse error.
            |} );
    ]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let queries_and_expected_responses =
    [
      (* Look up `Base()`. *)
      ( Format.sprintf
          "location_of_definition(path='%s', line=9, column=9)"
          (PyrePath.append custom_source_root ~element:"bar.py" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 5,
                      "character": 0
                    },
                    "end": {
                      "line": 7,
                      "character": 14
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root) );
      (* Look up `identity`. *)
      ( Format.sprintf
          "location_of_definition(path='%s', line=10, column=9)"
          (PyrePath.append custom_source_root ~element:"bar.py" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/stubbed_library.pyi",
                  "range": {
                    "start": {
                      "line": 2,
                      "character": 0
                    },
                    "end": {
                      "line": 2,
                      "character": 32
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root) );
      (* Look up `def`. *)
      ( Format.sprintf
          "location_of_definition(path='%s', line=7, column=1)"
          (PyrePath.append custom_source_root ~element:"bar.py" |> PyrePath.absolute),
        {|
            {
              "response": []
            }
          |} );
      ( Format.sprintf
          "location_of_definition(path='%s', line=10, column=9)"
          (PyrePath.append custom_source_root ~element:"non_existent.py" |> PyrePath.absolute),
        {|
            {
              "response": []
            }
          |} );
      ( Format.sprintf
          "location_of_definition(path='%s', line=1, column=9)"
          (PyrePath.append custom_source_root ~element:"parse_error_example.py" |> PyrePath.absolute),
        {|
            {
              "error": "Parse error in location request. Location: 2:6-2:11, message: invalid syntax"
            }
          |}
      );
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_location_of_definition_with_build_system context =
  let sources =
    [
      "original.py", "x: int = 42";
      "not_tracked.pyi", "y: int = 43";
      ( "test.py",
        Test.trim_extra_indentation
          {|
          import original
          import not_tracked
          original.x
          not_tracked.y
        |}
      );
    ]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let test_path = PyrePath.create_relative ~root:custom_source_root ~relative:"test.py" in
  let original_path = PyrePath.create_relative ~root:custom_source_root ~relative:"original.py" in
  let redirected_path =
    PyrePath.create_relative ~root:custom_source_root ~relative:"redirected.py"
  in
  let not_tracked_path =
    PyrePath.create_relative ~root:custom_source_root ~relative:"not_tracked.pyi"
  in
  let build_system_initializer =
    let initialize () =
      (* We want a build system that redirects `original.py` to `redirected.py`, and pretends
         `not_tracked.py` does not exist. *)
      let lookup_source artifact_path =
        let raw_path = ArtifactPath.raw artifact_path in
        if PyrePath.equal raw_path not_tracked_path then
          None
        else if PyrePath.equal raw_path redirected_path then
          Some (SourcePath.create original_path)
        else
          Some (SourcePath.create raw_path)
      in
      let lookup_artifact source_path =
        let raw_path = SourcePath.raw source_path in
        if PyrePath.equal raw_path not_tracked_path then
          []
        else if PyrePath.equal raw_path original_path then
          [ArtifactPath.create redirected_path]
        else
          [ArtifactPath.create raw_path]
      in
      Lwt.return (BuildSystem.create_for_testing ~lookup_source ~lookup_artifact ())
    in
    let load () = failwith "saved state loading is not supported" in
    let cleanup () = Lwt.return_unit in
    BuildSystem.Initializer.create_for_testing ~initialize ~load ~cleanup ()
  in
  let queries_and_expected_responses =
    [
      (* Look up `original.x`. *)
      ( Format.sprintf
          "location_of_definition(path='%s', line=4, column=9)"
          (PyrePath.absolute test_path),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/original.py",
                  "range": {
                    "start": {
                      "line": 1,
                      "character": 0
                    },
                    "end": {
                      "line": 1,
                      "character": 1
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root) );
      (* Look up `not_tracked.y`. *)
      ( Format.sprintf
          "location_of_definition(path='%s', line=5, column=12)"
          (PyrePath.absolute test_path),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/not_tracked.pyi",
                  "range": {
                    "start": {
                      "line": 1,
                      "character": 0
                    },
                    "end": {
                      "line": 1,
                      "character": 1
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root) );
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~build_system_initializer
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_find_references context =
  let sources =
    [
      "stubbed_library.py", {|
              def identity(a):
                return a
            |};
      "stubbed_library.pyi", {|
              def identity(a: int) -> int: ...
            |};
      ( "foo.py",
        {|
              def foo(a: int) -> int:
                x = a
                return x

              class Base:
                def __init__(self, x: int) -> None:
                  self.x = x
            |}
      );
      ( "bar.py",
        {|
              from foo import foo, Base
              from stubbed_library import identity

              class Child(Base): ...

              def bar() -> int:
                return foo(0)
            |}
      );
    ]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let queries_and_expected_responses =
    [
      ( Format.sprintf
          "find_references(path='%s', line=3, column=2)"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 3,
                      "character": 2
                    },
                    "end": {
                      "line": 3,
                      "character": 3
                    }
                  }
                },
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 4,
                      "character": 9
                    },
                    "end": {
                      "line": 4,
                      "character": 10
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root)
          (PyrePath.absolute custom_source_root) );
      (* Find all references on function parameter `foo.a`. *)
      ( Format.sprintf
          "find_references(path='%s', line=2, column=8)"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 2,
                      "character": 8
                    },
                    "end": {
                      "line": 2,
                      "character": 14
                    }
                  }
                },
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 3,
                      "character": 6
                    },
                    "end": {
                      "line": 3,
                      "character": 7
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root)
          (PyrePath.absolute custom_source_root) );
      ( Format.sprintf
          "find_references(path='%s', line=3, column=6)"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 2,
                      "character": 8
                    },
                    "end": {
                      "line": 2,
                      "character": 14
                    }
                  }
                },
                {
                  "path": "%s/foo.py",
                  "range": {
                    "start": {
                      "line": 3,
                      "character": 6
                    },
                    "end": {
                      "line": 3,
                      "character": 7
                    }
                  }
                }
              ]
            }
          |}
          (PyrePath.absolute custom_source_root)
          (PyrePath.absolute custom_source_root) );
      (* Find all references on keyword / syntax *)
      ( Format.sprintf
          "find_references(path='%s', line=2, column=1)"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute),
        {|{"response": []}|} );
      ( Format.sprintf
          "find_references(path='%s', line=2, column=7)"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute),
        {|{"response": []}|} );
      (* TODO(T114362295): Support global find all references. *)
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_expression_level_coverage context =
  let sources =
    [
      "foo.py", {|
              def foo(x) -> None:
                print(x + 1)
            |};
      "one.py", {|
              def one(x) -> None:
                print(x + 1)
            |};
      ( "two.py",
        {|
              # pyre-strict
              def two(x):
                print(x + 1)
            |}
      );
      "bar.pyi", {|
            def foo(x) -> None:
              ...
          |};
      "bar.py", {|
            def foo(x) -> None:
              print(x+1)
          |};
      "arguments.txt", {|
      foo.py
      |};
      "empty.txt", {||};
      "two_arguments.txt", {|
      foo.py
      one.py
      |};
    ]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let build_response response =
    Format.asprintf {|
        {
          "response": [%s]
        }
      |} response
  in
  let foo_response path function_name =
    Format.asprintf
      {|
        [
        "CoverageAtPath",
          {
              "path": "%s",
              "total_expressions": 8,
              "coverage_gaps": [
                  {
                      "location": {
                          "start": {
                              "line": 2,
                              "column": 4
                          },
                          "stop": {
                              "line": 2,
                              "column": 7
                          }
                      },
                      "function_name": "%s.%s",
                      "type_": "typing.Callable(%s.%s)[[Named(x, unknown)], None]",
                      "reason": ["%s"]
                  },
                  {
                      "location": {
                          "start": {
                              "line": 2,
                              "column": 8
                          },
                          "stop": {
                              "line": 2,
                              "column": 9
                          }
                      },
                      "function_name": null,
                      "type_": "typing.Any",
                      "reason": ["%s"]
                  },
                  {
                      "location": {
                          "start": {
                              "line": 3,
                              "column": 8
                          },
                          "stop": {
                              "line": 3,
                              "column": 9
                          }
                      },
                      "function_name": null,
                      "type_": "typing.Any",
                      "reason": ["%s"]
                  },
                  {
                      "location": {
                          "start": {
                              "line": 3,
                              "column": 8
                          },
                          "stop": {
                              "line": 3,
                              "column": 13
                          }
                      },
                      "function_name": null,
                      "type_": "typing.Any",
                      "reason": ["%s"]
                  }
              ]
          }
        ]
        |}
      path
      function_name
      function_name
      function_name
      function_name
      (List.nth_exn LocationBasedLookup.callable_parameter_is_unknown_or_any_message 0)
      (List.nth_exn LocationBasedLookup.parameter_is_any_message 0)
      (List.nth_exn LocationBasedLookup.parameter_is_any_message 0)
      (List.nth_exn LocationBasedLookup.expression_is_any_message 0)
  in
  let error_response file_name error =
    Format.sprintf
      {|
          ["ErrorAtPath",
            {"path":"%s","error":"Not able to get lookups in: `%s` (%s)"}
          ]
      |}
      file_name
      file_name
      error
  in
  let build_foo_response path function_name = build_response (foo_response path function_name) in
  let build_error_response file_name error = build_response (error_response file_name error) in
  let build_foo_and_error_response path function_name error_file_name error =
    let response = foo_response path function_name ^ "," ^ error_response error_file_name error in
    build_response response
  in
  let build_foo_and_bar_response path function_name other_path other_function_name =
    let response =
      foo_response path function_name ^ "," ^ foo_response other_path other_function_name
    in
    build_response response
  in
  let queries_and_expected_responses =
    [
      (* Test Empty request *)
      Format.sprintf "expression_level_coverage()", Format.asprintf {| { "response": [ ] } |};
      (* Ok request *)
      ( Format.sprintf
          "expression_level_coverage('%s')"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute),
        build_foo_response
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute)
          "foo" );
      (* No type annotations in signature. *)
      ( Format.sprintf
          "expression_level_coverage('%s')"
          (PyrePath.append custom_source_root ~element:"two.py" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
                [
                "CoverageAtPath",
                  {
                      "path": "%s/two.py",
                      "total_expressions": 7,
                      "coverage_gaps": [
                          {
                              "location": {
                                  "start": {
                                      "line": 3,
                                      "column": 4
                                  },
                                  "stop": {
                                      "line": 3,
                                      "column": 7
                                  }
                              },
                              "function_name": "two.two",
                              "type_": "typing.Callable(two.two)[[Named(x, unknown)], typing.Any]",
                              "reason": ["%s"]
                          },
                          {
                              "location": {
                                  "start": {
                                      "line": 3,
                                      "column": 8
                                  },
                                  "stop": {
                                      "line": 3,
                                      "column": 9
                                  }
                              },
                              "function_name": null,
                              "type_": "typing.Any",
                              "reason": ["%s"]
                          },
                          {
                              "location": {
                                  "start": {
                                      "line": 4,
                                      "column": 8
                                  },
                                  "stop": {
                                      "line": 4,
                                      "column": 9
                                  }
                              },
                              "function_name": null,
                              "type_": "typing.Any",
                              "reason": ["%s"]
                          },
                          {
                              "location": {
                                  "start": {
                                      "line": 4,
                                      "column": 8
                                  },
                                  "stop": {
                                      "line": 4,
                                      "column": 13
                                  }
                              },
                              "function_name": null,
                              "type_": "typing.Any",
                              "reason": ["%s"]
                          }
                      ]
                  }
                ]
              ]
          }
         |}
          (PyrePath.absolute custom_source_root)
          (List.nth_exn LocationBasedLookup.callable_return_is_any_message 0)
          (List.nth_exn LocationBasedLookup.parameter_is_any_message 0)
          (List.nth_exn LocationBasedLookup.parameter_is_any_message 0)
          (List.nth_exn LocationBasedLookup.expression_is_any_message 0) );
      (* Test Error FileNotFound *)
      ( Format.sprintf
          "expression_level_coverage('%s')"
          (PyrePath.append custom_source_root ~element:"file_not_found.py" |> PyrePath.absolute),
        build_error_response
          (PyrePath.append custom_source_root ~element:"file_not_found.py" |> PyrePath.absolute)
          "file not found" );
      (* Test Error StubShadowing *)
      ( Format.sprintf
          "expression_level_coverage('%s')"
          (PyrePath.append custom_source_root ~element:"bar.py" |> PyrePath.absolute),
        build_error_response
          (PyrePath.append custom_source_root ~element:"bar.py" |> PyrePath.absolute)
          "file shadowed by .pyi stub file" );
      (* Test @not_existing.txt not found *)
      ( Format.sprintf
          "expression_level_coverage('@%s')"
          (PyrePath.append custom_source_root ~element:"not_existing.txt" |> PyrePath.absolute),
        build_error_response
          (PyrePath.append custom_source_root ~element:"not_existing.txt" |> PyrePath.absolute)
          "file not found" );
      (* Test @empty.txt not found *)
      ( Format.sprintf
          "expression_level_coverage('@%s')"
          (PyrePath.append custom_source_root ~element:"empty.txt" |> PyrePath.absolute),
        Format.asprintf
          {|
            {
              "response": [
              ]
          }
         |} );
      (* Test Response and Error *)
      ( Format.sprintf
          "expression_level_coverage('%s','%s')"
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute)
          (PyrePath.append custom_source_root ~element:"not_existing.py" |> PyrePath.absolute),
        build_foo_and_error_response
          (PyrePath.append custom_source_root ~element:"foo.py" |> PyrePath.absolute)
          "foo"
          (PyrePath.append custom_source_root ~element:"not_existing.py" |> PyrePath.absolute)
          "file not found" );
      (* Test @arguments.txt *)
      ( Format.sprintf
          "expression_level_coverage('@%s')"
          (PyrePath.append custom_source_root ~element:"arguments.txt" |> PyrePath.absolute),
        build_foo_response "foo.py" "foo" );
      (* Test @two_arguments.txt *)
      ( Format.sprintf
          "expression_level_coverage('@%s')"
          (PyrePath.append custom_source_root ~element:"two_arguments.txt" |> PyrePath.absolute),
        build_foo_and_bar_response "foo.py" "foo" "one.py" "one" );
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_hover context =
  let sources =
    ["foo.py", {|
              def foo(x: int) -> int:
                return x
            |}]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let queries_and_expected_responses =
    [
      ( "hover_info_for_position(path='foo.py', line=2, column=0)",
        {|
      {
      "response": { "value": null, "docstring": null}
      }
    |} );
      ( "hover_info_for_position(path='foo.py', line=3, column=9)",
        {|
      {
      "response": { "value": "int", "docstring": null}
      }
    |} );
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_dump_call_graph context =
  let sources =
    [
      ( "foo.py",
        {|
              from bar import bar2
              def foo(x: int) -> int:
                return foo2(x) + bar2(x)

              def foo2(x: int) -> int:
                return x

              def not_called(x: int) -> int:
                print(x)
            |}
      );
      ( "bar.py",
        {|
              def bar(x: int) -> int:
                return x

              def bar2(x: int) -> int:

                def inner(x: int) -> int:
                  return bar(x)

                return inner(x)
            |}
      );
    ]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let set_path_in_json json =
    let root_str = PyrePath.show custom_source_root in
    Format.sprintf json root_str root_str root_str root_str root_str root_str
  in

  let queries_and_expected_responses =
    [
      ( "dump_call_graph()",
        set_path_in_json
          {|
          {
            "response": {
              "typing.Iterable.__iter__": [],
              "str.substr": [],
              "str.lower": [],
              "pyre_extensions.override": [],
              "pyre_extensions.generic.Generic.__class_getitem__": [],
              "foo.not_called": [
                {
                  "locations": [
                    {
                      "path": "%s/foo.py",
                      "start": { "line": 10, "column": 2 },
                      "stop": { "line": 10, "column": 7 }
                    }
                  ],
                  "kind": "function",
                  "target": "print"
                }
              ],
              "foo.foo2": [],
              "foo.foo": [
                {
                  "locations": [
                    {
                      "path": "%s/foo.py",
                      "start": { "line": 4, "column": 9 },
                      "stop": { "line": 4, "column": 16 }
                    }
                  ],
                  "kind": "method",
                  "is_optional_class_attribute": false,
                  "direct_target": "int.__add__",
                  "class_name": "int",
                  "dispatch": "dynamic"
                },
                {
                  "locations": [
                    {
                      "path": "%s/foo.py",
                      "start": { "line": 4, "column": 19 },
                      "stop": { "line": 4, "column": 23 }
                    }
                  ],
                  "kind": "function",
                  "target": "bar.bar2"
                },
                {
                  "locations": [
                    {
                      "path": "%s/foo.py",
                      "start": { "line": 4, "column": 9 },
                      "stop": { "line": 4, "column": 13 }
                    }
                  ],
                  "kind": "function",
                  "target": "foo.foo2"
                }
              ],
              "dict.items": [],
              "dict.add_value": [],
              "dict.add_key": [],
              "dict.add_both": [],
              "contextlib.ContextManager.__enter__": [],
              "bar.bar2.inner": [
                {
                  "locations": [
                    {
                      "path": "%s/bar.py",
                      "start": { "line": 8, "column": 11 },
                      "stop": { "line": 8, "column": 14 }
                    }
                  ],
                  "kind": "function",
                  "target": "bar.bar"
                }
              ],
              "bar.bar2": [
                {
                  "locations": [
                    {
                      "path": "%s/bar.py",
                      "start": { "line": 10, "column": 9 },
                      "stop": { "line": 10, "column": 14 }
                    }
                  ],
                  "kind": "function",
                  "target": "bar.bar2.inner"
                }
              ],
              "bar.bar": []
            }
          }
    |}
      );
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let test_global_leaks context =
  (* TODO (T144319460): the global write in `nested_run()` should be caught after define statements
     are implemented *)
  let sources =
    [
      ( "foo.py",
        {|
          from typing import List

          glob: List[int] = []

          def nested_run():
              def do_the_thing():
                  glob.append(1)
              do_the_thing()


          def nested_run_2():
              def do_the_thing_2():
                  def another_nest():
                     glob.append(2)
                  another_nest()
              do_the_thing_2()


          def immediate_example():
              glob.append(1)


          def get_these():
              immediate_example()
              nested_run()
        |}
      );
    ]
  in
  let custom_source_root =
    OUnit2.bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let temp_file_name = PyrePath.append custom_source_root ~element:"foo.py" in
  let set_path_in_json =
    String.substr_replace_all ~pattern:"%s" ~with_:(PyrePath.show temp_file_name)
  in
  let queries_and_expected_responses =
    [
      ( "global_leaks(foo.get_these, foo.immediate_example, foo.nested_run, \
         foo.nested_run.do_the_thing, foo.nested_run_2.do_the_thing_2.another_nest, \
         foo.this_one_doesnt_exist)",
        set_path_in_json
          {|
          {
            "response": {
              "query_errors": ["No qualifier found for `foo.this_one_doesnt_exist`"],
              "global_leaks": [
                  {
                    "line": 21,
                    "column": 4,
                    "stop_line": 21,
                    "stop_column": 15,
                    "path": "%s",
                    "code": 3101,
                    "name": "Leak to a mutable datastructure",
                    "description": "Leak to a mutable datastructure [3101]: Data write to global variable `foo.glob` of type `typing.List[int]`.",
                    "long_description": "Leak to a mutable datastructure [3101]: Data write to global variable `foo.glob` of type `typing.List[int]`.",
                    "concise_description": "Leak to a mutable datastructure [3101]: Data write to global variable `glob` of type `typing.List[int]`.",
                    "define": "foo.immediate_example"
                  },
                  {
                    "line": 8,
                    "column": 8,
                    "stop_line": 8,
                    "stop_column": 19,
                    "path": "%s",
                    "code": 3101,
                    "name": "Leak to a mutable datastructure",
                    "description": "Leak to a mutable datastructure [3101]: Data write to global variable `foo.glob` of type `typing.List[int]`.",
                    "long_description": "Leak to a mutable datastructure [3101]: Data write to global variable `foo.glob` of type `typing.List[int]`.",
                    "concise_description": "Leak to a mutable datastructure [3101]: Data write to global variable `glob` of type `typing.List[int]`.",
                    "define": "foo.nested_run.do_the_thing"
                  },
                  {
                    "line": 15,
                    "column": 11,
                    "stop_line": 15,
                    "stop_column": 22,
                    "path": "%s",
                    "code": 3101,
                    "name": "Leak to a mutable datastructure",
                    "description": "Leak to a mutable datastructure [3101]: Data write to global variable `foo.glob` of type `typing.List[int]`.",
                    "long_description": "Leak to a mutable datastructure [3101]: Data write to global variable `foo.glob` of type `typing.List[int]`.",
                    "concise_description": "Leak to a mutable datastructure [3101]: Data write to global variable `glob` of type `typing.List[int]`.",
                    "define": "foo.nested_run_2.do_the_thing_2.another_nest"
                  }
              ]
            } 
          }
        |}
      );
    ]
  in
  assert_queries_with_local_root
    ~custom_source_root
    ~context
    ~sources
    (List.map queries_and_expected_responses ~f:(fun (query, response) ->
         ( query,
           fun _ ->
             response
             |> Yojson.Safe.from_string
             |> fun json -> `List [`String "Query"; json] |> Yojson.Safe.to_string )))


let () =
  "query"
  >::: [
         "parse_query" >:: test_parse_query;
         "handle_query_basic" >:: OUnitLwt.lwt_wrapper test_handle_query_basic;
         "handle_query_references_used_by_file"
         >:: OUnitLwt.lwt_wrapper test_handle_references_used_by_file_query;
         "handle_query_types" >:: OUnitLwt.lwt_wrapper test_handle_types_query;
         "handle_query_with_build_system"
         >:: OUnitLwt.lwt_wrapper test_handle_query_with_build_system;
         "handle_query_pysa" >:: OUnitLwt.lwt_wrapper test_handle_query_pysa;
         "inline_decorators" >:: OUnitLwt.lwt_wrapper test_inline_decorators;
         "location_of_definition" >:: OUnitLwt.lwt_wrapper test_location_of_definition;
         "location_of_definition_with_build_system"
         >:: OUnitLwt.lwt_wrapper test_location_of_definition_with_build_system;
         "find_references" >:: OUnitLwt.lwt_wrapper test_find_references;
         "expression_level_coverage" >:: OUnitLwt.lwt_wrapper test_expression_level_coverage;
         "hover" >:: OUnitLwt.lwt_wrapper test_hover;
         "dump_call_graph" >:: OUnitLwt.lwt_wrapper test_dump_call_graph;
         "global_leaks" >:: OUnitLwt.lwt_wrapper test_global_leaks;
       ]
  |> Test.run
