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
  assert_parses "validate_taint_models()" (ValidateTaintModels None);
  assert_parses "validate_taint_models('foo.py')" (ValidateTaintModels (Some "foo.py"));
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
  assert_parses "modules_of_path('/a.py')" (ModulesOfPath (PyrePath.create_absolute "/a.py"));
  ()


let assert_queries_with_local_root ?custom_source_root ~context ~sources queries_and_responses =
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
      assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected_response actual_response;
      Lwt.return_unit
    in
    Lwt_list.iter_s handle_one_query queries_and_responses
  in
  ScratchProject.setup ?custom_source_root ~context ~include_helper_builtins:false sources
  |> ScratchProject.test_server_with ~f:test_handle_query


let test_handle_query_basic context =
  let open Query.Response in
  let assert_type_query_response_with_local_root
      ?custom_source_root
      ?(handle = "test.py")
      ~source
      ~query
      build_expected_response
    =
    let build_expected_response local_root =
      Response.Query (build_expected_response local_root)
      |> Response.to_yojson
      |> Yojson.Safe.to_string
    in
    assert_queries_with_local_root
      ?custom_source_root
      ~context
      ~sources:[handle, source]
      [query, build_expected_response]
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
  let parse_annotation serialized =
    serialized
    |> (fun literal ->
         Expression.Expression.Constant
           (Expression.Constant.String (Expression.StringLiteral.create literal)))
    |> Node.create_with_default_location
    |> Type.create ~aliases:Type.empty_aliases
  in
  let create_location start_line start_column stop_line stop_column =
    let start = { Location.line = start_line; column = start_column } in
    let stop = { Location.line = stop_line; column = stop_column } in
    { Location.start; stop }
  in
  let create_types_at_locations =
    let convert (start_line, start_column, end_line, end_column, annotation) =
      { Base.location = create_location start_line start_column end_line end_column; annotation }
    in
    List.map ~f:convert
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
                   2, 40, 2, 44, Type.none;
                   3, 2, 3, 3, Type.literal_integer 42;
                   3, 6, 3, 8, Type.literal_integer 42;
                 ]
                 |> create_types_at_locations;
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
                 |> create_types_at_locations;
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
                 |> create_types_at_locations;
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
                 |> create_types_at_locations;
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
                   3, 11, 3, 17, Type.list Type.integer;
                   3, 12, 3, 13, Type.literal_integer 1;
                   3, 15, 3, 16, Type.literal_integer 2;
                   4, 3, 4, 4, Type.literal_integer 1;
                   4, 7, 4, 8, Type.literal_integer 1;
                 ]
                 |> create_types_at_locations;
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
                   2, 13, 2, 17, Type.none;
                   4, 4, 4, 5, Type.literal_integer 1;
                   4, 8, 4, 9, Type.literal_integer 1;
                   5, 9, 5, 18, Type.parametric "type" [Single (Type.Primitive "Exception")];
                   6, 4, 6, 5, Type.literal_integer 2;
                   6, 8, 6, 9, Type.literal_integer 2;
                 ]
                 |> create_types_at_locations;
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
                 |> create_types_at_locations;
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
                 |> create_types_at_locations;
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
                 |> create_types_at_locations;
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
                   2, 32, 2, 36, Type.none;
                 ]
                 |> create_types_at_locations;
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
                     Base.location = create_location 2 6 2 9;
                     annotation = parse_annotation "typing.Type[test.Foo]";
                   };
                   { Base.location = create_location 3 2 3 3; annotation = Type.integer };
                   { Base.location = create_location 3 6 3 7; annotation = Type.literal_integer 1 };
                 ];
             };
           ]))
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
  assert_equal `Yes (Sys.is_file (temporary_directory ^/ "state"));
  Lwt.return_unit


let test_handle_query_with_build_system context =
  let custom_source_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let build_system_initializer =
    let initialize () =
      let lookup_artifact _ =
        [PyrePath.create_relative ~root:custom_source_root ~relative:"redirected.py"]
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
      ( "dump_call_graph()",
        {|
        {
            "response": {
                "typing.Iterable.__iter__": [],
                "pyre_extensions.generic.Generic.__class_getitem__": [],
                "wait.bar": [
                    {
                        "locations": [
                            {
                                "path": "wait.py",
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
                ],
                "contextlib.ContextManager.__enter__": [],
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


let () =
  "query"
  >::: [
         "parse_query" >:: test_parse_query;
         "handle_query_basic" >:: OUnitLwt.lwt_wrapper test_handle_query_basic;
         "handle_query_with_build_system"
         >:: OUnitLwt.lwt_wrapper test_handle_query_with_build_system;
         "handle_query_pysa" >:: OUnitLwt.lwt_wrapper test_handle_query_pysa;
         "inline_decorators" >:: OUnitLwt.lwt_wrapper test_inline_decorators;
       ]
  |> Test.run
