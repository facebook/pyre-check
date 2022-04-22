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
open Expression
open Test

let show_location { Location.start; stop } =
  Format.asprintf "%a-%a" Location.pp_position start Location.pp_position stop


let generate_lookup ~context ?(environment_sources = []) source =
  let environment =
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] ~external_sources:environment_sources
      |> ScratchProject.build_type_environment
    in
    TypeEnvironment.read_only type_environment
  in
  let lookup = LocationBasedLookup.create_of_module environment !&"test" in
  Memory.reset_shared_memory ();
  lookup


let assert_annotation_list ~lookup expected =
  let list_diff format list = Format.fprintf format "%s\n" (String.concat ~sep:"\n" list) in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    ~pp_diff:(diff ~print:list_diff)
    expected
    (LocationBasedLookup.get_all_resolved_types lookup
    |> List.sort ~compare:[%compare: Location.t * Type.t]
    |> List.map ~f:(fun (key, data) -> Format.asprintf "%s/%a" (show_location key) Type.pp data))


let assert_annotation ~lookup ~position ~annotation =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    annotation
    (LocationBasedLookup.get_resolved_type lookup ~position
    >>| fun (location, annotation) ->
    Format.asprintf "%s/%a" (show_location location) Type.pp annotation)


let test_lookup_out_of_bounds_location context =
  let source =
    {|
      1
      def foo() -> int:
          arbitrary["key"] = value
          return (
              1
          )
    |}
  in
  let lookup = generate_lookup ~context source in
  (* Test that no crazy combination crashes the lookup. *)
  let indices = [-100; -1; 0; 1; 2; 3; 4; 5; 12; 18; 28; 100] in
  let indices_product =
    List.concat_map indices ~f:(fun index_one ->
        List.map indices ~f:(fun index_two -> index_one, index_two))
  in
  let test_one (line, column) =
    LocationBasedLookup.get_resolved_type lookup ~position:{ Location.line; column } |> ignore
  in
  List.iter indices_product ~f:test_one


let test_lookup_pick_narrowest context =
  let source =
    {|
      def foo(flag: bool, testme: typing.Optional[bool]) -> None:
          if flag and (not testme):
              pass
    |}
  in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:4-2:7/typing.Callable(test.foo)[[Named(flag, bool), Named(testme, \
       typing.Optional[bool])], None]";
      "2:8-2:12/bool";
      "2:14-2:18/typing.Type[bool]";
      "2:20-2:26/typing.Optional[bool]";
      "2:28-2:49/typing.Type[typing.Optional[bool]]";
      "2:54-2:58/None";
      "3:7-3:11/bool";
      (* TODO (T68817342): Should be `bool` *)
      "3:7-3:27/typing.Optional[bool]";
      "3:17-3:27/bool";
      "3:21-3:27/typing.Optional[bool]";
    ];
  let assert_annotation = assert_annotation ~lookup in
  assert_annotation
    ~position:{ Location.line = 3; column = 11 } (* TODO (T68817342): Should be `bool` *)
    ~annotation:(Some "3:7-3:27/typing.Optional[bool]");
  assert_annotation
    ~position:{ Location.line = 3; column = 16 } (* TODO (T68817342): Should be `bool` *)
    ~annotation:(Some "3:7-3:27/typing.Optional[bool]");
  assert_annotation ~position:{ Location.line = 3; column = 17 } ~annotation:(Some "3:17-3:27/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 21 }
    ~annotation:(Some "3:21-3:27/typing.Optional[bool]");
  assert_annotation ~position:{ Location.line = 3; column = 28 } ~annotation:None


let test_narrowest_match _ =
  let open LocationBasedLookup in
  let assert_narrowest expressions expected =
    let narrowest_matching_expression =
      expressions
      |> List.map ~f:(fun (expression, location) ->
             {
               symbol_with_definition =
                 Expression
                   { (parse_single_expression expression) with location = parse_location location };
               cfg_data = { define_name = !&"test.foo"; node_id = 0; statement_index = 0 };
               use_postcondition_info = false;
             })
      |> narrowest_match
      >>| function
      | { symbol_with_definition = Expression expression; _ } -> expression
      | _ -> failwith "Expected expression"
    in
    assert_equal
      ~cmp:(fun left right ->
        match left, right with
        | Some left, Some right -> location_insensitive_compare left right = 0
        | None, None -> true
        | _ -> false)
      ~printer:[%show: Expression.t option]
      (expected >>| parse_single_expression)
      narrowest_matching_expression
  in
  assert_narrowest
    [
      "library.return_str().capitalize().lower()", "2:5-2:38";
      "library.return_str().capitalize().lower", "2:5-2:36";
      "library.return_str().capitalize()", "2:5-2:30";
      "library.return_str().capitalize", "2:5-2:28";
    ]
    (Some "library.return_str().capitalize");
  assert_narrowest [] None;
  ()


let test_find_narrowest_spanning_symbol context =
  let external_sources =
    [
      ( "library.py",
        {|
      class Base: ...

      def return_str() -> str:
          return "hello"
    |} );
    ]
  in
  let open LocationBasedLookup in
  let assert_narrowest_expression ?(external_sources = []) ~source position expected =
    let type_environment =
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] ~external_sources
        |> ScratchProject.build_type_environment
      in
      TypeEnvironment.read_only type_environment
    in
    assert_equal
      ~cmp:(fun left right ->
        Option.compare location_insensitive_compare_symbol_and_cfg_data left right = 0)
      ~printer:[%show: symbol_and_cfg_data option]
      expected
      (LocationBasedLookup.find_narrowest_spanning_symbol
         ~type_environment
         ~module_reference:!&"test"
         (parse_position position))
  in
  assert_narrowest_expression
    ~source:{|
        def getint() -> int:
          return 12
    |}
    "2:4"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "test.getint");
         cfg_data = { define_name = !&"test.getint"; node_id = 0; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def getint() -> int:
          return 12
    |}
    "2:3"
    None;
  assert_narrowest_expression
    ~source:{|
        def getint() -> int:
          return 12
    |}
    "2:16"
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "int");
         cfg_data = { define_name = !&"test.getint"; node_id = 0; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources
    ~source:{|
        from library import Base
    |}
    "2:20"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library.Base");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def foo(a: int, b: str) -> None: ...
    |}
    "2:16"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$b")));
         cfg_data = { define_name = !&"test.foo"; node_id = 0; statement_index = 0 };
         use_postcondition_info = true;
       });
  assert_narrowest_expression
    ~source:{|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
    |}
    "3:2"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location
                (Expression.Name (Name.Identifier "$local_test?foo$xs")));
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = true;
       });
  assert_narrowest_expression
    ~source:{|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
    |}
    "3:18"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression {|["a", "b"]|});
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
    |}
    "3:6"
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "list[str]");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources
    ~source:
      {|
        from library import Base
        def foo() -> None:
          print(Base())
    |}
    "4:8"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library.Base");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  (* TODO(T112570623): This should probably be `getint().__add__` so that we can go to the
     definition of `+`. *)
  assert_narrowest_expression
    ~source:
      {|
        def getint() -> int:
          return 42

        def foo() -> None:
          getint() + 2
    |}
    "6:11"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "test.getint() + 2");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def return_str() -> str:
          return "hello"

        def foo() -> None:
          return_str().capitalize().lower()
    |}
    "6:19"
    (Some
       {
         symbol_with_definition =
           Expression (parse_single_expression "(test.return_str()).capitalize");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
      class Foo:
        def bar(self) -> None:
            print(self.foo())
    |}
    "4:12"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location
                (Expression.Name (Name.Identifier "$parameter$self")));
         cfg_data = { define_name = !&"test.Foo.bar"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
      class Foo:
        def bar(self) -> None:
            print(self.foo())

        def foo(self) -> int:
          return 42
    |}
    "4:17"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location
                (Expression.Name
                   (Name.Attribute
                      {
                        base =
                          Node.create_with_default_location
                            (Expression.Name (Name.Identifier "$parameter$self"));
                        attribute = "foo";
                        special = false;
                      })));
         cfg_data = { define_name = !&"test.Foo.bar"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def takes_int(x: int) -> None: ...

        def foo() -> None:
          takes_int(x=42)
    |}
    "5:14"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "42");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo: ...

        class Bar:
          some_attribute: Foo = Foo()
    |}
    "5:2"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "test.Bar.some_attribute");
         cfg_data = { define_name = !&"test.Bar.$class_toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = true;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo: ...

        class Bar:
          some_attribute: Foo = Foo()
    |}
    "5:18"
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "test.Foo");
         cfg_data = { define_name = !&"test.Bar.$class_toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo:
          def bar(self) -> None:
              pass

        class Bar:
          some_attribute: Foo = Foo()

          def foo(self) -> Foo:
              return Foo()

        def test() -> None:
          Bar().foo().bar()
    |}
    "13:14"
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "(test.Bar()).foo().bar");
         cfg_data = { define_name = !&"test.test"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def foo(x: str) -> None:
          print(x)
    |}
    "3:8"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$x")));
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def foo() -> None:
          x = 42
          print(x)
    |}
    "4:8"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location
                (Expression.Name (Name.Identifier "$local_test?foo$x")));
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo() -> None:
          with open() as f:
            f.readline()
    |}
    "4:4"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$target$f")));
         cfg_data = { define_name = !&"test.foo"; node_id = 5; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def foo() -> None:
          for x in [1]:
            print(x)
    |}
    "4:10"
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$target$x")));
         cfg_data = { define_name = !&"test.foo"; node_id = 6; statement_index = 1 };
         use_postcondition_info = false;
       });
  ()


let test_resolve_definition_for_symbol context =
  let assert_resolved_definition ?(external_sources = []) ~source symbol_data expected =
    let type_environment =
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] ~external_sources
        |> ScratchProject.build_type_environment
      in
      TypeEnvironment.read_only type_environment
    in
    assert_equal
      ~cmp:[%compare.equal: Location.WithModule.t option]
      ~printer:[%show: Location.WithModule.t option]
      (expected >>| parse_location_with_module)
      (LocationBasedLookup.resolve_definition_for_symbol
         ~type_environment
         ~module_reference:!&"test"
         symbol_data)
  in
  let external_sources =
    [
      ( "library.py",
        {|
      class Base: ...

      def return_str() -> str:
          return "hello"
    |} );
    ]
  in
  let open LocationBasedLookup in
  assert_resolved_definition
    ~source:{|
        def getint() -> int:
          return 42
    |}
    {
      symbol_with_definition = Expression (parse_single_expression "test.getint");
      cfg_data = { define_name = !&"test.getint"; node_id = 0; statement_index = 0 };
      use_postcondition_info = false;
    }
    (Some "test:2:0-3:11");
  assert_resolved_definition
    ~source:{|
        def foo(a: int, b: str) -> None: ...
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$b")));
      cfg_data = { define_name = !&"test.foo"; node_id = 0; statement_index = 0 };
      use_postcondition_info = true;
    }
    None;
  assert_resolved_definition
    ~source:{|
        def foo(x: str) -> None:
          print(x)
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$x")));
      cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
      use_postcondition_info = false;
    }
    (Some "test:2:8-2:14");
  assert_resolved_definition
    ~source:{|
        def getint() -> int:
          return 42
    |}
    {
      symbol_with_definition = TypeAnnotation (parse_single_expression "int");
      cfg_data = { define_name = !&"test.getint"; node_id = 0; statement_index = 0 };
      use_postcondition_info = false;
    }
    (Some ":120:0-181:32");
  assert_resolved_definition
    ~source:{|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
    |}
    {
      symbol_with_definition = TypeAnnotation (parse_single_expression "list[str]");
      cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 4 };
      use_postcondition_info = false;
    }
    None;
  assert_resolved_definition
    ~source:{|
      class Foo:
        def bar(self) -> None:
            print(self.foo())
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$self")));
      cfg_data = { define_name = !&"test.Foo.bar"; node_id = 4; statement_index = 0 };
      use_postcondition_info = false;
    }
    (Some "test:3:10-3:14");
  (* TODO(T112570623): Get the definition for `self.foo()`. *)
  assert_resolved_definition
    ~source:
      {|
      class Foo:
        def bar(self) -> None:
            print(self.foo())

        def foo(self) -> int:
          return 42
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location
             (Expression.Name
                (Name.Attribute
                   {
                     base =
                       Node.create_with_default_location
                         (Expression.Name (Name.Identifier "$parameter$self"));
                     attribute = "foo";
                     special = false;
                   })));
      cfg_data = { define_name = !&"test.Foo.bar"; node_id = 4; statement_index = 0 };
      use_postcondition_info = false;
    }
    None;
  assert_resolved_definition
    ~external_sources
    ~source:{|
        from library import Base
    |}
    {
      symbol_with_definition = Expression (parse_single_expression "library.Base");
      cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
      use_postcondition_info = false;
    }
    (Some "library:2:0-2:15");
  (* TODO(T112570623): Get the definition for `return_str()`. *)
  assert_resolved_definition
    ~source:
      {|
        def return_str() -> str:
          return "hello"

        def foo() -> None:
          return_str().capitalize().lower()
    |}
    {
      symbol_with_definition = Expression (parse_single_expression "(test.return_str()).capitalize");
      cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
      use_postcondition_info = false;
    }
    None;
  (* TODO(T112570623): Get the definition for `Y().foo().bar()`. *)
  assert_resolved_definition
    ~source:
      {|
        class Foo:
          def bar(self) -> None:
              pass

        class Bar:
          some_attribute: Foo = Foo()

          def foo(self) -> Foo:
              return Foo()

        def test() -> None:
          Bar().foo().bar()
    |}
    {
      symbol_with_definition = Expression (parse_single_expression "(test.Bar()).foo().bar");
      cfg_data = { define_name = !&"test.test"; node_id = 4; statement_index = 0 };
      use_postcondition_info = false;
    }
    None;
  assert_resolved_definition
    ~source:{|
        def foo() -> None:
          x = 42
          print(x)
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location
             (Expression.Name (Name.Identifier "$local_test?foo$x")));
      cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 1 };
      use_postcondition_info = false;
    }
    (Some "test:3:2-3:3");
  assert_resolved_definition
    ~source:
      {|
        def foo() -> None:
          with open() as f:
            f.readline()
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location (Expression.Name (Name.Identifier "$target$f")));
      cfg_data = { define_name = !&"test.foo"; node_id = 5; statement_index = 1 };
      use_postcondition_info = false;
    }
    (Some "test:3:17-3:18");
  assert_resolved_definition
    ~source:{|
        def foo(x: str) -> None:
          for x in [1]:
            print(x)
    |}
    {
      symbol_with_definition =
        Expression
          (Node.create_with_default_location (Expression.Name (Name.Identifier "$target$x")));
      cfg_data = { define_name = !&"test.foo"; node_id = 6; statement_index = 1 };
      use_postcondition_info = false;
    }
    (Some "test:3:6-3:7");
  ()


(* Annotations *)

let test_lookup_attributes context =
  let assert_annotation_original = assert_annotation in
  let source =
    {|
      class A():
          x: int = 12
          def __init__(self, i: int) -> None:
              self.x = i

      def foo() -> int:
          a = A(100)
          return a.x
    |}
  in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation_original ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:6-2:7/typing.Type[test.A]";
      "3:4-3:5/int";
      "3:7-3:10/typing.Type[int]";
      "3:13-3:15/typing_extensions.Literal[12]";
      "4:8-4:16/typing.Callable(test.A.__init__)[[Named(self, test.A), Named(i, int)], None]";
      "4:17-4:21/test.A";
      "4:23-4:24/int";
      "4:26-4:29/typing.Type[int]";
      "4:34-4:38/None";
      "5:8-5:12/test.A";
      "5:8-5:14/int";
      "5:17-5:18/int";
      "7:4-7:7/typing.Callable(test.foo)[[], int]";
      "7:13-7:16/typing.Type[int]";
      "8:4-8:5/test.A";
      (* This is the annotation for `A()` (the function call). *)
      "8:8-8:9/typing.Type[test.A]";
      "8:8-8:14/test.A";
      "8:10-8:13/typing_extensions.Literal[100]";
      "9:11-9:12/test.A";
      "9:11-9:14/int";
    ];
  assert_annotation ~position:{ Location.line = 5; column = 8 } ~annotation:(Some "5:8-5:12/test.A");
  assert_annotation ~position:{ Location.line = 5; column = 13 } ~annotation:(Some "5:8-5:14/int");
  assert_annotation ~position:{ Location.line = 5; column = 14 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 5; column = 17 } ~annotation:(Some "5:17-5:18/int");
  assert_annotation
    ~position:{ Location.line = 9; column = 11 }
    ~annotation:(Some "9:11-9:12/test.A");
  assert_annotation ~position:{ Location.line = 9; column = 12 } ~annotation:(Some "9:11-9:14/int");
  assert_annotation ~position:{ Location.line = 9; column = 13 } ~annotation:(Some "9:11-9:14/int");

  let source =
    {|
      def foo() -> None:
          if not (unknown.
              multiline.
              access):
              pass

      class A():
          x: int = 12

      def bar() -> int:
          a = A()
          return (a.
                  x)

      def with_blanks() -> int:
          return (A().

                  x)
    |}
  in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation_original ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:7-5:15/bool";
      "8:6-8:7/typing.Type[test.A]";
      "9:4-9:5/int";
      "9:7-9:10/typing.Type[int]";
      "9:13-9:15/typing_extensions.Literal[12]";
      "11:4-11:7/typing.Callable(test.bar)[[], int]";
      "11:13-11:16/typing.Type[int]";
      "12:4-12:5/test.A";
      "12:8-12:9/typing.Type[test.A]";
      "12:8-12:11/test.A";
      "13:12-13:13/test.A";
      "13:12-14:13/int";
      "16:4-16:15/typing.Callable(test.with_blanks)[[], int]";
      "16:21-16:24/typing.Type[int]";
      "17:12-17:13/typing.Type[test.A]";
      "17:12-17:15/test.A";
      "17:12-19:13/int";
    ];
  assert_annotation ~position:{ Location.line = 3; column = 7 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 3; column = 14 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 3; column = 15 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 4; column = 8 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 5; column = 8 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation
    ~position:{ Location.line = 13; column = 12 }
    ~annotation:(Some "13:12-13:13/test.A");
  assert_annotation
    ~position:{ Location.line = 13; column = 13 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation
    ~position:{ Location.line = 14; column = 4 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation
    ~position:{ Location.line = 14; column = 12 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation ~position:{ Location.line = 14; column = 13 } ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 18; column = 0 }
    ~annotation:(Some "17:12-19:13/int")


let test_lookup_assign context =
  let source = {|
      def foo():
        x = 1
        x = str(x)
    |} in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], typing.Any]";
      "3:2-3:3/typing_extensions.Literal[1]";
      "3:6-3:7/typing_extensions.Literal[1]";
      "4:2-4:3/str";
      "4:6-4:9/typing.Type[str]";
      "4:6-4:12/str";
      "4:10-4:11/typing_extensions.Literal[1]";
    ]


let test_lookup_call_arguments context =
  let source =
    {|
      foo(12,
          argname="argval",
          multiline=
          "nextline")
    |}
  in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:0-5:15/typing.Any";
      "2:4-2:6/typing_extensions.Literal[12]";
      "3:4-3:20/typing_extensions.Literal['argval']";
      "3:12-3:20/typing_extensions.Literal['argval']";
      "4:4-5:14/typing_extensions.Literal['nextline']";
      "5:4-5:14/typing_extensions.Literal['nextline']";
    ];
  assert_annotation
    ~position:{ Location.line = 2; column = 4 }
    ~annotation:(Some "2:4-2:6/typing_extensions.Literal[12]");
  assert_annotation
    ~position:{ Location.line = 2; column = 6 }
    ~annotation:(Some "2:0-5:15/typing.Any");
  assert_annotation
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:(Some "2:0-5:15/typing.Any");
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "3:4-3:20/typing_extensions.Literal['argval']");
  assert_annotation
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:(Some "3:4-3:20/typing_extensions.Literal['argval']");
  assert_annotation
    ~position:{ Location.line = 3; column = 19 }
    ~annotation:(Some "3:12-3:20/typing_extensions.Literal['argval']");
  assert_annotation
    ~position:{ Location.line = 3; column = 20 }
    ~annotation:(Some "2:0-5:15/typing.Any");
  assert_annotation
    ~position:{ Location.line = 4; column = 3 }
    ~annotation:(Some "2:0-5:15/typing.Any");
  assert_annotation
    ~position:{ Location.line = 4; column = 4 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 4; column = 13 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 4; column = 14 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 3 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "5:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 14 }
    ~annotation:(Some "2:0-5:15/typing.Any")


let test_lookup_class_attributes context =
  let source = {|
      class Foo():
          b: bool
    |} in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:6-2:9/typing.Type[test.Foo]";
      "3:4-3:5/bool";
      "3:7-3:11/typing.Type[bool]";
      "3:11-3:11/typing.Any";
    ];
  assert_annotation ~position:{ Location.line = 3; column = 3 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 3; column = 4 } ~annotation:(Some "3:4-3:5/bool");
  assert_annotation ~position:{ Location.line = 3; column = 5 } ~annotation:None


let test_lookup_comprehensions context =
  let source = {|
       def foo() -> None:
         a = [x for x in [1.0]]
    |} in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing.List[float]";
      "3:6-3:24/typing.List[float]";
      "3:7-3:8/float";
      "3:13-3:14/float";
      "3:18-3:23/typing.List[float]";
      "3:19-3:22/float";
    ];
  let source =
    {|
       class Foo:
         def __init__(self, x: int) -> None:
           pass

       def foo() -> None:
         a = [Foo(x) for x in [1]]
    |}
  in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:6-2:9/typing.Type[test.Foo]";
      "3:6-3:14/typing.Callable(test.Foo.__init__)[[Named(self, test.Foo), Named(x, int)], None]";
      "3:15-3:19/test.Foo";
      "3:21-3:22/int";
      "3:24-3:27/typing.Type[int]";
      "3:32-3:36/None";
      "6:4-6:7/typing.Callable(test.foo)[[], None]";
      "6:13-6:17/None";
      "7:2-7:3/typing.List[test.Foo]";
      "7:6-7:27/typing.List[test.Foo]";
      "7:7-7:10/typing.Type[test.Foo]";
      "7:7-7:13/test.Foo";
      (* TODO(T60237096): "7:11-7:12/int" should be a valid lookup. *)
      "7:18-7:19/int";
      "7:23-7:26/typing.List[int]";
      "7:24-7:25/typing_extensions.Literal[1]";
    ];
  let source = {|
       def foo() -> None:
         x = 1
         a = [x for x in [1.0]]
    |} in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing_extensions.Literal[1]";
      "3:6-3:7/typing_extensions.Literal[1]";
      "4:2-4:3/typing.List[float]";
      "4:6-4:24/typing.List[float]";
      "4:7-4:8/float";
      "4:13-4:14/float";
      "4:18-4:23/typing.List[float]";
      "4:19-4:22/float";
    ];
  let source = {|
       def foo() -> None:
         a = [a for a in [None, 1.0] if a]
    |} in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing.List[float]";
      "3:6-3:35/typing.List[float]";
      "3:7-3:8/float";
      "3:13-3:14/float";
      "3:18-3:29/typing.List[typing.Optional[float]]";
      "3:19-3:23/None";
      "3:25-3:28/float";
      "3:33-3:34/typing.Optional[float]";
    ];
  let source =
    {|
       def foo() -> None:
         a = [(a, b) for a in [1.0, 2.0] for b in [1, 2]]
    |}
  in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing.List[typing.Tuple[float, int]]";
      "3:6-3:50/typing.List[typing.Tuple[float, int]]";
      "3:7-3:13/typing.Tuple[float, int]";
      "3:18-3:19/float";
      "3:23-3:33/typing.List[float]";
      "3:24-3:27/float";
      "3:29-3:32/float";
      "3:38-3:39/int";
      "3:43-3:49/typing.List[int]";
      "3:44-3:45/typing_extensions.Literal[1]";
      "3:47-3:48/typing_extensions.Literal[2]";
    ];
  let source = {|
       def foo() -> None:
         x = 1
         a = {x for x in [1.0]}
    |} in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing_extensions.Literal[1]";
      "3:6-3:7/typing_extensions.Literal[1]";
      "4:2-4:3/typing.Set[float]";
      "4:6-4:24/typing.Set[float]";
      "4:7-4:8/float";
      "4:13-4:14/float";
      "4:18-4:23/typing.List[float]";
      "4:19-4:22/float";
    ];
  let source =
    {|
       def foo() -> None:
         a = 1
         b = 1
         c = {a: b for a in [1.0]}
    |}
  in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing_extensions.Literal[1]";
      "3:6-3:7/typing_extensions.Literal[1]";
      "4:2-4:3/typing_extensions.Literal[1]";
      "4:6-4:7/typing_extensions.Literal[1]";
      "5:2-5:3/typing.Dict[float, int]";
      "5:6-5:27/typing.Dict[float, int]";
      "5:7-5:8/float";
      "5:10-5:11/typing_extensions.Literal[1]";
      "5:16-5:17/float";
      "5:21-5:26/typing.List[float]";
      "5:22-5:25/float";
    ];
  let source =
    {|
       def foo() -> None:
         a = 1
         b = 1
         c = {a: b for a in [1.0] for b in [1, 2]}
    |}
  in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:2-3:3/typing_extensions.Literal[1]";
      "3:6-3:7/typing_extensions.Literal[1]";
      "4:2-4:3/typing_extensions.Literal[1]";
      "4:6-4:7/typing_extensions.Literal[1]";
      "5:2-5:3/typing.Dict[float, int]";
      "5:6-5:43/typing.Dict[float, int]";
      "5:7-5:8/float";
      "5:10-5:11/int";
      "5:16-5:17/float";
      "5:21-5:26/typing.List[float]";
      "5:22-5:25/float";
      "5:31-5:32/int";
      "5:36-5:42/typing.List[int]";
      "5:37-5:38/typing_extensions.Literal[1]";
      "5:40-5:41/typing_extensions.Literal[2]";
    ]


let test_lookup_if_statements context =
  let source =
    {|
      def foo(flag: bool, list: typing.List[int]) -> None:
          if flag:
              pass
          if not flag:
              pass
          if list:
              pass
          if not list:
              pass
    |}
  in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:4-2:7/typing.Callable(test.foo)[[Named(flag, bool), Named(list, typing.List[int])], None]";
      "2:8-2:12/bool";
      "2:14-2:18/typing.Type[bool]";
      "2:20-2:24/typing.List[int]";
      "2:26-2:42/typing.Type[typing.List[int]]";
      "2:47-2:51/None";
      "3:7-3:11/bool";
      "5:7-5:15/bool";
      "5:11-5:15/bool";
      "7:7-7:11/typing.List[int]";
      "9:7-9:15/bool";
      "9:11-9:15/typing.List[int]";
    ]


let test_lookup_imports context =
  let source = {|
      from typing import List as l
    |} in
  assert_annotation_list ~lookup:(generate_lookup ~context source) ["2:19-2:28/typing.Type[list]"];

  let source =
    {|
      from unittest.mock import Mock
      from subprocess import call as my_call
    |}
  in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:26-2:30/typing.Type[unittest.mock.Mock]";
      "3:23-3:38/typing.Callable(subprocess.call)[[Named(command, unknown), Named(shell, \
       unknown)], typing.Any]";
    ];

  (* Wildcard Imports *)
  let source = {|
      from environment import *
    |} in
  let environment_source = {|
      class Foo: pass
      class Bar: pass
    |} in
  assert_annotation_list
    ~lookup:
      (generate_lookup ~context ~environment_sources:["environment.py", environment_source] source)
    []


let test_lookup_string_annotations context =
  let source =
    {|
      def foo(
         x: "int",
         y: "str",
      ) -> None:
          pass
    |}
  in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:4-2:7/typing.Callable(test.foo)[[Named(x, int), Named(y, str)], None]";
      "3:3-3:4/int";
      "3:6-3:11/typing.Type[int]";
      "4:3-4:4/str";
      "4:6-4:11/typing.Type[str]";
      "5:5-5:9/None";
    ];
  assert_annotation ~position:{ Location.line = 3; column = 3 } ~annotation:(Some "3:3-3:4/int");
  assert_annotation
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 7 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 10 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation ~position:{ Location.line = 3; column = 11 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 4; column = 3 } ~annotation:(Some "4:3-4:4/str");
  assert_annotation
    ~position:{ Location.line = 4; column = 6 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation
    ~position:{ Location.line = 4; column = 10 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation ~position:{ Location.line = 4; column = 11 } ~annotation:None


let test_lookup_unbound context =
  let source =
    {|
      def foo(list: typing.List[_T]) -> None:
        a = [x for x in []]
        b = (a[0] if a else a[1])
        c = identity
        d = list
    |}
  in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:4-2:7/typing.Callable(test.foo)[[Named(list, typing.List[Variable[_T]])], None]";
      "2:8-2:12/typing.List[Variable[_T]]";
      "2:14-2:29/typing.Type[typing.List[Variable[_T]]]";
      "2:34-2:38/None";
      "3:2-3:3/typing.List[typing.Any]";
      "3:6-3:21/typing.List[typing.Any]";
      "3:7-3:8/typing.Any";
      "3:13-3:14/typing.Any";
      "3:18-3:20/typing.List[Variable[_T]]";
      "4:2-4:3/typing.Any";
      "4:7-4:8/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
       typing.List[typing.Any]), Named(i, int)], typing.Any][[Named(self, \
       typing.List[typing.Any]), Named(s, slice)], typing.List[typing.Any]]], \
       typing.List[typing.Any]]";
      "4:7-4:11/typing.Any";
      "4:7-4:26/typing.Any";
      "4:9-4:10/typing_extensions.Literal[0]";
      "4:15-4:16/typing.List[typing.Any]";
      "4:22-4:23/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
       typing.List[typing.Any]), Named(i, int)], typing.Any][[Named(self, \
       typing.List[typing.Any]), Named(s, slice)], typing.List[typing.Any]]], \
       typing.List[typing.Any]]";
      "4:22-4:26/typing.Any";
      "4:24-4:25/typing_extensions.Literal[1]";
      "5:2-5:3/typing.Callable(identity)[[Named(x, Variable[_T])], Variable[_T]]";
      "5:6-5:14/typing.Callable(identity)[[Named(x, Variable[_T])], Variable[_T]]";
      "6:2-6:3/typing.List[Variable[_T]]";
      "6:6-6:10/typing.List[Variable[_T]]";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "3:6-3:21/typing.List[typing.Any]");
  assert_annotation
    ~position:{ Location.line = 3; column = 18 }
    ~annotation:(Some "3:18-3:20/typing.List[Variable[_T]]");
  assert_annotation
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:
      (Some
         "4:7-4:8/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
          typing.List[typing.Any]), Named(i, int)], typing.Any][[Named(self, \
          typing.List[typing.Any]), Named(s, slice)], typing.List[typing.Any]]], \
          typing.List[typing.Any]]");
  assert_annotation
    ~position:{ Location.line = 4; column = 22 }
    ~annotation:
      (Some
         "4:22-4:23/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
          typing.List[typing.Any]), Named(i, int)], typing.Any][[Named(self, \
          typing.List[typing.Any]), Named(s, slice)], typing.List[typing.Any]]], \
          typing.List[typing.Any]]");
  ()


let test_lookup_union_type_resolution context =
  let source =
    {|
      class A():
          pass

      class B():
          pass

      class C():
          pass

      def foo(condition):
          if condition:
              f = A()
          elif condition > 1:
              f = B()
          else:
              f = C()

          return f
    |}
  in
  assert_annotation
    ~lookup:(generate_lookup ~context source)
    ~position:{ Location.line = 19; column = 11 }
    ~annotation:(Some "19:11-19:12/typing.Union[test.A, test.B, test.C]")


let test_lookup_unknown_accesses context =
  let source = {|
      def foo() -> None:
          arbitrary["key"] = value
    |} in
  let lookup = generate_lookup ~context source in
  let assert_annotation = assert_annotation ~lookup in
  assert_annotation_list
    ~lookup
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/None";
      "3:4-3:28/typing.Any";
      "3:14-3:19/typing_extensions.Literal['key']";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "3:4-3:28/typing.Any");
  assert_annotation
    ~position:{ Location.line = 3; column = 23 }
    ~annotation:(Some "3:4-3:28/typing.Any")


let () =
  "lookup"
  >::: [
         "lookup_out_of_bounds_location" >:: test_lookup_out_of_bounds_location;
         "lookup_pick_narrowest" >:: test_lookup_pick_narrowest;
         "narrowest_match" >:: test_narrowest_match;
         "find_narrowest_spanning_symbol" >:: test_find_narrowest_spanning_symbol;
         "resolve_definition_for_symbol" >:: test_resolve_definition_for_symbol;
         "lookup_attributes" >:: test_lookup_attributes;
         "lookup_assign" >:: test_lookup_assign;
         "lookup_call_arguments" >:: test_lookup_call_arguments;
         "lookup_class_attributes" >:: test_lookup_class_attributes;
         "lookup_comprehensions" >:: test_lookup_comprehensions;
         "lookup_if_statements" >:: test_lookup_if_statements;
         "lookup_imports" >:: test_lookup_imports;
         "lookup_string_annotations" >:: test_lookup_string_annotations;
         "lookup_unbound" >:: test_lookup_unbound;
         "lookup_union_type_resolution" >:: test_lookup_union_type_resolution;
         "lookup_unknown_accesses" >:: test_lookup_unknown_accesses;
       ]
  |> Test.run
