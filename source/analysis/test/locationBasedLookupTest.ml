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

(* Find position of the indicator in [source].

   `# ^- <indicator>` refers to the position on the previous line. *)
let find_indicator_position ~source indicator_name =
  let extract_indicator_position line_number line =
    match String.substr_index line ~pattern:("^- " ^ indicator_name) with
    | Some column -> Some { Location.line = line_number; column }
    | _ -> None
  in
  let indicator_position =
    trim_extra_indentation source
    |> String.split_lines
    |> List.find_mapi ~f:extract_indicator_position
  in
  Option.value_exn
    ~message:(Format.asprintf "Expected a comment with an arrow (`^- %s`)" indicator_name)
    indicator_position


let find_indicated_multi_line_range source =
  let find_start line_number line =
    match String.substr_index line ~pattern:"start line" with
    | Some _ ->
        (* The indicator points to the start of the current line. *)
        let start_column = String.lfindi line ~f:(fun _ character -> character != ' ') in
        Some { Location.line = line_number + 1; column = Option.value_exn start_column }
    | None -> None
  in
  let find_stop line_number line =
    match String.substr_index line ~pattern:"stop line" with
    | Some _ ->
        (* The indicator points to the end of the current line. *)
        let stop_column =
          String.chop_suffix_exn ~suffix:"# stop line" line |> String.rstrip |> String.length
        in
        Some { Location.line = line_number + 1; column = stop_column }
    | _ -> None
  in
  let lines = trim_extra_indentation source |> String.split_lines in
  Option.both (List.find_mapi lines ~f:find_start) (List.find_mapi lines ~f:find_stop)
  >>| fun (start, stop) -> { Location.start; stop }


let find_indicated_single_line_range source =
  let extract_indicator_range line_number line =
    match String.substr_index_all line ~pattern:"^" ~may_overlap:false with
    | [start; stop] ->
        Some
          {
            Location.start = { Location.line = line_number; column = start };
            stop = { Location.line = line_number; column = stop };
          }
    | _ -> None
  in
  trim_extra_indentation source |> String.split_lines |> List.find_mapi ~f:extract_indicator_range


let show_location { Location.start; stop } =
  Format.asprintf "%a-%a" Location.pp_position start Location.pp_position stop


let generate_lookup ~context ?(environment_sources = []) source =
  let environment =
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] ~external_sources:environment_sources
      |> ScratchProject.build_type_environment
    in
    type_environment
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
    (LocationBasedLookup.get_all_nodes_and_coverage_data lookup
    |> List.map ~f:(fun (location, { LocationBasedLookup.type_; expression = _ }) ->
           location, type_)
    |> List.sort ~compare:[%compare: Location.t * Type.t]
    |> List.map ~f:(fun (key, data) -> Format.asprintf "%s/%a" (show_location key) Type.pp data))


let assert_annotation ~lookup ~position ~annotation =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    annotation
    (LocationBasedLookup.get_coverage_data lookup ~position
    >>| fun (location, { LocationBasedLookup.type_; _ }) ->
    Format.asprintf "%s/%a" (show_location location) Type.pp type_)


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
    LocationBasedLookup.get_coverage_data lookup ~position:{ Location.line; column } |> ignore
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
      "2:54-2:58/typing.Type[None]";
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
    let narrowest_matching_expression expressions =
      expressions
      |> List.map ~f:(fun (expression, location) ->
             {
               symbol_with_definition =
                 Expression
                   {
                     (parse_single_expression ~coerce_special_methods:true expression) with
                     location = parse_location location;
                   };
               cfg_data = { define_name = !&"test.foo"; node_id = 0; statement_index = 0 };
               use_postcondition_info = false;
             })
      |> narrowest_match
      >>| function
      | { symbol_with_definition = Expression expression; _ } -> expression
      | _ -> failwith "Expected expression"
    in
    let assert_narrowest_matching_expression expressions =
      assert_equal
        ~cmp:(fun left right ->
          match left, right with
          | Some left, Some right -> location_insensitive_compare left right = 0
          | None, None -> true
          | _ -> false)
        ~printer:[%show: Expression.t option]
        (expected >>| parse_single_expression)
        (narrowest_matching_expression expressions)
    in
    assert_narrowest_matching_expression expressions;
    (* The narrowest match should be insensitive to the list order. *)
    assert_narrowest_matching_expression (List.rev expressions)
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
  (* Pick `my_dictionary` over `my_dictionary.__getitem__` even though they have the same "location"
     range. *)
  assert_narrowest
    [
      "my_dictionary['foo']", "5:2-5:24";
      "my_dictionary.__getitem__", "5:2-5:15";
      "my_dictionary", "5:2-5:15";
    ]
    (Some "my_dictionary");
  (* For if-statements, the assertion `if foo.bar:` is desugared into two assert statements:

     - one for the if-branch: `assert foo.bar`

     - one for the else-branch: `assert (not foo.bar)`.

     These assertions have the same location range. Pick the former as the narrowest spanning
     symbol. Otherwise, we would end up looking for the definition of the non-reference `not
     foo.bar`, and fail. *)
  assert_narrowest ["not foo.bar", "5:2-5:24"; "foo.bar", "5:2-5:24"] (Some "foo.bar");
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
  let assert_narrowest_expression ?(external_sources = []) ~source expected =
    let type_environment =
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] ~external_sources
        |> ScratchProject.build_type_environment
      in
      type_environment
    in
    assert_equal
      ~cmp:(fun left right ->
        Option.compare location_insensitive_compare_symbol_and_cfg_data left right = 0)
      ~printer:[%show: symbol_and_cfg_data option]
      expected
      (LocationBasedLookup.find_narrowest_spanning_symbol
         ~type_environment
         ~module_reference:!&"test"
         (find_indicator_position ~source "cursor"))
  in
  assert_narrowest_expression
    ~source:{|
        def getint() -> int:
        #   ^- cursor
          return 12
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "test.getint");
         cfg_data = { define_name = !&"test.getint"; node_id = 0; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def getint() -> int:
        #  ^- cursor
          return 12
    |}
    None;
  assert_narrowest_expression
    ~source:
      {|
        def getint() -> int:
        #               ^- cursor
          return 12
    |}
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
        #                   ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library.Base");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from .library import Base as MyBase
                                    # ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library.Base");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        import library
        #        ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from . import library as my_library
                                    # ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from . import library as my_library

        x = my_library.Base()
        #    ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "library");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources:["a.py", {| |}; "b.py", {| import a as x |}]
    ~source:{|
        from b import x
        print(x)
           #  ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "b.x");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources:["a.py", {| |}; "b.py", {| import a as x |}]
    ~source:{|
        from b import x as y
        print(y)
           #  ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "b.x");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources:["a.py", {| class Foo: ... |}; "b.py", {| import a as x |}]
    ~source:{|
        from b import x as y
        print(y.Foo)
              #  ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "b.x.Foo");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources:["foo/a.py", {| class Foo: ... |}; "b.py", {| import foo.a as x |}]
    ~source:{|
        from b import x as y
        print(y)
           #  ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "b.x");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources:["my_placeholder_stub.pyi", {| # pyre-placeholder-stub |}]
    ~source:
      {|
        import my_placeholder_stub
        print(my_placeholder_stub)
           #  ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "my_placeholder_stub");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~external_sources:["my_placeholder_stub.pyi", {| # pyre-placeholder-stub |}]
    ~source:
      {|
        from my_placeholder_stub import x as y
        print(y.Foo)
              #  ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "my_placeholder_stub.x.Foo");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        def foo(a: int, b: str) -> None: ...
        #               ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$b")));
         cfg_data = { define_name = !&"test.foo"; node_id = 0; statement_index = 0 };
         use_postcondition_info = true;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo(a: int, b: str) -> None:
          x = a
        #     ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$a")));
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
        # ^- cursor
    |}
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
    ~source:
      {|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
        #                 ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression {|["a", "b"]|});
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  (* This is a judgment call. We could either show `list` or `list[str]`.

     Doing the latter - always returning the entire annotation - would mean that go-to-def would
     break for cases where we want to get the precise element type, e.g., `dict[int, Foo]` would
     jump to `dict` instead of `Foo`. So, we will go with the closest symbol within the annotation
     that spans the cursor position.

     Given that `hover` on a type annotation isn't as valuable as go-to-def, this should be a
     reasonable tradeoff. *)
  assert_narrowest_expression
    ~source:
      {|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
        #     ^- cursor
    |}
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "list");
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
        #       ^- cursor
    |}
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
        #          ^- cursor
    |}
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
        #              ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression (parse_single_expression "(test.return_str()).capitalize");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
      class Foo:
        def bar(self) -> None:
            print(self.foo())
        #         ^- cursor
    |}
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
        #              ^- cursor
        def foo(self) -> int:
          return 42
    |}
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
        #             ^- cursor
    |}
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

        Bar().some_attribute
        #             ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "test.Bar().some_attribute");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 2 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo: ...

        class Bar:
          some_attribute: Foo = Foo()
        #                 ^- cursor
    |}
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
        #             ^- cursor
    |}
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
        #       ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$x")));
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo() -> None:
          x = 42
          print(x)
        #       ^- cursor
    |}
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
        #   ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$target$f")));
         cfg_data = { define_name = !&"test.foo"; node_id = 5; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo() -> None:
          for x in [1]:
            print(x)
        #         ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$target$x")));
         cfg_data = { define_name = !&"test.foo"; node_id = 6; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo:
          @staticmethod
          def my_static_method() -> None: ...

        def foo() -> None:
          Foo.my_static_method()
        #         ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "(test.Foo).my_static_method");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo:
          @classmethod
          def my_class_method(cls) -> None: ...

        def foo() -> None:
          Foo.my_class_method()
        #         ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "(test.Foo).my_class_method");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo:
          def my_method() -> None: ...

        def foo() -> None:
          Foo.my_method()
        #         ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "(test.Foo).my_method");
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from typing import Dict

        def foo(my_dictionary: Dict[str, int]) -> None:
          my_dictionary["hello"]
        #         ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Name (Name.Identifier "$parameter$my_dictionary")
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from typing import Dict

        def foo(my_dictionary: Dict[str, int]) -> None:
          my_dictionary.__getitem__("hello")
        #                 ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Name
                (Name.Attribute
                   {
                     base =
                       Node.create_with_default_location
                         (Expression.Name (Name.Identifier "$parameter$my_dictionary"));
                     attribute = "__getitem__";
                     special = false;
                   })
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  (* When the cursor is on the square bracket, we return the entire expression. *)
  assert_narrowest_expression
    ~source:
      {|
        from typing import Dict

        def foo(my_dictionary: Dict[str, int]) -> None:
          my_dictionary["hello"]
        #              ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Call
                {
                  callee =
                    Expression.Name
                      (Name.Attribute
                         {
                           base =
                             Node.create_with_default_location
                               (Expression.Name (Name.Identifier "$parameter$my_dictionary"));
                           attribute = "__getitem__";
                           special = true;
                         })
                    |> Node.create_with_default_location;
                  arguments =
                    [
                      {
                        name = None;
                        value =
                          Expression.Constant (String { value = "hello"; kind = String })
                          |> Node.create_with_default_location;
                      };
                    ];
                }
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int

        def main(foo: Foo) -> None:
          print(foo.my_attribute)
        #              ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Name
                (Name.Attribute
                   {
                     base =
                       Node.create_with_default_location
                         (Expression.Name (Name.Identifier "$parameter$foo"));
                     attribute = "my_attribute";
                     special = false;
                   })
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.main"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int

        def main(foo: Foo) -> None:
          if foo.my_attribute:
            #        ^- cursor
            print("hello")
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Name
                (Name.Attribute
                   {
                     base =
                       Node.create_with_default_location
                         (Expression.Name (Name.Identifier "$parameter$foo"));
                     attribute = "my_attribute";
                     special = false;
                   })
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.main"; node_id = 7; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int
          other_attribute: str

        def main(foo: Foo) -> None:
          if foo.my_attribute and not foo.other_attribute:
            #                               ^- cursor
            print("hello")
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Name
                (Name.Attribute
                   {
                     base =
                       Node.create_with_default_location
                         (Expression.Name (Name.Identifier "$parameter$foo"));
                     attribute = "other_attribute";
                     special = false;
                   })
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.main"; node_id = 7; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def getint(xs: list[int]) -> None:
          for x in xs:
            #      ^- cursor
            pass
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$xs")));
         cfg_data = { define_name = !&"test.getint"; node_id = 6; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo(xs: list[int]) -> None:
          print(f"xs: {xs}")
          #            ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Node.create_with_default_location (Expression.Name (Name.Identifier "$parameter$xs")));
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        def foo(xs: list[int]) -> None:
          print(f"xs: {xs.append(xs)}")
          #                  ^- cursor
    |}
    (Some
       {
         symbol_with_definition =
           Expression
             (Expression.Name
                (Name.Attribute
                   {
                     base =
                       Node.create_with_default_location
                         (Expression.Name (Name.Identifier "$parameter$xs"));
                     attribute = "append";
                     special = false;
                   })
             |> Node.create_with_default_location);
         cfg_data = { define_name = !&"test.foo"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        from typing import Callable

        f: Callable
        #   ^- cursor
    |}
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "typing.Callable");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        from typing import Callable

        f: Callable
        #   ^- cursor
    |}
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "typing.Callable");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:
      {|
        class Foo:
          def my_method(self, x: "Foo") -> None: ...
            #                      ^- cursor
    |}
    (Some
       {
         symbol_with_definition = TypeAnnotation (parse_single_expression "test.Foo");
         cfg_data = { define_name = !&"test.Foo.my_method"; node_id = 0; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        from typing import Callable
        #                    ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "typing.Callable");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        from typing import Callable
        #        ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "typing");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 0 };
         use_postcondition_info = false;
       });
  assert_narrowest_expression
    ~source:{|
        import foo
        foo(abc=5)
        #     ^- cursor
    |}
    (Some
       {
         symbol_with_definition = Expression (parse_single_expression "foo");
         cfg_data = { define_name = !&"test.$toplevel"; node_id = 4; statement_index = 1 };
         use_postcondition_info = false;
       });
  (* TODO(T151907213) add closed-over variables to lookups in inner function scope *)
  assert_narrowest_expression
    ~source:
      {|
      def foo() -> None:
        a = 5
        def bar() -> None:
          print(a)
     # .        ^- cursor
  |}
    None;
  ()


let test_resolve_definition_for_symbol context =
  let default_external_sources =
    [
      ( "library.py",
        {|
      class Base: ...

      def return_str() -> str:
          return "hello"
      def contains_kw_args(foo: str, **kwargs) -> str:
          return "hello"
    |}
      );
    ]
  in
  let module_reference = !&"test" in
  let assert_resolved_definition_with_location
      ?(external_sources = default_external_sources)
      ~source
      expected
    =
    let type_environment =
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] ~external_sources
        |> ScratchProject.build_type_environment
      in
      type_environment
    in
    let symbol_data =
      LocationBasedLookup.find_narrowest_spanning_symbol
        ~type_environment
        ~module_reference
        (find_indicator_position ~source "cursor")
    in
    assert_equal
      ~cmp:[%compare.equal: Location.WithModule.t option]
      ~printer:[%show: Location.WithModule.t option]
      expected
      (symbol_data
      >>= LocationBasedLookup.resolve_definition_for_symbol ~type_environment ~module_reference)
  in
  let assert_resolved_definition ?external_sources source =
    let expected_definition_location =
      if String.is_substring ~substring:"# No definition found" source then
        None
      else
        let location =
          Option.first_some
            (find_indicated_single_line_range source)
            (find_indicated_multi_line_range source)
        in
        Option.value_exn
          ~message:
            "Expected either a comment with two carets (e.g., ^____^) or comments with `start \
             line` and `stop line`"
          location
        |> Location.with_module ~module_reference
        |> Option.some
    in
    assert_resolved_definition_with_location ?external_sources ~source expected_definition_location
  in
  let assert_resolved_definition_with_location_string ?external_sources ~source expected =
    assert_resolved_definition_with_location
      ?external_sources
      ~source
      (expected >>| parse_location_with_module)
  in
  assert_resolved_definition
    {|
        def getint() -> int: # start line
          #    ^- cursor

          return 42          # stop line
    |};
  assert_resolved_definition
    {|
        def foo(a: int, b: str) -> None: ...
          #             ^- cursor

        # No definition found.
    |};
  assert_resolved_definition
    {|
        def foo(x: str) -> None:
          #     ^     ^

          print(x)
          #     ^- cursor
    |};
  assert_resolved_definition_with_location_string
    ~source:
      {|
        def getint() -> int:
                      # ^- cursor
          return 42
    |}
    (* This points to builtins.pyi. *)
    (Some ":109:0-210:35");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
          #    ^- cursor

          # No definition found.
    |}
    (* This points to builtins.pyi. *)
    (Some ":392:0-415:31");
  assert_resolved_definition
    {|
      class Foo:
        def bar(self) -> None:
          #     ^   ^

            print(self.foo())
            #      ^- cursor
    |};
  assert_resolved_definition
    {|
      class Foo:
        def bar(self) -> None:
            print(self.foo())
            #           ^- cursor


        def foo(self) -> int:  # start line
          return 42            # stop line

    |};
  assert_resolved_definition_with_location_string
    ~source:{|
        from library import Base
                           # ^- cursor
    |}
    (Some "library:2:0-2:15");
  assert_resolved_definition_with_location_string
    ~source:{|
        import library
        #        ^- cursor
    |}
    (Some "library:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from . import library as my_library
                                    # ^- cursor
    |}
    (Some "library:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from . import library as my_library

        x = my_library.Base()
        #    ^- cursor
    |}
    (Some "library:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        def return_str() -> str:
          return "hello"

        def foo() -> None:
          return_str().capitalize().lower()
                     # ^- cursor
    |}
    (* This points to builtins.pyi. *)
    (Some ":333:2-333:34");
  assert_resolved_definition
    {|
        class Foo:
          def bar(self) -> None:  # start line
              pass                # stop line

        class Bar:
          some_attribute: Foo = Foo()

          def foo(self) -> Foo:
              return Foo()

        def test() -> None:
          Bar().foo().bar()
          #           ^- cursor
    |};
  assert_resolved_definition
    {|
        class Foo: ...

        class Bar:
          some_attribute: Foo = Foo()
        # ^                          ^

        Bar().some_attribute
        #       ^- cursor
    |};
  assert_resolved_definition
    {|
        def foo() -> None:
          x = 42
        # ^^


          print(x)
          #     ^- cursor
    |};
  assert_resolved_definition
    {|
        def foo() -> None:
          with open() as f:
                      #  ^^
            f.readline()
          # ^- cursor
    |};
  assert_resolved_definition
    {|
        def foo(x: str) -> None:
          for x in [1]:
            # ^^

            print(x)
            #     ^- cursor
    |};
  assert_resolved_definition
    {|
        class Foo:
          @classmethod
          def my_class_method(cls) -> None: ...
        # ^                                    ^

        def foo() -> None:
          Foo.my_class_method()
          #    ^- cursor
    |};
  assert_resolved_definition
    {|
        class Foo:
          def my_method() -> None: ...
        # ^                           ^

        def foo() -> None:
          Foo.my_method()
          #    ^- cursor
    |};
  assert_resolved_definition
    {|
        class Foo:
          @staticmethod
          def my_static_method() -> None: ...
        # ^                                  ^

        def foo() -> None:
          Foo.my_static_method()
          #     ^- cursor
    |};
  assert_resolved_definition
    {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int
        # ^                ^

        def main(foo: Foo) -> None:
          print(foo.my_attribute)
          #           ^- cursor
    |};
  assert_resolved_definition
    {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int
        # ^                ^

        def main(foo: Foo) -> None:
          if foo.my_attribute:
            #        ^- cursor
            print("hello")
    |};
  assert_resolved_definition
    {|
        def getint(xs: list[int]) -> None:
          #        ^            ^

          for x in xs:
            #      ^- cursor
            pass
    |};
  assert_resolved_definition
    {|
        def foo(xs: list[int]) -> None:
          #     ^            ^

          print(f"xs: {xs}")
          #            ^- cursor
    |};
  assert_resolved_definition_with_location_string
    ~source:
      {|
        def foo(xs: list[int]) -> None:
          print(f"xs: {xs.append(xs)}")
                        # ^- cursor
    |}
    (* This points to builtins.pyi. *)
    (Some ":403:2-403:46");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        try:
          print("hello")
        except Exception as exception:
          print(exception)
          #      ^- cursor
    |}
    (Some "test:4:20-5:2");
  (* We create special stubs in `MissingFromStubs` for special forms like `Callable`. They end up
     making the location be `any` (with all positions as `-1`), which the IDE doesn't recognize. We
     should translate that to something sensible, so that the IDE at least goes to the relevant
     file. *)
  assert_resolved_definition_with_location_string
    ~source:{|
        from typing import Callable

        f: Callable
        #   ^- cursor
    |}
    (Some "typing:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~external_sources:["a.py", {| |}; "b.py", {| import a as x |}]
    ~source:{|
        from b import x as y
        print(y)
           #  ^- cursor
    |}
    (Some "a:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~external_sources:["a.py", {| |}; "b.py", {| import a as x |}]
    ~source:{|
        from b import x
        print(x)
           #  ^- cursor
    |}
    (Some "a:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~external_sources:["foo/a.py", {| class Foo: ... |}; "b.py", {| import foo.a as x |}]
    ~source:{|
        from b import x as y
        print(y)
           #  ^- cursor
    |}
    (Some "foo.a:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~external_sources:["a.py", {| class Foo: ... |}; "b.py", {| import a as x |}]
    ~source:{|
        from b import x as y
        print(y.Foo)
              #  ^- cursor
    |}
    (Some "a:1:0-1:14");
  assert_resolved_definition_with_location_string
    ~external_sources:["my_placeholder_stub.pyi", {| # pyre-placeholder-stub |}]
    ~source:
      {|
        import my_placeholder_stub
        print(my_placeholder_stub)
           #  ^- cursor
    |}
    (Some "my_placeholder_stub:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~external_sources:["my_placeholder_stub.pyi", {| # pyre-placeholder-stub |}]
    ~source:
      {|
        from my_placeholder_stub import x as y
        print(y.Foo)
              #  ^- cursor
    |}
    (Some "my_placeholder_stub:1:0-1:0");
  assert_resolved_definition
    {|
        class Base:
          def base_method(self) -> None: ...
        # ^                                 ^

        class Child(Base): ...

        def foo(x: Child) -> None:
          x.base_method()
          #    ^- cursor
    |};
  assert_resolved_definition
    {|
        class Base:
          base_attribute: int
        # ^                  ^

        class Child(Base): ...

        def foo(x: Child) -> None:
          x.base_attribute
          #    ^- cursor
    |};
  assert_resolved_definition_with_location_string
    ~source:
      {|
        MY_GLOBAL = "hello"

        def main() -> int:
          MY_GLOBAL.capitalize()
          #    ^- cursor
    |}
    (Some "test:2:0-2:9");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from typing import Callable

        Foo = list[int]

        def main(x: Foo) -> None:
          #          ^- cursor
          pass
    |}
    (Some "test:4:0-4:3");
  assert_resolved_definition
    {|
        from typing import Optional

        class Foo:
          some_attribute: int
        # ^                  ^

        def main(foo: Optional[Foo]) -> None:

          if foo is not None:
            print(foo.some_attribute)
            #           ^- cursor
    |};
  (* TODO(T129228930): Handle refinement within a statement. *)
  assert_resolved_definition
    {|
        from typing import Optional

        class Foo:
          some_attribute: int

        def main(foo: Optional[Foo]) -> None:
          d = foo.some_attribute if foo is not None else None
            #           ^- cursor

        # No definition found.
    |};
  assert_resolved_definition
    {|
        from typing import Optional

        class Foo:              # start line
          some_attribute: int   # stop line

        def main(foo: Optional[Foo]) -> None:
          #                     ^- cursor
          pass
    |};
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from typing import Optional

        foo: Optional[int]
        #      ^- cursor
    |}
    (* `Optional` is a special form, so it points to the start of the file. *)
    (Some "typing:1:0-1:0");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from typing import Union

        foo: Union[int, str, bool]
        #      ^- cursor
    |}
    (* `Union` is a special form, so it points to the start of the file. *)
    (Some "typing:1:0-1:0");
  assert_resolved_definition
    {|
        from typing import Generic, TypeVar

        T = TypeVar("T")

        class MyContainer(Generic[T]):  # start line
          pass                          # stop line

        foo: MyContainer[int]
        #      ^- cursor
    |};
  assert_resolved_definition
    {|
        class Foo:                                     # start line
          def my_method(self, x: "Foo") -> None: ...   # stop line
            #                      ^- cursor
    |};
  assert_resolved_definition
    {|
        def main() -> None:
          while (x := True):
            #    ^^
              print(x)
                #   ^- cursor
    |};
  assert_resolved_definition
    {|
        from typing import Callable

        def my_decorator(f: Callable[[], int]) -> Callable[[], int]:  # start line
          pass                                                        # stop line

        @my_decorator
        #   ^- cursor
        def foo() -> int: ...
    |};
  (* TODO(T129228930): We don't handle files that are shadowed by stubs. *)
  assert_resolved_definition_with_location_string
    ~external_sources:["test.pyi", {|
        FOO: int = ...
      |}]
    ~source:{|
        FOO = 1
        # ^- cursor
    |}
    None;
  assert_resolved_definition_with_location_string
    ~external_sources:["test.pyi", {|
        FOO: int = ...
      |}]
    ~source:{|
        FOO = 1

        print(FOO)
          #    ^- cursor
    |}
    None;
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from typing import Optional
        from dataclasses import dataclass

        @dataclass
        # ^- cursor
        class Foo: ...

        # No definition found.
    |}
    (Some "dataclasses:6:0-6:46");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from library import contains_kw_args

        contains_kw_args(foo="test")
        #                 ^- cursor
      |}
    (Some "library:6:0-7:18");
  assert_resolved_definition_with_location_string
    ~source:
      {|
        from library import contains_kw_args

        contains_kw_args(kw="test")
        #                 ^- cursor
      |}
    (Some "library:6:0-7:18");
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
      "4:34-4:38/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "3:4-3:11/typing_extensions.Literal['argval']";
      "3:12-3:20/typing_extensions.Literal['argval']";
      "4:4-4:13/typing_extensions.Literal['nextline']";
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
    ~annotation:(Some "3:4-3:11/typing_extensions.Literal['argval']");
  assert_annotation
    ~position:{ Location.line = 3; column = 10 }
    ~annotation:(Some "3:4-3:11/typing_extensions.Literal['argval']");
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
    ~annotation:(Some "4:4-4:13/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 4; column = 12 }
    ~annotation:(Some "4:4-4:13/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 4 }
    ~annotation:(Some "5:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "5:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 14 }
    ~annotation:(Some "2:0-5:15/typing.Any")


let test_lookup_class_attributes context =
  let source =
    {|
      from typing import List, Iterable
      class Foo():
          b: bool
          def baz(self, x: List[Iterable[str]]) -> List[Iterable[str]]:
            return x
    |}
  in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:19-2:23/typing.Type[list]";
      "2:25-2:33/typing.Type[typing.Iterable]";
      "3:6-3:9/typing.Type[test.Foo]";
      "4:4-4:5/bool";
      "4:7-4:11/typing.Type[bool]";
      "5:8-5:11/typing.Callable(test.Foo.baz)[[Named(self, test.Foo), Named(x, \
       typing.List[typing.Iterable[str]])], typing.List[typing.Iterable[str]]]";
      "5:12-5:16/test.Foo";
      "5:18-5:19/typing.List[typing.Iterable[str]]";
      "5:21-5:40/typing.Type[typing.List[typing.Iterable[str]]]";
      "5:45-5:64/typing.Type[typing.List[typing.Iterable[str]]]";
      "6:13-6:14/typing.List[typing.Iterable[str]]";
    ];
  ()


let test_lookup_class_attributes_nested context =
  let source =
    {|
      from typing import List, Iterable
      class Foo:
        x: int = 5
        def baz(self) -> None:
          class FooTwo:
            def foo_function(self, x: List[Iterable[str]]) -> List[Iterable[str]]:
              return x
    |}
  in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:19-2:23/typing.Type[list]";
      "2:25-2:33/typing.Type[typing.Iterable]";
      "3:6-3:9/typing.Type[test.Foo]";
      "4:2-4:3/int";
      "4:5-4:8/typing.Type[int]";
      "4:11-4:12/typing_extensions.Literal[5]";
      "5:6-5:9/typing.Callable(test.Foo.baz)[[Named(self, test.Foo)], None]";
      "5:10-5:14/test.Foo";
      "5:19-5:23/typing.Type[None]";
      "6:10-6:16/typing.Type[test.Foo.baz.FooTwo]";
    ];
  ()


let test_lookup_dataclass_attributes context =
  let source =
    {|
      from dataclasses import dataclass
      @dataclass
      class Foo:
        x: int
    |}
  in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:24-2:33/typing.Callable(dataclasses.dataclass)[[Named(_cls, \
       typing.Type[Variable[dataclasses._T]])], typing.Type[Variable[dataclasses._T]]]";
      "4:6-4:9/typing.Type[test.Foo]";
      "5:2-5:3/int";
      "5:5-5:8/typing.Type[int]";
    ];
  ()


let test_lookup_comprehensions context =
  let source = {|
       def foo() -> None:
         a = [x for x in [1.0]]
    |} in
  assert_annotation_list
    ~lookup:(generate_lookup ~context source)
    [
      "2:4-2:7/typing.Callable(test.foo)[[], None]";
      "2:13-2:17/typing.Type[None]";
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
      "3:32-3:36/typing.Type[None]";
      "6:4-6:7/typing.Callable(test.foo)[[], None]";
      "6:13-6:17/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "2:13-2:17/typing.Type[None]";
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
      "2:47-2:51/typing.Type[None]";
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
      "5:5-5:9/typing.Type[None]";
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
      "2:34-2:38/typing.Type[None]";
      "3:2-3:3/typing.List[typing.Any]";
      "3:6-3:21/typing.List[typing.Any]";
      "3:7-3:8/typing.Any";
      "3:13-3:14/typing.Any";
      "3:18-3:20/typing.List[Variable[_T]]";
      "4:2-4:3/typing.Any";
      "4:7-4:8/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
       typing.List[typing.Any]), Named(index, int)], typing.Any][[Named(self, \
       typing.List[typing.Any]), Named(index, slice)], typing.List[typing.Any]]], \
       typing.List[typing.Any]]";
      "4:7-4:11/typing.Any";
      "4:7-4:26/typing.Any";
      "4:9-4:10/typing_extensions.Literal[0]";
      "4:15-4:16/typing.List[typing.Any]";
      "4:22-4:23/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
       typing.List[typing.Any]), Named(index, int)], typing.Any][[Named(self, \
       typing.List[typing.Any]), Named(index, slice)], typing.List[typing.Any]]], \
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
          typing.List[typing.Any]), Named(index, int)], typing.Any][[Named(self, \
          typing.List[typing.Any]), Named(index, slice)], typing.List[typing.Any]]], \
          typing.List[typing.Any]]");
  assert_annotation
    ~position:{ Location.line = 4; column = 22 }
    ~annotation:
      (Some
         "4:22-4:23/BoundMethod[typing.Callable(list.__getitem__)[..., unknown][[[Named(self, \
          typing.List[typing.Any]), Named(index, int)], typing.Any][[Named(self, \
          typing.List[typing.Any]), Named(index, slice)], typing.List[typing.Any]]], \
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


let test_lookup_def context =
  let source =
    {|
      from typing import List, Iterable
      def foo(x: List[Iterable[str]]) -> List[Iterable[str]]:
          return x
    |}
  in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:19-2:23/typing.Type[list]";
      "2:25-2:33/typing.Type[typing.Iterable]";
      "3:4-3:7/typing.Callable(test.foo)[[Named(x, typing.List[typing.Iterable[str]])], \
       typing.List[typing.Iterable[str]]]";
      "3:8-3:9/typing.List[typing.Iterable[str]]";
      "3:11-3:30/typing.Type[typing.List[typing.Iterable[str]]]";
      "3:35-3:54/typing.Type[typing.List[typing.Iterable[str]]]";
      "4:11-4:12/typing.List[typing.Iterable[str]]";
    ]


let test_lookup_async_def context =
  let source = {|
      async def foo(x: int) -> None:
          pass
    |} in
  let lookup = generate_lookup ~context source in
  assert_annotation_list
    ~lookup
    [
      "2:10-2:13/typing.Callable(test.foo)[[Named(x, int)], typing.Coroutine[typing.Any, \
       typing.Any, None]]";
      "2:14-2:15/int";
      "2:17-2:20/typing.Type[int]";
      "2:25-2:29/typing.Type[None]";
    ]


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
      "2:13-2:17/typing.Type[None]";
      "3:4-3:28/typing.Any";
      "3:14-3:19/typing_extensions.Literal['key']";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "3:4-3:28/typing.Any");
  assert_annotation
    ~position:{ Location.line = 3; column = 23 }
    ~annotation:(Some "3:4-3:28/typing.Any")


let test_classify_coverage_data _ =
  let parse_expression expression =
    match expression with
    | "None" -> None
    | _ -> Some (parse_single_expression expression)
  in
  let parse_type type_ = Type.create ~aliases:Type.empty_aliases (parse_single_expression type_) in
  let make_coverage_data_record ~expression type_ = { LocationBasedLookup.expression; type_ } in
  let make_coverage_gap_record ~coverage_data reason =
    Some { LocationBasedLookup.coverage_data; reason }
  in
  let make_coverage_gap ~coverage_data reason =
    match reason with
    | None -> None
    | Some current_reason -> make_coverage_gap_record ~coverage_data current_reason
  in
  let assert_coverage_gap ~coverage_data reason =
    assert_equal
      ~cmp:[%compare.equal: LocationBasedLookup.coverage_gap option]
      ~printer:[%show: LocationBasedLookup.coverage_gap option]
      (make_coverage_gap ~coverage_data reason)
      (LocationBasedLookup.classify_coverage_data coverage_data)
  in
  let assert_coverage_gap_parse_both ~expression ~type_ reason =
    let coverage_data =
      make_coverage_data_record ~expression:(parse_expression expression) (parse_type type_)
    in
    assert_coverage_gap ~coverage_data reason
  in
  let assert_coverage_gap_parse_expression ~expression ~type_ reason =
    let coverage_data = make_coverage_data_record ~expression:(parse_expression expression) type_ in
    assert_coverage_gap ~coverage_data reason
  in
  let assert_coverage_gap_parse_type ~expression ~type_ reason =
    let coverage_data = make_coverage_data_record ~expression (parse_type type_) in
    assert_coverage_gap ~coverage_data reason
  in
  assert_coverage_gap_parse_both
    ~expression:"x"
    ~type_:"typing.Any"
    (Some (LocationBasedLookup.TypeIsAny LocationBasedLookup.OtherExpressionIsAny));
  assert_coverage_gap_parse_type
    ~expression:
      (Some (Expression.Name (Name.Identifier "$parameter$x") |> Node.create_with_default_location))
    ~type_:"typing.Any"
    (Some (LocationBasedLookup.TypeIsAny LocationBasedLookup.ParameterIsAny));
  assert_coverage_gap_parse_both
    ~expression:"print(x + 1)"
    ~type_:"typing.Any"
    (Some (LocationBasedLookup.TypeIsAny LocationBasedLookup.OtherExpressionIsAny));
  assert_coverage_gap_parse_both ~expression:"1" ~type_:"typing_extensions.Literal[1]" None;
  assert_coverage_gap_parse_both
    ~expression:"x"
    ~type_:"typing.List[typing.Any]"
    (Some LocationBasedLookup.ContainerParameterIsAny);
  assert_coverage_gap_parse_both ~expression:"x" ~type_:"typing.List[float]" None;
  assert_coverage_gap_parse_both
    ~expression:"x"
    ~type_:"typing.Set[typing.Any]"
    (Some LocationBasedLookup.ContainerParameterIsAny);
  assert_coverage_gap_parse_both ~expression:"x" ~type_:"typing.Set[int]" None;
  assert_coverage_gap_parse_both
    ~expression:"x"
    ~type_:"typing.Dict[typing.Any, typing.Any]"
    (Some LocationBasedLookup.ContainerParameterIsAny);
  assert_coverage_gap_parse_both ~expression:"x" ~type_:"typing.Dict[str, typing.Any]" None;
  assert_coverage_gap_parse_both ~expression:"Foo[Any]" ~type_:"Foo[Any]" None;
  (* TODO(T123023697): We only identify coverage gaps in Callable when all the parameters are
     Unknown or Any. Task handles niche cases such as keyword-only or positional arguments*)
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:(Type.Callable.create ~annotation:Type.bytes ())
    None;
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [Type.Callable.Parameter.Named { name = "x"; annotation = Type.Any; default = false }])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [Type.Callable.Parameter.Named { name = "x"; annotation = Type.Top; default = false }])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named
                  { name = "x"; annotation = Type.integer; default = false };
              ])
         ~annotation:Type.bytes
         ())
    None;
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named { name = "x"; annotation = Type.Any; default = false };
                Type.Callable.Parameter.Named { name = "y"; annotation = Type.Any; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named { name = "x"; annotation = Type.Top; default = false };
                Type.Callable.Parameter.Named { name = "y"; annotation = Type.Top; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named { name = "x"; annotation = Type.Top; default = false };
                Type.Callable.Parameter.Named { name = "y"; annotation = Type.Any; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named { name = "x"; annotation = Type.Any; default = false };
                Type.Callable.Parameter.Named { name = "y"; annotation = Type.Any; default = false };
                Type.Callable.Parameter.Named { name = "z"; annotation = Type.Any; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named { name = "x"; annotation = Type.Top; default = false };
                Type.Callable.Parameter.Named { name = "y"; annotation = Type.Top; default = false };
                Type.Callable.Parameter.Named { name = "z"; annotation = Type.Top; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named
                  { name = "x"; annotation = Type.integer; default = false };
                Type.Callable.Parameter.Named { name = "y"; annotation = Type.Any; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:
      (Type.Callable.create
         ~parameters:
           (Type.Callable.Defined
              [
                Type.Callable.Parameter.Named { name = "x"; annotation = Type.Top; default = false };
                Type.Callable.Parameter.Named
                  { name = "y"; annotation = Type.integer; default = false };
              ])
         ~annotation:Type.bytes
         ())
    (Some LocationBasedLookup.CallableParameterIsUnknownOrAny);
  assert_coverage_gap_parse_expression
    ~expression:"foo"
    ~type_:(Type.Callable.create ~parameters:(Type.Callable.Defined []) ~annotation:Type.bytes ())
    None;
  ()


let test_lookup_expression context =
  let assert_coverage_data ~context ~source expected_coverage_gap_list =
    let coverage_data_lookup = generate_lookup ~context source in
    let lookup_list = LocationBasedLookup.get_all_nodes_and_coverage_data coverage_data_lookup in
    let coverage_data_list = List.map ~f:(fun (_, coverage_data) -> coverage_data) lookup_list in
    let actual_tuple_list =
      List.map
        ~f:(fun { LocationBasedLookup.expression; type_ } -> expression, type_)
        coverage_data_list
    in
    assert_equal
      ~printer:(fun x -> [%sexp_of: (Expression.t option * Type.t) list] x |> Sexp.to_string_hum)
      ~cmp:(fun left right ->
        let compare_coverage_data (left_expression, left_type) (right_expression, right_type) =
          let same =
            Option.compare location_insensitive_compare left_expression right_expression = 0
            && Type.equal left_type right_type
          in
          match same with
          | true -> 0
          | false -> -1
        in
        List.compare compare_coverage_data left right = 0)
      expected_coverage_gap_list
      (List.sort actual_tuple_list ~compare:[%compare: Expression.t option * Type.t])
  in
  assert_coverage_data
    ~context
    ~source:{|
      def foo(x) -> None:
        print(x + 1) |}
    [
      None, Type.meta NoneType;
      ( Some
          (Expression.Name
             (Name.Attribute
                {
                  base =
                    Node.create_with_default_location (Expression.Name (Name.Identifier "test"));
                  attribute = "foo";
                  special = false;
                })
          |> Node.create_with_default_location),
        Type.Callable
          {
            kind = Named (Reference.create "test.foo");
            implementation =
              {
                parameters =
                  Type.Callable.Defined
                    [
                      Type.Callable.Parameter.Named
                        { name = "$parameter$x"; annotation = Type.Top; default = false };
                    ];
                annotation = NoneType;
              };
            overloads = [];
          } );
      ( Some (Expression.Name (Name.Identifier "$parameter$x") |> Node.create_with_default_location),
        Type.Any );
      ( Some (Expression.Name (Name.Identifier "print") |> Node.create_with_default_location),
        Type.Callable
          {
            kind = Named (Reference.create "print");
            implementation =
              {
                parameters =
                  Type.Callable.Defined
                    [
                      Type.Callable.Parameter.Variable (Concrete Type.object_primitive);
                      Type.Callable.Parameter.KeywordOnly
                        { name = "$parameter$sep"; annotation = Type.Top; default = true };
                      Type.Callable.Parameter.KeywordOnly
                        { name = "$parameter$end"; annotation = Type.Top; default = true };
                      Type.Callable.Parameter.KeywordOnly
                        {
                          name = "$parameter$file";
                          annotation = Type.union [Type.NoneType; Type.Primitive "_Writer"];
                          default = true;
                        };
                      Type.Callable.Parameter.KeywordOnly
                        {
                          name = "$parameter$flush";
                          annotation = Type.Primitive "bool";
                          default = true;
                        };
                    ];
                annotation = NoneType;
              };
            overloads = [];
          } );
      ( Some
          (Expression.Call
             {
               callee =
                 Expression.Name (Name.Identifier "print") |> Node.create_with_default_location;
               arguments =
                 [
                   {
                     name = None;
                     value =
                       Expression.Call
                         {
                           callee =
                             Expression.Name
                               (Name.Attribute
                                  {
                                    base =
                                      Node.create_with_default_location
                                        (Expression.Name (Name.Identifier "$parameter$x"));
                                    attribute = "__add__";
                                    special = true;
                                  })
                             |> Node.create_with_default_location;
                           arguments =
                             [
                               {
                                 name = None;
                                 value =
                                   Expression.Constant (Integer 1)
                                   |> Node.create_with_default_location;
                               };
                             ];
                         }
                       |> Node.create_with_default_location;
                   };
                 ];
             }
          |> Node.create_with_default_location),
        NoneType );
      ( Some (Expression.Name (Name.Identifier "$parameter$x") |> Node.create_with_default_location),
        Type.Any );
      ( Some
          (Expression.Call
             {
               callee =
                 Expression.Name
                   (Name.Attribute
                      {
                        base =
                          Node.create_with_default_location
                            (Expression.Name (Name.Identifier "$parameter$x"));
                        attribute = "__add__";
                        special = true;
                      })
                 |> Node.create_with_default_location;
               arguments =
                 [
                   {
                     name = None;
                     value = Expression.Constant (Integer 1) |> Node.create_with_default_location;
                   };
                 ];
             }
          |> Node.create_with_default_location),
        Type.Any );
      ( Some (Expression.Constant (Integer 1) |> Node.create_with_default_location),
        Type.literal_integer 1 );
    ];
  ()


let test_coverage_gaps_in_module context =
  let assert_coverage_gaps_in_module ~context ~source expected =
    let lookup = generate_lookup ~context source in
    let all_nodes_and_coverage = LocationBasedLookup.get_all_nodes_and_coverage_data lookup in
    let coverage_data = List.map ~f:(fun (_, coverage) -> coverage) all_nodes_and_coverage in
    let equal_coverage_gap
        {
          LocationBasedLookup.coverage_data = { expression = left_expression; type_ = left_type };
          reason = left_reason;
        }
        {
          LocationBasedLookup.coverage_data =
            { LocationBasedLookup.expression = right_expression; type_ = right_type };
          reason = right_reason;
        }
      =
      Option.compare location_insensitive_compare left_expression right_expression = 0
      && Type.equal left_type right_type
      && [%compare.equal: LocationBasedLookup.reason] left_reason right_reason
    in
    let actual_coverage_gaps = LocationBasedLookup.coverage_gaps_in_module coverage_data in
    assert_equal
      ~printer:(fun x -> [%sexp_of: LocationBasedLookup.coverage_gap list] x |> Sexp.to_string_hum)
      ~cmp:(List.equal equal_coverage_gap)
      expected
      actual_coverage_gaps
  in
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
      def foo(x) -> None:
          print(x+1)
    |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$parameter$x")
                |> Node.create_with_default_location);
            type_ = Type.Any;
          };
        reason = TypeIsAny ParameterIsAny;
      };
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$parameter$x")
                |> Node.create_with_default_location);
            type_ = Type.Any;
          };
        reason = TypeIsAny ParameterIsAny;
      };
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Call
                   {
                     callee =
                       Expression.Name
                         (Name.Attribute
                            {
                              base =
                                Node.create_with_default_location
                                  (Expression.Name (Name.Identifier "$parameter$x"));
                              attribute = "__add__";
                              special = true;
                            })
                       |> Node.create_with_default_location;
                     arguments =
                       [
                         {
                           name = None;
                           value =
                             Expression.Constant (Integer 1) |> Node.create_with_default_location;
                         };
                       ];
                   }
                |> Node.create_with_default_location);
            type_ = Type.Any;
          };
        reason = TypeIsAny OtherExpressionIsAny;
      };
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name
                   (Name.Attribute
                      {
                        base =
                          Node.create_with_default_location
                            (Expression.Name (Name.Identifier "test"));
                        attribute = "foo";
                        special = false;
                      })
                |> Node.create_with_default_location);
            type_ =
              Type.Callable
                {
                  kind = Type.Callable.Named (Reference.create "test.foo");
                  implementation =
                    {
                      annotation = NoneType;
                      parameters =
                        Type.Callable.Defined
                          [
                            Type.Callable.Parameter.Named
                              { name = "$parameter$x"; annotation = Type.Top; default = false };
                          ];
                    };
                  overloads = [];
                };
          };
        reason = CallableParameterIsUnknownOrAny;
      };
    ];
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
      def foo(x:int) -> int:
          return x
    |}
    [];
  assert_coverage_gaps_in_module ~context ~source:{|
      y: int = 5
    |} [];
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
      y: Any
    |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$local_test$y")
                |> Node.create_with_default_location);
            type_ = Type.Any;
          };
        reason = TypeIsAny OtherExpressionIsAny;
      };
    ];
  assert_coverage_gaps_in_module ~context ~source:{|
      a: list[int] = []
    |} [];
  assert_coverage_gaps_in_module ~context ~source:{|
      a: set[int] = set()
    |} [];
  assert_coverage_gaps_in_module ~context ~source:{|
      a: dict[int,int] = dict()
    |} [];
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
      a = []
    |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$local_test$a")
                |> Node.create_with_default_location);
            type_ = Type.Parametric { name = "list"; parameters = [Type.Parameter.Single Type.Any] };
          };
        reason = ContainerParameterIsAny;
      };
    ];
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
      a = set()
    |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$local_test$a")
                |> Node.create_with_default_location);
            type_ = Type.Parametric { name = "set"; parameters = [Type.Parameter.Single Type.Any] };
          };
        reason = ContainerParameterIsAny;
      };
    ];
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
      a = dict()
    |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$local_test$a")
                |> Node.create_with_default_location);
            type_ =
              Type.Parametric
                {
                  name = "dict";
                  parameters = [Type.Parameter.Single Type.Any; Type.Parameter.Single Type.Any];
                };
          };
        reason = ContainerParameterIsAny;
      };
    ];
  assert_coverage_gaps_in_module
    ~context
    ~source:
      {|
      from typing import Iterable
      def foo() -> Iterable[str]:
        return []

      a = foo()
    |}
    [];
  assert_coverage_gaps_in_module
    ~context
    ~source:
      {|
      from typing import Iterable, Callable, Any
      def foo() -> Callable[[], Any]:
        return a
      x = foo
      y = x()
      |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name (Name.Identifier "$local_test$y")
                |> Node.create
                     ~location:{ start = { line = 6; column = 0 }; stop = { line = 6; column = 1 } }
                );
            type_ =
              Type.Callable
                {
                  kind = Type.Callable.Anonymous;
                  implementation = { annotation = Type.Any; parameters = Defined [] };
                  overloads = [];
                };
          };
        reason = CallableReturnIsAny;
      };
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Call
                   {
                     callee =
                       Expression.Name (Name.Identifier "$local_test$x")
                       |> Node.create
                            ~location:
                              { start = { line = 6; column = 4 }; stop = { line = 6; column = 5 } };
                     arguments = [];
                   }
                |> Node.create
                     ~location:{ start = { line = 6; column = 4 }; stop = { line = 6; column = 7 } }
                );
            type_ =
              Type.Callable
                {
                  kind = Type.Callable.Anonymous;
                  implementation = { annotation = Type.Any; parameters = Defined [] };
                  overloads = [];
                };
          };
        reason = CallableReturnIsAny;
      };
    ];
  assert_coverage_gaps_in_module
    ~context
    ~source:{|
    from typing import Iterable, Any
    def foo() -> Any:
      return 5
    |}
    [
      {
        LocationBasedLookup.coverage_data =
          {
            expression =
              Some
                (Expression.Name
                   (Name.Attribute
                      {
                        base =
                          Expression.Name (Name.Identifier "test")
                          |> Node.create_with_default_location;
                        attribute = "foo";
                        special = false;
                      })
                |> Node.create_with_default_location);
            type_ =
              Type.Callable
                {
                  kind = Type.Callable.Named (Reference.create "test.foo");
                  implementation = { annotation = Type.Any; parameters = Defined [] };
                  overloads = [];
                };
          };
        reason = CallableReturnIsAny;
      };
    ];
  ()


let test_resolve_type_for_symbol context =
  let default_external_sources =
    [
      ( "library.py",
        {|
      class Base: ...

      def return_str() -> str:
          return "hello"
    |} );
    ]
  in
  let module_reference = !&"test" in
  let assert_resolved_explicit_type ?(external_sources = default_external_sources) source expected =
    let type_environment =
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] ~external_sources
        |> ScratchProject.build_type_environment
      in
      type_environment
    in
    let symbol_data =
      LocationBasedLookup.find_narrowest_spanning_symbol
        ~type_environment
        ~module_reference
        (find_indicator_position ~source "cursor")
    in
    assert_equal
      ~cmp:[%compare.equal: Type.t option]
      ~printer:[%show: Type.t option]
      expected
      (symbol_data >>= LocationBasedLookup.resolve_type_for_symbol ~type_environment)
  in
  let assert_resolved_type ?external_sources ?(aliases = Type.empty_aliases) source expected_type =
    let parse_type type_ = Type.create ~aliases (parse_single_expression type_) in
    assert_resolved_explicit_type ?external_sources source (expected_type >>| parse_type)
  in
  assert_resolved_type
    {|
        def getint() -> int:
          return 42

        def main() -> None:
          x = getint()
        # ^- cursor
    |}
    (Some "int");
  assert_resolved_type {|
        x = SomeUntrackedClass()
          #    ^- cursor
    |} None;
  assert_resolved_type
    {|
        def foo(a: int, b: str) -> None: ...
          #             ^- cursor

        # No definition found.
    |}
    None;
  assert_resolved_type
    {|
        def foo(x: str) -> None:
          print(x)
          #     ^- cursor
    |}
    (Some "str");
  assert_resolved_type
    {|
        def getint() -> int:
                      # ^- cursor
          return 42
    |}
    (Some "typing.Type[int]");
  assert_resolved_type
    {|
        def foo() -> None:
          xs: list[str] = ["a", "b"]
          #    ^- cursor
    |}
    (Some "typing.Type[list]");
  assert_resolved_type
    {|
      class Foo:
        def bar(self) -> None:
            print(self.foo())
            #      ^- cursor
    |}
    (Some "test.Foo");
  assert_resolved_explicit_type
    {|
      class Foo:
        def bar(self) -> None:
            print(self.foo())
            #           ^- cursor

        def foo(self) -> int:
          return 42

    |}
    (Type.parametric
       "BoundMethod"
       [
         Single
           (Type.Callable
              {
                kind = Type.Callable.Named (Reference.create "test.Foo.foo");
                implementation =
                  {
                    annotation = Type.integer;
                    parameters =
                      Type.Callable.Defined
                        [
                          Type.Callable.Parameter.Named
                            {
                              name = "$parameter$self";
                              annotation = Type.Primitive "test.Foo";
                              default = false;
                            };
                        ];
                  };
                overloads = [];
              });
         Single (Type.Primitive "test.Foo");
       ]
    |> Option.some);
  assert_resolved_type
    {|
        from library import Base
                           # ^- cursor
    |}
    (Some "typing.Type[library.Base]");
  assert_resolved_type {|
        import library
        #        ^- cursor
    |} None;
  assert_resolved_type
    {|
        from . import library as my_library
                                    # ^- cursor
    |}
    None;
  assert_resolved_type
    {|
        from . import library as my_library

        x = my_library.Base() #    ^- cursor
    |}
    None;
  assert_resolved_explicit_type
    {|
        def return_str() -> str:
          return "hello"

        def foo() -> None:
          return_str().capitalize().lower()
                     # ^- cursor
    |}
    (Type.parametric
       "BoundMethod"
       [
         Single
           (Type.Callable
              {
                kind = Type.Callable.Named (Reference.create "str.capitalize");
                implementation =
                  {
                    annotation = Type.string;
                    parameters =
                      Type.Callable.Defined
                        [
                          Type.Callable.Parameter.Named
                            { name = "$parameter$self"; annotation = Type.string; default = false };
                        ];
                  };
                overloads = [];
              });
         Single Type.string;
       ]
    |> Option.some);
  assert_resolved_explicit_type
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
          #           ^- cursor
    |}
    (Type.parametric
       "BoundMethod"
       [
         Single
           (Type.Callable
              {
                kind = Type.Callable.Named (Reference.create "test.Foo.bar");
                implementation =
                  {
                    annotation = Type.none;
                    parameters =
                      Type.Callable.Defined
                        [
                          Type.Callable.Parameter.Named
                            {
                              name = "$parameter$self";
                              annotation = Type.Primitive "test.Foo";
                              default = false;
                            };
                        ];
                  };
                overloads = [];
              });
         Single (Type.Primitive "test.Foo");
       ]
    |> Option.some);
  assert_resolved_type
    {|
        class Foo: ...

        class Bar:
          some_attribute: Foo = Foo()

        Bar().some_attribute
        #       ^- cursor
    |}
    (Some "test.Foo");
  assert_resolved_type
    {|
        def foo() -> None:
          x = 42
          print(x)
          #     ^- cursor
    |}
    (Some "typing.Literal[42]");
  assert_resolved_type
    {|
        from typing import Iterator
        from contextlib import contextmanager

        @contextmanager
        def open() -> Iterator[str]: ...

        def foo() -> None:
          with open() as f:
            f.readline()
          # ^- cursor
    |}
    (Some "str");
  assert_resolved_type
    {|
        def foo(x: str) -> None:
          for x in [1]:
            print(x)
            #     ^- cursor
    |}
    (Some "int");
  assert_resolved_explicit_type
    {|
        class Foo:
          @classmethod
          def my_class_method(cls) -> None: ...

        def foo() -> None:
          Foo.my_class_method()
          #    ^- cursor
    |}
    (Type.parametric
       "BoundMethod"
       [
         Single
           (Type.Callable
              {
                kind = Type.Callable.Named (Reference.create "test.Foo.my_class_method");
                implementation =
                  {
                    annotation = Type.none;
                    parameters =
                      Type.Callable.Defined
                        [
                          Type.Callable.Parameter.Named
                            {
                              name = "$parameter$cls";
                              annotation = Type.meta (Type.Primitive "test.Foo");
                              default = false;
                            };
                        ];
                  };
                overloads = [];
              });
         Single (Type.meta (Type.Primitive "test.Foo"));
       ]
    |> Option.some);
  assert_resolved_explicit_type
    {|
        class Foo:
          def my_method() -> None: ...

        def foo() -> None:
          Foo.my_method()
          #    ^- cursor
    |}
    (Type.Callable
       {
         kind = Type.Callable.Named (Reference.create "test.Foo.my_method");
         implementation = { annotation = Type.none; parameters = Type.Callable.Defined [] };
         overloads = [];
       }
    |> Option.some);
  assert_resolved_explicit_type
    {|
        class Foo:
          @staticmethod
          def my_static_method() -> None: ...

        def foo() -> None:
          Foo.my_static_method()
          #     ^- cursor
    |}
    (Type.Callable
       {
         kind = Type.Callable.Named (Reference.create "test.Foo.my_static_method");
         implementation = { annotation = Type.none; parameters = Type.Callable.Defined [] };
         overloads = [];
       }
    |> Option.some);
  assert_resolved_type
    {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int

        def main(foo: Foo) -> None:
          print(foo.my_attribute)
          #           ^- cursor
    |}
    (Some "int");
  assert_resolved_type
    {|
        from dataclasses import dataclass

        @dataclass(frozen=True)
        class Foo:
          my_attribute: int

        def main(foo: Foo) -> None:
          if foo.my_attribute:
            #        ^- cursor
            print("hello")
    |}
    (Some "int");
  assert_resolved_type
    {|
        def getint(xs: list[int]) -> None:
          for x in xs:
            #      ^- cursor
            pass
    |}
    (Some "typing.List[int]");
  assert_resolved_type
    {|
        def foo(xs: list[int]) -> None:
          print(f"xs: {xs}")
          #            ^- cursor
    |}
    (Some "typing.List[int]");
  assert_resolved_explicit_type
    {|
        def foo(xs: list[int]) -> None:
          print(f"xs: {xs.append(xs)}")
                        # ^- cursor
    |}
    (Type.parametric
       "BoundMethod"
       [
         Single
           (Type.Callable
              {
                kind = Type.Callable.Named (Reference.create "list.append");
                implementation =
                  {
                    annotation = Type.none;
                    parameters =
                      Type.Callable.Defined
                        [
                          Type.Callable.Parameter.Named
                            {
                              name = "$parameter$self";
                              annotation = Type.list Type.integer;
                              default = false;
                            };
                          Type.Callable.Parameter.PositionalOnly
                            { index = 1; annotation = Type.integer; default = false };
                        ];
                  };
                overloads = [];
              });
         Single (Type.list Type.integer);
       ]
    |> Option.some);
  assert_resolved_type
    {|
        try:
          print("hello")
        except Exception as exception:
          print(exception)
          #      ^- cursor
    |}
    (Some "Exception");
  assert_resolved_type
    {|
        from typing import Callable

        f: Callable
        #   ^- cursor
    |}
    (Some "typing.Type[typing.Callable]");
  ()


let test_hover_info_for_position context =
  let default_external_sources =
    [
      ( "library.py",
        {|
          """module docstring"""
          class Base:
            @staticmethod
            def return_str() -> str:
                """Test"""
                return "hello"
    |}
      );
    ]
  in
  let assert_hover_info_for_position ?(external_sources = default_external_sources) source expected =
    let type_environment =
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] ~external_sources
        |> ScratchProject.build_type_environment
      in
      type_environment
    in
    let value =
      LocationBasedLookup.hover_info_for_position
        ~type_environment
        ~module_reference:!&"test"
        (find_indicator_position ~source "cursor")
    in
    assert_equal ~ctxt:context ~printer:[%show: LocationBasedLookup.hover_info] expected value
  in
  assert_hover_info_for_position
    {|
      test = 5
      # ^- cursor
  |}
    { value = Some "typing_extensions.Literal[5]"; docstring = None };
  assert_hover_info_for_position
    {|
      def test() -> None:
          """docstring"""
          x = 5
      test
      # ^- cursor
  |}
    { value = Some "() -> None"; docstring = Some "docstring" };
  assert_hover_info_for_position
    {|
      class Foo:
          def test() -> None:
              """docstring"""
              x = 5
      Foo.test
      #     ^- cursor
  |}
    { value = Some "() -> None"; docstring = Some "docstring" };
  assert_hover_info_for_position
    {|
      class Foo:
          def test() -> None:
              """docstring"""
              x = 5
      Foo.test()
      #     ^- cursor
  |}
    { value = Some "() -> None"; docstring = Some "docstring" };
  assert_hover_info_for_position
    {|
      class Foo:
          def test() -> None:
      #       ^- cursor
              """docstring"""
              x = 5
  |}
    { value = Some "() -> None"; docstring = Some "docstring" };
  assert_hover_info_for_position
    {|
      def test() -> None:
      #    ^- cursor
          """docstring"""
          x = 5
  |}
    { value = Some "() -> None"; docstring = Some "docstring" };
  assert_hover_info_for_position
    {|
      def test() -> None:
      #    ^- cursor
          x = 5
  |}
    { value = Some "() -> None"; docstring = None };
  (* TODO(T139775850) support complex hover *)
  assert_hover_info_for_position
    {|
      import library
      library.Base.return_str()
      #             ^- cursor
  |}
    { value = Some "() -> str"; docstring = None };
  (* TODO(T139776124) support module docstrings *)
  assert_hover_info_for_position
    {|
      import library
      #       ^- cursor
  |}
    { value = None; docstring = None };
  (* TODO(T139776113) docstrings for classes *)
  assert_hover_info_for_position
    {|
   class Test:
      """docstring"""
      x = 5
   Test
   # ^- cursor
  |}
    { value = Some "Type[test.Test]"; docstring = None };
  (* TODO(T139776639) move docstring into ast *)
  assert_hover_info_for_position
    {|
   class Test:
      """docstring"""
   #      ^- cursor
      x = 5
  |}
    { value = Some "typing_extensions.Literal['docstring']"; docstring = None };
  ()


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
         "lookup_class_attributes_nested" >:: test_lookup_class_attributes_nested;
         "lookup_dataclass_attributes" >:: test_lookup_dataclass_attributes;
         "lookup_comprehensions" >:: test_lookup_comprehensions;
         "lookup_if_statements" >:: test_lookup_if_statements;
         "lookup_imports" >:: test_lookup_imports;
         "lookup_string_annotations" >:: test_lookup_string_annotations;
         "lookup_unbound" >:: test_lookup_unbound;
         "lookup_union_type_resolution" >:: test_lookup_union_type_resolution;
         "lookup_def" >:: test_lookup_def;
         "lookup_async_def" >:: test_lookup_async_def;
         "lookup_unknown_accesses" >:: test_lookup_unknown_accesses;
         "classify_coverage_data" >:: test_classify_coverage_data;
         "lookup_expression" >:: test_lookup_expression;
         "coverage_gaps_in_module" >:: test_coverage_gaps_in_module;
         "resolve_type_for_symbol" >:: test_resolve_type_for_symbol;
         "hover_info_for_position" >:: test_hover_info_for_position;
       ]
  |> Test.run
