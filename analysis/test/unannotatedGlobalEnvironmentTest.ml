(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let test_global_registration context =
  let assert_registers ?(expected = true) source name =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let ast_environment = AstEnvironment.read_only ast_environment in
    let update_result =
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    assert_equal (UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only name) expected
  in
  assert_registers {|
   class Bar:
     pass
  |} "test.Bar";
  assert_registers ~expected:false {|
   class Foo:
     pass
  |} "test.Bar";
  ()


let test_define_registration context =
  let assert_registers ~expected source =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let ast_environment = AstEnvironment.read_only ast_environment in
    let update_result =
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let actual = UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module read_only !&"test" in
    let expected = List.sort expected ~compare:Reference.compare in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:(List.to_string ~f:Reference.show)
      expected
      actual
  in
  assert_registers {|
    def foo():
      pass
  |} ~expected:[!&"test.$toplevel"; !&"test.foo"];
  assert_registers
    {|
    def bar(): ...
    def foo():
      return bar()
  |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"test.bar"];
  assert_registers
    {|
     from typing import overload

     @overload
     def foo(x: int) -> int: ...
     @overload
     def foo(x: str) -> str: ...

     def foo(x):
       return x
  |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"];
  assert_registers
    {|
     class Foo:
       pass
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"];
  assert_registers
    {|
     class Foo:
       x: int
     class Foo:
       y: str
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"];
  assert_registers
    {|
     class Foo:
       def foo(self): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"; !&"test.Foo.foo"];
  assert_registers
    {|
     class Foo:
       @property
       def foo(self): ...
       @foo.setter
       def foo(self, value): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.Foo.$class_toplevel"; !&"test.Foo.foo"];
  assert_registers
    {|
    def foo():
      def bar():
        ...
      return bar()
  |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"];
  assert_registers
    {|
     def foo():
       def bar():
         pass
       bar()
       def baz():
         pass
       baz()
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo$baz"];
  assert_registers
    {|
     def foo():
       def bar():
         def baz():
           pass
       bar(x)
    |}
    ~expected:
      [!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo?bar$baz"];
  assert_registers
    {|
     def foo(flag):
       if flag:
         def bar():
           pass
         return bar()
       else:
         def baz():
           pass
         return baz()
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo$baz"];
  assert_registers
    {|
     def foo():
       for x in range(3):
         def bar():
           def baz():
             pass
       return bar(x)
    |}
    ~expected:
      [!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo?bar$baz"];
  assert_registers
    {|
     def foo():
       with open("something") as f:
         def bar():
           def baz():
             pass
         bar()
    |}
    ~expected:
      [!&"test.$toplevel"; !&"test.foo"; !&"$local_test?foo$bar"; !&"$local_test?foo?bar$baz"];
  assert_registers
    {|
     def foo():
       try:
         def bar():
           pass
         bar()
       except:
         def baz():
           pass
         baz()
       finally:
         def quix():
           pass
         return quix()
    |}
    ~expected:
      [
        !&"test.$toplevel";
        !&"test.foo";
        !&"$local_test?foo$bar";
        !&"$local_test?foo$baz";
        !&"$local_test?foo$quix";
      ];

  (* Do not look into bodies of classes that are not at top-level *)
  assert_registers
    {|
     def foo():
       class C:
         x: int
         def bar(self): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"];
  assert_registers
    {|
     def foo():
       class C:
         x: int
         def bar(self):
           class D:
             def baz(self): ...
    |}
    ~expected:[!&"test.$toplevel"; !&"test.foo"];

  (* Nested class at module toplevel should still work *)
  assert_registers
    {|
     class C:
       def foo(self): ...
       class D:
         def bar(self): ...
  |}
    ~expected:
      [
        !&"test.$toplevel";
        !&"test.C.$class_toplevel";
        !&"test.C.foo";
        !&"test.C.D.$class_toplevel";
        !&"test.C.D.bar";
      ];
  ()


let test_simple_global_registration context =
  let assert_registers source name expected =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let ast_environment = AstEnvironment.read_only ast_environment in
    let update_result =
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let printer global =
      global
      >>| UnannotatedGlobalEnvironment.show_unannotated_global
      |> Option.value ~default:"None"
    in
    let location_insensitive_compare left right =
      Option.compare UnannotatedGlobalEnvironment.compare_unannotated_global left right = 0
    in
    assert_equal
      ~cmp:location_insensitive_compare
      ~printer
      expected
      (UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
         read_only
         (Reference.create name))
  in
  let target_location =
    {
      Location.path = Reference.create "test";
      start = { line = 2; column = 0 };
      stop = { line = 2; column = 3 };
    }
  in
  let value_location =
    {
      Location.path = Reference.create "test";
      start = { line = 2; column = 6 };
      stop = { line = 2; column = 7 };
    }
  in
  let value =
    let value = parse_single_expression "8" in
    { value with location = value_location }
  in
  assert_registers
    {|
    bar = 8
  |}
    "test.bar"
    (Some (SimpleAssign { explicit_annotation = None; value; target_location }));
  assert_registers {|
    other.bar = 8
  |} "test.other.bar" None;
  assert_registers {|
    other.bar = 8
  |} "other.bar" None;
  assert_registers
    {|
    try:
      baz = 8
    except:
      pass
  |}
    "test.baz"
    (Some
       (SimpleAssign
          {
            explicit_annotation = None;
            value = +Expression.Expression.Integer 8;
            target_location =
              {
                Location.path = !&"test";
                start = { line = 3; column = 2 };
                stop = { line = 3; column = 5 };
              };
          }));
  let parse_define define =
    match parse_single_statement define ~preprocess:true ~handle:"test.py" with
    | { Node.value = Statement.Statement.Define { signature; _ }; location } ->
        Node.create signature ~location
    | _ -> failwith "not define"
  in
  assert_registers
    {|
      def foo(x: int) -> str:
        pass
      def foo(x: float) -> bool:
        pass
    |}
    "test.foo"
    (Some
       (Define
          [
            parse_define
              {|
                def foo(x: int) -> str:
                    pass
              |};
            parse_define
              {|
                def foo(x: float) -> bool:
                  pass
              |};
          ]));
  ()


let test_updates context =
  let assert_updates
      ?original_source
      ?new_source
      ~middle_actions
      ~expected_triggers
      ?post_actions
      ()
    =
    Memory.reset_shared_memory ();
    let sources = original_source >>| (fun source -> "test.py", source) |> Option.to_list in
    let project =
      ScratchProject.setup
        ~include_typeshed_stubs:false
        ~incremental_style:FineGrained
        sources
        ~context
    in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let configuration = ScratchProject.configuration_of project in
    let update_result =
      let ast_environment = AstEnvironment.read_only ast_environment in
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler:(mock_scheduler ())
        ~configuration
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
    in
    let read_only = UnannotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let execute_action = function
      | `Get (class_name, dependency, expected_number_of_statements) ->
          let printer number =
            number >>| Format.sprintf "number of attributes: %d" |> Option.value ~default:"No class"
          in
          UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
            read_only
            ~dependency
            class_name
          >>| Node.value
          >>| (fun { ClassSummary.attribute_components; _ } -> attribute_components)
          >>| Ast.Statement.Class.attributes
          >>| Identifier.SerializableMap.bindings
          >>| List.length
          |> assert_equal ~printer expected_number_of_statements
      | `Mem (class_name, dependency, expectation) ->
          UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only ~dependency class_name
          |> assert_equal expectation
      | `AllClasses expectation ->
          UnannotatedGlobalEnvironment.ReadOnly.all_classes read_only
          |> assert_equal ~printer:(List.to_string ~f:Fn.id) expectation
      | `Global (global_name, dependency, expectation) ->
          let printer optional =
            optional
            >>| UnannotatedGlobalEnvironment.show_unannotated_global
            |> Option.value ~default:"none"
          in
          let cmp left right =
            Option.compare UnannotatedGlobalEnvironment.compare_unannotated_global left right = 0
          in
          let remove_target_location = function
            | UnannotatedGlobalEnvironment.SimpleAssign assign ->
                UnannotatedGlobalEnvironment.SimpleAssign
                  { assign with target_location = Location.Reference.any }
            | UnannotatedGlobalEnvironment.TupleAssign assign ->
                UnannotatedGlobalEnvironment.TupleAssign
                  { assign with target_location = Location.Reference.any }
            | global -> global
          in
          UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
            read_only
            global_name
            ~dependency
          >>| remove_target_location
          |> assert_equal ~cmp ~printer expectation
      | `Define (define_name, dependency, expectation) ->
          let actual =
            UnannotatedGlobalEnvironment.ReadOnly.get_define read_only define_name ~dependency
          in
          let cmp left right =
            Int.equal
              0
              (Option.compare UnannotatedGlobalEnvironment.FunctionDefinition.compare left right)
          in
          assert_equal
            ~cmp
            ~printer:(fun definition ->
              Sexp.to_string_hum
                [%message (definition : UnannotatedGlobalEnvironment.FunctionDefinition.t option)])
            expectation
            actual
      | `DefineBody (define_name, dependency, expectation) ->
          let actual =
            UnannotatedGlobalEnvironment.ReadOnly.get_define_body read_only define_name ~dependency
          in
          let cmp =
            let equal left right =
              Int.equal
                0
                (Node.location_sensitive_compare
                   Statement.Define.location_sensitive_compare
                   left
                   right)
            in
            Option.equal equal
          in
          assert_equal
            ~cmp
            ~printer:(fun bodies ->
              Sexp.to_string_hum [%message (bodies : Statement.Define.t Node.t option)])
            expectation
            actual
    in
    List.iter middle_actions ~f:execute_action;
    let add_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        content
        ~relative
      =
      let content = trim_extra_indentation content in
      let file = File.create ~content (Path.create_relative ~root:local_root ~relative) in
      File.write file
    in
    let delete_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        relative
      =
      Path.create_relative ~root:local_root ~relative |> Path.absolute |> Core.Unix.remove
    in
    if Option.is_some original_source then
      delete_file project "test.py";
    new_source >>| add_file project ~relative:"test.py" |> Option.value ~default:();
    let { ScratchProject.module_tracker; _ } = project in
    let { Configuration.Analysis.local_root; _ } = configuration in
    let path = Path.create_relative ~root:local_root ~relative:"test.py" in
    let update_result =
      ModuleTracker.update ~configuration ~paths:[path] module_tracker
      |> (fun updates -> AstEnvironment.Update updates)
      |> AstEnvironment.update ~configuration ~scheduler:(mock_scheduler ()) ast_environment
      |> fun ast_environment_update_result ->
      UnannotatedGlobalEnvironment.update_this_and_all_preceding_environments
        (AstEnvironment.read_only ast_environment)
        ~scheduler:(mock_scheduler ())
        ~configuration
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
    in
    let printer set =
      SharedMemoryKeys.DependencyKey.KeySet.elements set
      |> List.to_string ~f:SharedMemoryKeys.show_dependency
    in
    let expected_triggers = SharedMemoryKeys.DependencyKey.KeySet.of_list expected_triggers in
    assert_equal
      ~cmp:SharedMemoryKeys.DependencyKey.KeySet.equal
      ~printer
      expected_triggers
      (UnannotatedGlobalEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = SharedMemoryKeys.TypeCheckDefine (Reference.create "dep") in
  (* get_class_definition *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Get ("test.Missing", dependency, None)]
    ~expected_triggers:[]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Unrelated:
        x: int
      class Foo:
        x: int
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[]
    ();

  (* Last class definition wins *)
  assert_updates
    ~original_source:
      {|
      class Foo:
        x: int
      class Foo:
        x: int
        y: int
    |}
    ~new_source:{|
      class Unrelated:
        x: int
      class Foo:
        x: int
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 2)]
    ~expected_triggers:[dependency]
    ();

  (* class_exists *)
  assert_updates
    ~new_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, false)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[]
    ();

  (* TODO(T53500184): need to add an existence-only dependency "kind" *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[dependency]
    ();

  (* all_classes *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
      class Bar:
        y: str
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`AllClasses ["test.Bar"; "test.Foo"]]
    ~expected_triggers:[]
    ~post_actions:[`AllClasses ["test.Foo"]]
    ();

  (* get_unannotated_global *)
  let dependency = SharedMemoryKeys.AliasRegister (Reference.create "dep") in
  assert_updates
    ~original_source:{|
      x: int = 7
    |}
    ~new_source:{|
      x: int = 9
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = Some (parse_single_expression "int");
                   value = parse_single_expression "7";
                   target_location = Location.Reference.any;
                 }) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = Some (parse_single_expression "int");
                   value = parse_single_expression "9";
                   target_location = Location.Reference.any;
                 }) );
      ]
    ();
  assert_updates
    ~original_source:{|
      import target.member as alias
    |}
    ~new_source:{|
      import target.member as new_alias
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.alias",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.member")) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:[`Global (Reference.create "test.alias", dependency, None)]
    ();
  assert_updates
    ~original_source:{|
      from target import member, other_member
    |}
    ~new_source:{|
      from target import other_member, member
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.member")) );
        `Global
          ( Reference.create "test.other_member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.other_member")) );
      ]
      (* Location insensitive *)
    ~expected_triggers:[]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.member")) );
        `Global
          ( Reference.create "test.other_member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.other_member")) );
      ]
    ();

  (* Don't infer * as a real thing *)
  assert_updates
    ~original_source:{|
      from target import *
    |}
    ~middle_actions:[`Global (Reference.create "test.*", dependency, None)]
    ~expected_triggers:[]
    ();

  assert_updates
    ~original_source:{|
      X, Y, Z = int, str, bool
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.TupleAssign
                 {
                   value = parse_single_expression "int, str, bool";
                   index = 0;
                   target_location = Location.Reference.any;
                   total_length = 3;
                 }) );
        `Global
          ( Reference.create "test.Y",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.TupleAssign
                 {
                   value = parse_single_expression "int, str, bool";
                   index = 1;
                   target_location = Location.Reference.any;
                   total_length = 3;
                 }) );
        `Global
          ( Reference.create "test.Z",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.TupleAssign
                 {
                   value = parse_single_expression "int, str, bool";
                   index = 2;
                   target_location = Location.Reference.any;
                   total_length = 3;
                 }) );
      ]
    ~expected_triggers:[dependency]
    ();

  (* First global wins. *)
  assert_updates
    ~original_source:{|
      X = int
      X = str
    |}
    ~new_source:{|
      X = int
      X = str
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = None;
                   value = parse_single_expression "int";
                   target_location = Location.Reference.any;
                 }) );
      ]
    ~expected_triggers:[]
    ();

  (* Only recurse into ifs *)
  assert_updates
    ~original_source:{|
      if condition:
        X = int
      else:
        X = str
    |}
    ~new_source:{|
      if condition:
        X = int
      else:
        X = str
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = None;
                   value = parse_single_expression "int";
                   target_location = Location.Reference.any;
                 }) );
      ]
    ~expected_triggers:[]
    ();

  (* Keep different dependencies straight *)
  let alias_dependency = SharedMemoryKeys.AliasRegister (Reference.create "same_dep") in
  let check_dependency = SharedMemoryKeys.TypeCheckDefine (Reference.create "same_dep") in
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:
      [`Get ("test.Foo", alias_dependency, Some 1); `Get ("test.Foo", check_dependency, Some 1)]
    ~expected_triggers:[alias_dependency; check_dependency]
    ();

  (* Addition should trigger previous failed reads *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
      x: int = 9
    |}
    ~middle_actions:[`Global (Reference.create "test.x", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = Some (parse_single_expression "int");
                   value = parse_single_expression "9";
                   target_location = Location.Reference.any;
                 }) );
      ]
    ();
  assert_updates
    ~original_source:
      {|
      class Foo:
        def method(self) -> None:
         print("hello")
    |}
    ~new_source:{|
      class Foo:
        def method(self) -> int:
          return 1
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[dependency]
    ~post_actions:[`Get ("test.Foo", dependency, Some 1)]
    ();
  assert_updates
    ~original_source:
      {|
      class Foo:
        def method(self) -> None:
         print("hello")
    |}
    ~new_source:
      {|
      class Foo:
        def method(self) -> None:
         print("goodbye")
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[]
    ~post_actions:[`Get ("test.Foo", dependency, Some 1)]
    ();
  let parse_define define =
    match parse_single_statement define ~preprocess:true ~handle:"test.py" with
    | { Node.value = Statement.Statement.Define { signature; _ }; location } ->
        Node.create signature ~location
    | _ -> failwith "not define"
  in
  assert_updates
    ~original_source:{|
      def foo() -> None:
       print("hello")
    |}
    ~new_source:{|
      def foo() -> None:
       print("goodbye")
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.foo",
            dependency,
            Some (UnannotatedGlobalEnvironment.Define [parse_define "def foo() -> None: pass"]) );
      ]
    ~expected_triggers:[]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.foo",
            dependency,
            Some (UnannotatedGlobalEnvironment.Define [parse_define "def foo() -> None: pass"]) );
      ]
    ();

  (* Get typecheck unit *)
  let dependency = SharedMemoryKeys.TypeCheckDefine !&"test" in
  let open Statement in
  let open Expression in
  let path = !&"test" in
  let create_simple_return ~start ~stop expression =
    node
      ~path
      ~start
      ~stop
      (Statement.Return { Return.is_implicit = false; expression = Some expression })
  in
  let create_simple_define ~start ~stop name body =
    node
      ~path
      ~start
      ~stop
      {
        Define.signature =
          {
            Define.Signature.name;
            parameters = [];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            generator = false;
            parent = None;
            nesting_define = None;
          };
        captures = [];
        body;
      }
  in
  (* Body doesn't change *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
      def foo():
        return 1
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 1));
                 ]) );
      ]
    ~expected_triggers:[]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 1));
                 ]) );
      ]
    ();

  (* Body changes *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
      def foo():
        return 2
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 1));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 2));
                 ]) );
      ]
    ();

  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
      def foo():
        return 2
      def foo():
        return 3
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 1));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        (* Last define wins *)
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(4, 0)
                 ~stop:(5, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(5, 2)
                     ~stop:(5, 10)
                     (node ~path ~start:(5, 9) ~stop:(5, 10) (Expression.Integer 3));
                 ]) );
      ]
    ();
  assert_updates
    ~original_source:{|
      def foo():
        return 1
      def foo():
        return 2
    |}
    ~new_source:{|
      def foo():
        return 3
    |}
    ~middle_actions:
      [
        (* Last define wins *)
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(4, 0)
                 ~stop:(5, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(5, 2)
                     ~stop:(5, 10)
                     (node ~path ~start:(5, 9) ~stop:(5, 10) (Expression.Integer 2));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 3));
                 ]) );
      ]
    ();

  assert_updates
    ~original_source:
      {|
      from typing import overload
      @overload
      def foo(x: int) -> int: ...
      @overload
      def foo(x: str) -> str: ...
      def foo(x):
        return x
    |}
    ~new_source:
      {|
      from typing import overload
      @overload
      def foo(x: str) -> str: ...
      def foo(x):
        return x
      @overload
      def foo(x: int) -> int: ...
    |}
    ~middle_actions:
      [
        (let body =
           node
             ~path
             ~start:(7, 0)
             ~stop:(8, 10)
             {
               Define.signature =
                 {
                   Define.Signature.name = !&"test.foo";
                   parameters =
                     [
                       node
                         ~path
                         ~start:(7, 8)
                         ~stop:(7, 9)
                         { Parameter.name = "$parameter$x"; value = None; annotation = None };
                     ];
                   decorators = [];
                   docstring = None;
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               body =
                 [
                   create_simple_return
                     ~start:(8, 2)
                     ~stop:(8, 10)
                     (node
                        ~path
                        ~start:(8, 9)
                        ~stop:(8, 10)
                        (Expression.Name (Name.Identifier "$parameter$x")));
                 ];
             }
         in
         `DefineBody (!&"test.foo", dependency, Some body));
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        (let body =
           node
             ~path
             ~start:(5, 0)
             ~stop:(6, 10)
             {
               Define.signature =
                 {
                   Define.Signature.name = !&"test.foo";
                   parameters =
                     [
                       node
                         ~path
                         ~start:(5, 8)
                         ~stop:(5, 9)
                         { Parameter.name = "$parameter$x"; value = None; annotation = None };
                     ];
                   decorators = [];
                   docstring = None;
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = None;
                   nesting_define = None;
                 };
               captures = [];
               body =
                 [
                   create_simple_return
                     ~start:(6, 2)
                     ~stop:(6, 10)
                     (node
                        ~path
                        ~start:(6, 9)
                        ~stop:(6, 10)
                        (Expression.Name (Name.Identifier "$parameter$x")));
                 ];
             }
         in
         `DefineBody (!&"test.foo", dependency, Some body));
      ]
    ();
  let () =
    let body =
      node
        ~path
        ~start:(5, 0)
        ~stop:(6, 10)
        {
          Define.signature =
            {
              Define.Signature.name = !&"test.foo";
              parameters =
                [
                  node
                    ~path
                    ~start:(5, 8)
                    ~stop:(5, 9)
                    { Parameter.name = "$parameter$x"; value = None; annotation = None };
                ];
              decorators = [];
              docstring = None;
              return_annotation = None;
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            };
          captures = [];
          body =
            [
              create_simple_return
                ~start:(6, 2)
                ~stop:(6, 10)
                (node
                   ~path
                   ~start:(6, 9)
                   ~stop:(6, 10)
                   (Expression.Name (Name.Identifier "$parameter$x")));
            ];
        }
    in
    let first_overload =
      node
        ~path
        ~start:(4, 0)
        ~stop:(4, 27)
        {
          Define.signature =
            {
              Define.Signature.name = !&"test.foo";
              parameters =
                [
                  node
                    ~path
                    ~start:(4, 8)
                    ~stop:(4, 9)
                    {
                      Parameter.name = "$parameter$x";
                      value = None;
                      annotation =
                        Some
                          (node
                             ~path
                             ~start:(4, 11)
                             ~stop:(4, 14)
                             (Expression.Name (Name.Identifier "int")));
                    };
                ];
              decorators =
                [
                  node
                    ~path
                    ~start:(3, 1)
                    ~stop:(3, 9)
                    (Expression.Name
                       (Name.Attribute
                          {
                            Name.Attribute.base =
                              node
                                ~path
                                ~start:(3, 1)
                                ~stop:(3, 9)
                                (Expression.Name (Name.Identifier "typing"));
                            attribute = "overload";
                            special = false;
                          }));
                ];
              docstring = None;
              return_annotation =
                Some
                  (node
                     ~path
                     ~start:(4, 19)
                     ~stop:(4, 22)
                     (Expression.Name (Name.Identifier "int")));
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            };
          captures = [];
          body =
            [
              node
                ~path
                ~start:(4, 24)
                ~stop:(4, 27)
                (Statement.Expression (node ~path ~start:(4, 24) ~stop:(4, 27) Expression.Ellipsis));
            ];
        }
    in
    let second_overload =
      node
        ~path
        ~start:(8, 0)
        ~stop:(8, 27)
        {
          Define.signature =
            {
              Define.Signature.name = !&"test.foo";
              parameters =
                [
                  node
                    ~path
                    ~start:(8, 8)
                    ~stop:(8, 9)
                    {
                      Parameter.name = "$parameter$x";
                      value = None;
                      annotation =
                        Some
                          (node
                             ~path
                             ~start:(8, 11)
                             ~stop:(8, 14)
                             (Expression.Name (Name.Identifier "str")));
                    };
                ];
              decorators =
                [
                  node
                    ~path
                    ~start:(7, 1)
                    ~stop:(7, 9)
                    (Expression.Name
                       (Name.Attribute
                          {
                            Name.Attribute.base =
                              node
                                ~path
                                ~start:(7, 1)
                                ~stop:(7, 9)
                                (Expression.Name (Name.Identifier "typing"));
                            attribute = "overload";
                            special = false;
                          }));
                ];
              docstring = None;
              return_annotation =
                Some
                  (node
                     ~path
                     ~start:(7, 19)
                     ~stop:(7, 22)
                     (Expression.Name (Name.Identifier "str")));
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            };
          captures = [];
          body =
            [
              node
                ~path
                ~start:(8, 24)
                ~stop:(8, 27)
                (Statement.Expression (node ~path ~start:(8, 24) ~stop:(8, 27) Expression.Ellipsis));
            ];
        }
    in
    assert_updates
      ~original_source:
        {|
      from typing import overload
      @overload
      def foo(x: int) -> int: ...
      def foo(x):
        return x
    |}
      ~new_source:
        {|
      from typing import overload
      @overload
      def foo(x: int) -> int: ...
      def foo(x):
        return x
      @overload
      def foo(x: str) -> str: ...
    |}
      ~middle_actions:
        [
          (let definition =
             let open UnannotatedGlobalEnvironment.FunctionDefinition in
             let siblings = [{ Sibling.kind = Sibling.Kind.Overload; body = first_overload }] in
             { body = Some body; siblings; qualifier = !&"test" }
           in
           `Define (!&"test.foo", dependency, Some definition));
        ]
      ~expected_triggers:[dependency]
      ~post_actions:
        [
          (let definition =
             let open UnannotatedGlobalEnvironment.FunctionDefinition in
             let siblings =
               [
                 { Sibling.kind = Sibling.Kind.Overload; body = first_overload };
                 { Sibling.kind = Sibling.Kind.Overload; body = second_overload };
               ]
             in
             { body = Some body; siblings; qualifier = !&"test" }
           in
           `Define (!&"test.foo", dependency, Some definition));
        ]
      ()
  in

  assert_updates
    ~original_source:{|
      class A:
        foo: int
    |}
    ~new_source:
      {|
      class A:
        @property
        def foo(self) -> int: ...
        @foo.setter
        def foo(self, value: int) -> None: ...
    |}
    ~middle_actions:[`Define (!&"test.A.foo", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        (let definition =
           let open UnannotatedGlobalEnvironment.FunctionDefinition in
           let create_elipsis ~start ~stop () =
             node
               ~path
               ~start
               ~stop
               (Statement.Expression (node ~path ~start ~stop Expression.Ellipsis))
           in
           let body =
             node
               ~path
               ~start:(4, 2)
               ~stop:(4, 27)
               {
                 Define.signature =
                   {
                     Define.Signature.name = !&"test.A.foo";
                     parameters =
                       [
                         node
                           ~path
                           ~start:(4, 10)
                           ~stop:(4, 14)
                           { Parameter.name = "$parameter$self"; value = None; annotation = None };
                       ];
                     decorators =
                       [
                         node
                           ~path
                           ~start:(3, 3)
                           ~stop:(3, 11)
                           (Expression.Name (Name.Identifier "property"));
                       ];
                     docstring = None;
                     return_annotation =
                       Some
                         (node
                            ~path
                            ~start:(4, 19)
                            ~stop:(4, 22)
                            (Expression.Name (Name.Identifier "int")));
                     async = false;
                     generator = false;
                     parent = Some !&"test.A";
                     nesting_define = None;
                   };
                 captures = [];
                 body = [create_elipsis ~start:(4, 24) ~stop:(4, 27) ()];
               }
           in
           let siblings =
             [
               (let body =
                  node
                    ~path
                    ~start:(6, 2)
                    ~stop:(6, 40)
                    {
                      Define.signature =
                        {
                          Define.Signature.name = !&"test.A.foo";
                          parameters =
                            [
                              node
                                ~path
                                ~start:(6, 10)
                                ~stop:(6, 14)
                                {
                                  Parameter.name = "$parameter$self";
                                  value = None;
                                  annotation = None;
                                };
                              node
                                ~path
                                ~start:(6, 16)
                                ~stop:(6, 21)
                                {
                                  Parameter.name = "$parameter$value";
                                  value = None;
                                  annotation =
                                    Some
                                      (node
                                         ~path
                                         ~start:(6, 23)
                                         ~stop:(6, 26)
                                         (Expression.Name (Name.Identifier "int")));
                                };
                            ];
                          decorators =
                            [
                              node
                                ~path
                                ~start:(5, 3)
                                ~stop:(5, 13)
                                (Expression.Name
                                   (Name.Attribute
                                      {
                                        base =
                                          node
                                            ~path
                                            ~start:(5, 3)
                                            ~stop:(5, 6)
                                            (Expression.Name (Name.Identifier "foo"));
                                        attribute = "setter";
                                        special = false;
                                      }));
                            ];
                          docstring = None;
                          return_annotation =
                            Some
                              (node
                                 ~path
                                 ~start:(6, 31)
                                 ~stop:(6, 35)
                                 (Expression.Name (Name.Identifier "None")));
                          async = false;
                          generator = false;
                          parent = Some !&"test.A";
                          nesting_define = None;
                        };
                      captures = [];
                      body = [create_elipsis ~start:(6, 37) ~stop:(6, 40) ()];
                    }
                in
                { Sibling.kind = Sibling.Kind.PropertySetter; body });
             ]
           in
           { body = Some body; siblings; qualifier = !&"test" }
         in
         `Define (!&"test.A.foo", dependency, Some definition));
      ]
    ();

  (* Location-only change *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:
      {|
      # The truth is, the game was rigged from the start.
      def foo():
          return 1
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 1));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(3, 0)
                 ~stop:(4, 12)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(4, 4)
                     ~stop:(4, 12)
                     (node ~path ~start:(4, 11) ~stop:(4, 12) (Expression.Integer 1));
                 ]) );
      ]
    ();

  (* Added define *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
      def foo():
        return 2
    |}
    ~middle_actions:[`DefineBody (!&"test.foo", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 2));
                 ]) );
      ]
    ();

  (* Removed define *)
  assert_updates
    ~original_source:{|
      def foo():
        return 1
    |}
    ~new_source:{|
    |}
    ~middle_actions:
      [
        `DefineBody
          ( !&"test.foo",
            dependency,
            Some
              (create_simple_define
                 ~start:(2, 0)
                 ~stop:(3, 10)
                 !&"test.foo"
                 [
                   create_simple_return
                     ~start:(3, 2)
                     ~stop:(3, 10)
                     (node ~path ~start:(3, 9) ~stop:(3, 10) (Expression.Integer 1));
                 ]) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:[`DefineBody (!&"test.foo", dependency, None)]
    ();
  ()


let () =
  "environment"
  >::: [
         "global_registration" >:: test_global_registration;
         "define_registration" >:: test_define_registration;
         "simple_globals" >:: test_simple_global_registration;
         "updates" >:: test_updates;
       ]
  |> Test.run
