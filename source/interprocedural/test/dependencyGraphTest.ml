(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Interprocedural
open Statement
open Test

let setup ?(update_environment_with = []) ~context ~handle source =
  let project =
    let external_sources =
      List.map update_environment_with ~f:(fun { handle; source } -> handle, source)
    in
    ScratchProject.setup ~context ~external_sources [handle, source]
  in
  let { ScratchProject.BuiltTypeEnvironment.sources; type_environment; _ } =
    ScratchProject.build_type_environment project
  in
  let source =
    List.find_exn sources ~f:(fun { Source.module_path = { ModulePath.relative; _ }; _ } ->
        String.equal relative handle)
  in
  source, type_environment, ScratchProject.configuration_of project


let create_call_graph ?(update_environment_with = []) ~context source_text =
  let source, environment, configuration =
    setup ~update_environment_with ~context ~handle:"test.py" source_text
  in
  let static_analysis_configuration = Configuration.StaticAnalysis.create configuration () in
  let override_graph =
    OverrideGraph.Heap.from_source ~environment ~include_unit_tests:true ~source
    |> OverrideGraph.SharedMemory.from_heap
  in
  let () =
    let errors = TypeEnvironment.ReadOnly.get_errors environment !&"test" in
    if not (List.is_empty errors) then
      Format.asprintf
        "Type errors in %s\n%a"
        source_text
        (Format.pp_print_list TypeCheck.Error.pp)
        errors
      |> failwith
  in
  let callables =
    FetchCallables.from_source
      ~configuration
      ~resolution:(TypeEnvironment.ReadOnly.global_resolution environment)
      ~include_unit_tests:true
      ~source
    |> FetchCallables.get_callables
  in
  let fold call_graph callable =
    let callees =
      CallGraph.call_graph_of_callable
        ~static_analysis_configuration
        ~environment
        ~override_graph
        ~attribute_targets:(Target.HashSet.create ())
        ~callable
      |> CallGraph.DefineCallGraph.all_targets
    in
    CallGraph.WholeProgramCallGraph.add_or_exn call_graph ~callable ~callees
  in
  List.fold ~init:CallGraph.WholeProgramCallGraph.empty ~f:fold callables


let create_callable = function
  | `Function name -> !&name |> Target.create_function
  | `Method name -> !&name |> Target.create_method
  | `Override name -> !&name |> Target.create_override


let compare_dependency_graph call_graph ~expected =
  let expected =
    let map_callee_callers (callee, callers) =
      ( create_callable callee,
        List.map callers ~f:create_callable |> List.sort ~compare:Target.compare )
    in
    List.map expected ~f:map_callee_callers
  in
  let call_graph =
    List.map call_graph ~f:(fun (callee, callers) ->
        callee, List.sort callers ~compare:Target.compare)
  in
  let printer call_graph =
    Sexp.to_string [%message (call_graph : (Target.t * Target.t list) list)]
  in
  assert_equal ~printer expected call_graph


let assert_call_graph ?update_environment_with ~context source ~expected =
  let graph =
    create_call_graph ?update_environment_with ~context source
    |> DependencyGraph.Reversed.from_call_graph
    |> DependencyGraph.Reversed.to_target_graph
    |> TargetGraph.to_alist
  in
  compare_dependency_graph graph ~expected


let assert_reverse_call_graph ~context source ~expected =
  let graph =
    create_call_graph ~context source
    |> DependencyGraph.Reversed.from_call_graph
    |> DependencyGraph.Reversed.reverse
    |> DependencyGraph.to_target_graph
    |> TargetGraph.to_alist
  in
  compare_dependency_graph graph ~expected


let test_construction context =
  let assert_call_graph = assert_call_graph ~context in
  assert_call_graph
    {|
    class Foo:
      def __init__(self):
        pass

      def bar(self):
        return 10

      def qux(self):
        return self.bar()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Method "test.Foo.$class_toplevel", [];
        `Method "test.Foo.__init__", [];
        `Method "test.Foo.bar", [];
        `Method "test.Foo.qux", [`Method "test.Foo.bar"];
      ];
  assert_call_graph
    {|
    class Foo:
      def __init__(self):
        pass

      def bar(self):
        return self.qux()

      def qux(self):
        return self.bar()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Method "test.Foo.$class_toplevel", [];
        `Method "test.Foo.__init__", [];
        `Method "test.Foo.bar", [`Method "test.Foo.qux"];
        `Method "test.Foo.qux", [`Method "test.Foo.bar"];
      ];
  assert_call_graph
    {|
     class A:
       def __init__(self) -> None:
         pass

     class B:
       def __init__(self) -> None:
         a = A()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Method "test.A.$class_toplevel", [];
        `Method "test.A.__init__", [];
        `Method "test.B.$class_toplevel", [];
        `Method "test.B.__init__", [`Method "test.A.__init__"; `Method "object.__new__"];
      ];
  assert_call_graph
    ~update_environment_with:
      [{ handle = "foobar.pyi"; source = {|
            def bar(x: str) -> str: ...
          |} }]
    {|
     def foo():
       foobar.bar("foo")
    |}
    ~expected:[`Function "test.$toplevel", []; `Function "test.foo", [`Function "foobar.bar"]];
  assert_call_graph
    ~update_environment_with:
      [{ handle = "bar/baz/qux.pyi"; source = {|
            def derp() -> str: ...
          |} }]
    {|
     from bar.baz import qux
     def foo():
       qux.derp()
    |}
    ~expected:[`Function "test.$toplevel", []; `Function "test.foo", [`Function "bar.baz.qux.derp"]];
  assert_call_graph
    {|
       class Base:
         def foo(self) -> None: ...
       class C(Base):
         pass
       def call_foo(c: C) -> None:
         c.foo()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.call_foo", [`Method "test.Base.foo"];
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
      ];
  assert_call_graph
    {|
       class Base:
         def foo(self) -> None: ...
       class C(Base):
         pass
       class D(C):
         def foo(self) -> None: ...
       class E(C):
         pass
       def call_foo(c: C) -> None:
         c.foo()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.call_foo", [`Method "test.Base.foo"; `Method "test.D.foo"];
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Method "test.E.$class_toplevel", [];
      ];

  (* Ensure that we don't include UnrelatedToC.foo here. *)
  assert_call_graph
    {|
       class Base:
         def foo(self) -> None: ...
       class C(Base):
         pass
       class D(C):
         def foo(self) -> None: ...
       class UnrelatedToC(Base):
         def foo(self) -> None: ...
       def call_foo(c: C) -> None:
         c.foo()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.call_foo", [`Method "test.Base.foo"; `Method "test.D.foo"];
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Method "test.UnrelatedToC.$class_toplevel", [];
      ];

  (* We only dereference overrides by one level. *)
  assert_call_graph
    {|
       class Base:
         def foo(self) -> None: ...
       class C(Base):
         pass
       class Child(C):
         def foo(self) -> None: ...
       class Grandchild(Child):
         def foo(self) -> None: ...
       def call_foo(c: C) -> None:
         c.foo()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.call_foo", [`Override "test.Child.foo"; `Method "test.Base.foo"];
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Method "test.Child.$class_toplevel", [];
        `Method "test.Grandchild.$class_toplevel", [];
      ];
  assert_call_graph
    {|
      class C:
        def foo(self) -> int: ...
      class D(C):
        def bar(self) -> int: ...
      class E(D):
        def foo(self) -> int: ...
      def calls_c(c: C) -> None:
        c.foo()
      def calls_d(d: D) -> None:
        d.foo()
      def calls_e(e: E) -> None:
        e.foo()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.calls_c", [`Override "test.C.foo"];
        `Function "test.calls_d", [`Method "test.E.foo"; `Method "test.C.foo"];
        `Function "test.calls_e", [`Method "test.E.foo"];
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Method "test.E.$class_toplevel", [];
      ];
  assert_call_graph
    {|
      class C(str):
        def format(self, *args) -> C: ...
      def format_str() -> None:
        "string literal {}".format("foo")
    |}
    (* If we didn't weaken literals, the call would be a method("str.format") instead of override
       here. *)
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.format_str", [`Override "str.format"];
        `Method "test.C.$class_toplevel", [];
      ];

  assert_call_graph
    {|
      def foo() -> None:
        def bar() -> None:
          "ASD".format("ASD").lower()
        bar()
    |}
    ~expected:
      [
        `Function "$local_test?foo$bar", [`Method "str.lower"; `Override "str.format"];
        `Function "test.$toplevel", [];
        `Function "test.foo", [`Function "$local_test?foo$bar"];
      ];
  assert_call_graph
    {|
      from typing import Generic, TypeVar
      T = TypeVar("T")
      class C(Generic[T]):
        def method(self) -> int: ...
      class D(C[int]):
        def method(self) -> int: ...
      def calls_C_str(c: C[str]) -> None:
        c.method()
      def calls_C_int(c: C[int]) -> None:
        c.method()
    |}
    ~expected:
      [
        `Function "test.$toplevel", [];
        `Function "test.calls_C_int", [`Override "test.C.method"];
        `Function "test.calls_C_str", [`Override "test.C.method"];
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
      ]


let test_construction_reverse context =
  assert_reverse_call_graph
    ~context
    {|
    class Foo:
      def __init__(self):
        pass

      def bar(self):
        return 10

      def qux(self):
        return self.bar()
    |}
    ~expected:[`Method "test.Foo.bar", [`Method "test.Foo.qux"]];
  assert_reverse_call_graph
    ~context
    {|
    class Foo:
      def __init__(self):
        pass

      def baz(self):
        return self.bar()

      def qux(self):
        return self.bar()

      def bar(self):
        return self.qux()
    |}
    ~expected:
      [
        `Method "test.Foo.bar", [`Method "test.Foo.qux"; `Method "test.Foo.baz"];
        `Method "test.Foo.qux", [`Method "test.Foo.bar"];
      ]


let test_type_collection context =
  let assert_type_collection source ~handle ~expected =
    let source, environment =
      let project = ScratchProject.setup ~context [handle, source] in
      let { ScratchProject.BuiltTypeEnvironment.type_environment = environment; _ } =
        ScratchProject.build_type_environment project
      in
      let source =
        AstEnvironment.ReadOnly.get_processed_source
          (TypeEnvironment.ReadOnly.ast_environment environment)
          (Reference.create (String.chop_suffix_exn handle ~suffix:".py"))
        |> fun option -> Option.value_exn option
      in
      source, environment
    in
    let defines =
      Preprocessing.defines ~include_toplevels:true source
      |> List.map ~f:(fun { Node.value; _ } -> value)
    in
    let { Define.signature = { name; _ }; body = statements; _ } = List.nth_exn defines 2 in
    let lookup =
      TypeEnvironment.ReadOnly.get_local_annotations environment name
      |> fun value -> Option.value_exn value
    in
    let test_expect (node_id, statement_index, test_expression, expected_type) =
      let statement_key = [%hash: int * int] (node_id, statement_index) in
      let annotation_store =
        LocalAnnotationMap.ReadOnly.get_precondition lookup ~statement_key
        |> fun value -> Option.value_exn value
      in
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
      let resolution =
        TypeCheck.resolution global_resolution ~annotation_store (module TypeCheck.DummyContext)
      in

      let statement = List.nth_exn statements statement_index in
      Visit.collect_calls_and_names statement
      |> List.filter ~f:Expression.has_identifier_base
      |> List.hd_exn
      |> fun expression ->
      if String.equal (Expression.show expression) test_expression then
        match Resolution.resolve_expression_to_type resolution expression with
        | Type.Callable { Type.Callable.kind = Type.Callable.Named callable_type; _ } ->
            assert_equal expected_type (Reference.show callable_type)
        | _ -> assert false
    in
    List.iter expected ~f:test_expect
  in
  assert_type_collection
    {|
        class A:
          def foo(self) -> int:
            return 1

        class B:
          def foo(self) -> int:
            return 2

        class X:
          def caller(self):
            a = A()
            a.foo()
            a = B()
            a.foo()
        |}
    ~handle:"test1.py"
    ~expected:
      [4, 1, "$local_0$a.foo.(...)", "test1.A.foo"; 4, 3, "$local_0$a.foo.(...)", "test1.B.foo"];
  assert_type_collection
    {|
       class A:
         def foo(self) -> int:
           return 1

       class B:
         def foo(self) -> A:
           return A()

       class X:
         def caller(self):
           a = B().foo().foo()
    |}
    ~handle:"test2.py"
    ~expected:[4, 0, "$local_0$a.foo.(...).foo.(...)", "test2.A.foo"]


let test_prune_callables _ =
  let open Ast in
  let open Interprocedural in
  let assert_pruned
      ~callgraph
      ~overrides
      ~project_callables
      ~external_callables
      ~expected_callables
      ~expected_dependencies
    =
    let create name =
      if String.is_prefix ~prefix:"O|" (Reference.show name) then
        Target.create_override (String.drop_prefix (Reference.show name) 2 |> Reference.create)
      else
        Target.create_method name
    in
    let callgraph =
      List.map callgraph ~f:(fun (key, values) ->
          ( Target.create_method (Reference.create key),
            List.map values ~f:(fun value -> create (Reference.create value)) ))
      |> CallGraph.WholeProgramCallGraph.of_alist_exn
    in
    let overrides =
      List.map overrides ~f:(fun (key, values) ->
          ( Target.create_method (Reference.create key),
            List.map values ~f:(fun value -> Reference.create value) ))
      |> OverrideGraph.Heap.of_alist_exn
    in
    let project_callables =
      List.map ~f:(fun name -> name |> Reference.create |> Target.create_method) project_callables
    in
    let external_callables =
      List.map ~f:(fun name -> name |> Reference.create |> Target.create_method) external_callables
    in
    let initial_callables =
      {
        FetchCallables.internals = project_callables;
        callables = List.rev_append project_callables external_callables;
        stubs = [];
      }
    in
    let overrides = DependencyGraph.Reversed.from_overrides overrides in
    let dependencies =
      DependencyGraph.Reversed.from_call_graph callgraph
      |> DependencyGraph.Reversed.disjoint_union overrides
    in
    let {
      DependencyGraph.Reversed.reverse_dependency_graph = actual_dependencies;
      callables_kept = actual_callables;
    }
      =
      DependencyGraph.Reversed.prune dependencies ~initial_callables
    in
    let actual_dependencies = DependencyGraph.Reversed.to_target_graph actual_dependencies in
    assert_equal
      ~cmp:(List.equal Target.equal)
      ~printer:(List.to_string ~f:Target.show_pretty)
      (List.map expected_callables ~f:(fun callable -> create (Reference.create callable)))
      actual_callables;
    assert_equal
      ~cmp:
        (List.equal (fun (left_key, left_values) (right_key, right_values) ->
             Target.equal left_key right_key && List.equal Target.equal left_values right_values))
      ~printer:(fun graph ->
        graph
        |> List.map ~f:(fun (key, values) ->
               Format.asprintf
                 "%a -> %s"
                 Target.pp_pretty
                 key
                 (List.to_string values ~f:Target.show_pretty))
        |> String.concat ~sep:"\n")
      (List.map expected_dependencies ~f:(fun (key, values) ->
           ( create (Reference.create key),
             List.map values ~f:(fun value -> create (Reference.create value)) )))
      (Target.Map.to_alist actual_dependencies)
  in
  (* Basic case. *)
  assert_pruned
    ~callgraph:
      ["a.foo", ["external.bar"]; "external.bar", []; "external.test.test_bar", ["external.bar"]]
    ~overrides:[]
    ~project_callables:["a.foo"]
    ~external_callables:["external.bar"; "external.test.test_bar"]
    ~expected_callables:["a.foo"; "external.bar"]
    ~expected_dependencies:["a.foo", ["external.bar"]; "external.bar", []];

  (* Transitive case. *)
  assert_pruned
    ~callgraph:
      [
        "a.foo", ["external.bar"];
        "external.bar", ["external.baz"];
        "external.baz", [];
        "external.test.test_baz", ["external.baz"];
        "external.test.test_bar", ["external.bar"];
      ]
    ~overrides:[]
    ~project_callables:["a.foo"]
    ~external_callables:
      ["external.bar"; "external.baz"; "external.test.test_bar"; "external.test.test_baz"]
    ~expected_callables:["a.foo"; "external.bar"; "external.baz"]
    ~expected_dependencies:
      ["a.foo", ["external.bar"]; "external.bar", ["external.baz"]; "external.baz", []];
  (* Basic override. *)
  assert_pruned
    ~callgraph:
      [
        "a.foo", ["external.bar"];
        "external.bar", ["O|external.C.m"];
        "external.C.m", [];
        "external.D.m", ["external.called_by_override"];
        "external.called_by_override", [];
        "external.unrelated", [];
      ]
    ~overrides:["external.C.m", ["external.D"]; "external.D.m", []]
    ~project_callables:["a.foo"]
    ~external_callables:
      [
        "external.bar";
        "external.C.m";
        "external.D.m";
        "external.called_by_override";
        "external.unrelated";
      ]
    ~expected_callables:
      [
        "a.foo";
        "external.bar";
        "O|external.C.m";
        "external.C.m";
        "O|external.D.m";
        "external.D.m";
        "external.called_by_override";
      ]
    ~expected_dependencies:
      [
        "a.foo", ["external.bar"];
        "external.bar", ["O|external.C.m"];
        "external.called_by_override", [];
        "external.C.m", [];
        "external.D.m", ["external.called_by_override"];
        "O|external.C.m", ["O|external.D.m"; "external.C.m"];
        "O|external.D.m", ["external.D.m"];
      ];
  (* The calls go away if we don't have the override between C and D. *)
  assert_pruned
    ~callgraph:
      [
        "a.foo", ["external.bar"];
        "external.bar", ["external.C.m"];
        "external.C.m", [];
        "external.D.m", ["external.called_by_override"];
        "external.called_by_override", [];
        "external.unrelated", [];
      ]
    ~overrides:[]
    ~project_callables:["a.foo"]
    ~external_callables:
      [
        "external.bar";
        "external.C.m";
        "external.D.m";
        "external.called_by_override";
        "external.unrelated";
      ]
    ~expected_callables:["a.foo"; "external.bar"; "external.C.m"]
    ~expected_dependencies:
      ["a.foo", ["external.bar"]; "external.bar", ["external.C.m"]; "external.C.m", []];

  (* Transitive overrides. *)
  assert_pruned
    ~callgraph:
      [
        "a.foo", ["O|external.C.m"];
        "external.C.m", [];
        "external.D.m", [];
        "external.E.m", ["external.called_by_override"];
        "external.called_by_override", [];
        "external.unrelated", [];
      ]
    ~overrides:["external.C.m", ["external.D"]; "external.D.m", ["external.E"]]
    ~project_callables:["a.foo"]
    ~external_callables:
      [
        "external.C.m";
        "external.D.m";
        "external.E.m";
        "external.called_by_override";
        "external.unrelated";
      ]
    ~expected_callables:
      [
        "a.foo";
        "O|external.C.m";
        "external.C.m";
        "O|external.D.m";
        "external.D.m";
        "O|external.E.m";
        "external.E.m";
        "external.called_by_override";
      ]
    ~expected_dependencies:
      [
        "a.foo", ["O|external.C.m"];
        "external.called_by_override", [];
        "external.C.m", [];
        "external.D.m", [];
        "external.E.m", ["external.called_by_override"];
        "O|external.C.m", ["O|external.D.m"; "external.C.m"];
        "O|external.D.m", ["O|external.E.m"; "external.D.m"];
        "O|external.E.m", ["external.E.m"];
      ];
  (* Strongly connected components are handled fine. *)
  assert_pruned
    ~callgraph:
      [
        "a.foo", ["external.a"];
        "external.a", ["external.b"];
        "external.b", ["external.c"];
        "external.c", ["external.a"];
        "external.d", ["external.e"];
        "external.e", ["external.f"];
        "external.f", ["external.d"];
      ]
    ~overrides:[]
    ~project_callables:["a.foo"]
    ~external_callables:
      ["external.a"; "external.b"; "external.c"; "external.d"; "external.e"; "external.f"]
    ~expected_callables:["a.foo"; "external.a"; "external.b"; "external.c"]
    ~expected_dependencies:
      [
        "a.foo", ["external.a"];
        "external.a", ["external.b"];
        "external.b", ["external.c"];
        "external.c", ["external.a"];
      ];
  ()


let () =
  Scheduler.Daemon.check_entry_point ();
  "callGraph"
  >::: [
         "type_collection" >:: test_type_collection;
         "build" >:: test_construction;
         "build_reverse" >:: test_construction_reverse;
         "prune_callables" >:: test_prune_callables;
       ]
  |> Test.run
