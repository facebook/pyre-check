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

let setup ?(other_sources = []) ~context ~handle source =
  let project =
    let external_sources = List.map other_sources ~f:(fun { handle; source } -> handle, source) in
    ScratchProject.setup ~context ~external_sources [handle, source]
  in
  let pyre_api = ScratchProject.pyre_pysa_read_only_api project in
  let { ScratchProject.BuiltTypeEnvironment.sources; type_environment; _ } =
    ScratchProject.build_type_environment project
  in
  let source =
    List.find_exn sources ~f:(fun { Source.module_path; _ } ->
        String.equal (ModulePath.relative module_path) handle)
  in
  source, pyre_api, type_environment, ScratchProject.configuration_of project


let create_call_graph ?(other_sources = []) ~context source_text =
  let module_name, handle = !&"test", "test.py" in
  let source, pyre_api, environment, configuration =
    setup ~other_sources ~context ~handle source_text
  in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create
      ~maximum_target_depth:Configuration.StaticAnalysis.default_maximum_target_depth
      configuration
      ()
  in
  let override_graph = OverrideGraph.Heap.from_source ~pyre_api ~source in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph in
  let () =
    let errors = TypeEnvironment.ReadOnly.get_errors environment module_name in
    if not (List.is_empty errors) then
      Format.asprintf
        "Type errors in %s\n%a"
        source_text
        (Format.pp_print_list TypeCheck.Error.pp)
        errors
      |> failwith
  in
  let initial_callables = FetchCallables.from_source ~configuration ~pyre_api ~source in
  let definitions = FetchCallables.get_definitions initial_callables in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let definitions_and_stubs =
    Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
  in
  let callables_to_definitions_map =
    Interprocedural.Target.CallablesSharedMemory.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let fold call_graph callable =
    let callees =
      CallGraph.call_graph_of_callable
        ~static_analysis_configuration
        ~pyre_api
        ~override_graph:
          (Some (Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
        ~attribute_targets:(Target.HashSet.create ())
        ~decorators:
          (Interprocedural.CallGraph.CallableToDecoratorsMap.SharedMemory.empty ()
          |> Interprocedural.CallGraph.CallableToDecoratorsMap.SharedMemory.read_only)
        ~callables_to_definitions_map:
          (Interprocedural.Target.CallablesSharedMemory.read_only callables_to_definitions_map)
        ~check_invariants:true
        ~callable
      |> CallGraph.DefineCallGraph.all_targets
           ~use_case:CallGraph.AllTargetsUseCase.TaintAnalysisDependency
    in
    CallGraph.WholeProgramCallGraph.add_or_exn call_graph ~callable ~callees
  in
  let call_graph = List.fold ~init:CallGraph.WholeProgramCallGraph.empty ~f:fold definitions in
  let () = OverrideGraph.SharedMemory.cleanup override_graph_shared_memory in
  let () = Target.CallablesSharedMemory.cleanup callables_to_definitions_map in
  call_graph


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


let assert_call_graph ?other_sources ~context source ~expected =
  let graph =
    create_call_graph ?other_sources ~context source
    |> DependencyGraph.Reversed.from_call_graph
    |> DependencyGraph.Reversed.to_target_graph
    |> TargetGraph.to_alist ~sorted:true
  in
  compare_dependency_graph graph ~expected


let assert_reverse_call_graph ~context source ~expected =
  let graph =
    create_call_graph ~context source
    |> DependencyGraph.Reversed.from_call_graph
    |> DependencyGraph.Reversed.reverse
    |> DependencyGraph.to_target_graph
    |> TargetGraph.to_alist ~sorted:true
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
    ~other_sources:
      [{ handle = "foobar.pyi"; source = {|
            def bar(x: str) -> str: ...
          |} }]
    {|
     def foo():
       foobar.bar("foo")
    |}
    ~expected:[`Function "test.$toplevel", []; `Function "test.foo", [`Function "foobar.bar"]];
  assert_call_graph
    ~other_sources:
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
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Function "test.call_foo", [`Method "test.Base.foo"];
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
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Method "test.E.$class_toplevel", [];
        `Function "test.call_foo", [`Method "test.Base.foo"; `Method "test.D.foo"];
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
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Method "test.UnrelatedToC.$class_toplevel", [];
        `Function "test.call_foo", [`Method "test.Base.foo"; `Method "test.D.foo"];
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
        `Method "test.Base.$class_toplevel", [];
        `Method "test.C.$class_toplevel", [];
        `Method "test.Child.$class_toplevel", [];
        `Method "test.Grandchild.$class_toplevel", [];
        `Function "test.call_foo", [`Override "test.Child.foo"; `Method "test.Base.foo"];
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
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Method "test.E.$class_toplevel", [];
        `Function "test.calls_c", [`Override "test.C.foo"];
        `Function "test.calls_d", [`Method "test.E.foo"; `Method "test.C.foo"];
        `Function "test.calls_e", [`Method "test.E.foo"];
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
        `Method "test.C.$class_toplevel", [];
        `Function "test.format_str", [`Override "str.format"];
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
        `Function "test.$toplevel", [];
        `Function "test.foo", [`Function "test.foo.bar"];
        `Function "test.foo.bar", [`Method "str.lower"; `Method "str.format"];
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
        `Method "test.C.$class_toplevel", [];
        `Method "test.D.$class_toplevel", [];
        `Function "test.calls_C_int", [`Override "test.C.method"];
        `Function "test.calls_C_str", [`Override "test.C.method"];
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
    let module_name = Reference.create (String.chop_suffix_exn handle ~suffix:".py") in
    let source, environment =
      let project = ScratchProject.setup ~context [handle, source] in
      let { ScratchProject.BuiltTypeEnvironment.type_environment = environment; _ } =
        ScratchProject.build_type_environment project
      in
      let source =
        SourceCodeApi.source_of_qualifier
          (TypeEnvironment.ReadOnly.get_untracked_source_code_api environment)
          module_name
        |> fun option -> Option.value_exn option
      in
      source, environment
    in
    let defines = Preprocessing.defines ~include_toplevels:true source in
    let ({ Node.value = { Define.body = statements; _ }; _ } as define) = List.nth_exn defines 2 in
    let define_name =
      FunctionDefinition.qualified_name_of_define ~module_name (Node.value define)
    in
    let lookup =
      TypeEnvironment.ReadOnly.get_local_annotations environment define_name (Node.location define)
      |> fun value -> Option.value_exn value
    in
    let test_expect (node_id, statement_index, test_expression, expected_type) =
      let statement_key = [%hash: int * int] (node_id, statement_index) in
      let type_info_store =
        TypeInfo.ForFunctionBody.ReadOnly.get_precondition lookup ~statement_key
        |> fun value -> Option.value_exn value
      in
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
      let resolution =
        TypeCheck.resolution global_resolution ~type_info_store (module TypeCheck.DummyContext)
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
  let assert_pruned
      ~callgraph
      ~overrides
      ~project_callables
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
      DependencyGraph.Reversed.prune dependencies ~callables_to_analyze:project_callables
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
      (TargetGraph.to_alist ~sorted:true actual_dependencies)
  in
  (* Basic case. *)
  assert_pruned
    ~callgraph:
      ["a.foo", ["external.bar"]; "external.bar", []; "external.test.test_bar", ["external.bar"]]
    ~overrides:[]
    ~project_callables:["a.foo"]
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
        "external.C.m", [];
        "O|external.C.m", ["external.C.m"; "O|external.D.m"];
        "external.D.m", ["external.called_by_override"];
        "O|external.D.m", ["external.D.m"];
        "external.bar", ["O|external.C.m"];
        "external.called_by_override", [];
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
    ~expected_callables:["a.foo"; "external.bar"; "external.C.m"]
    ~expected_dependencies:
      ["a.foo", ["external.bar"]; "external.C.m", []; "external.bar", ["external.C.m"]];

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
        "external.C.m", [];
        "O|external.C.m", ["external.C.m"; "O|external.D.m"];
        "external.D.m", [];
        "O|external.D.m", ["external.D.m"; "O|external.E.m"];
        "external.E.m", ["external.called_by_override"];
        "O|external.E.m", ["external.E.m"];
        "external.called_by_override", [];
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
  "callGraph"
  >::: [
         "type_collection" >:: test_type_collection;
         "build" >:: test_construction;
         "build_reverse" >:: test_construction_reverse;
         "prune_callables" >:: test_prune_callables;
       ]
  |> Test.run
