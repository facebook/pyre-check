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
    List.find_exn sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } ->
        String.equal relative handle)
  in
  source, TypeEnvironment.read_only type_environment, project.configuration


let create_call_graph ?(update_environment_with = []) ~context source_text =
  let source, environment, configuration =
    setup ~update_environment_with ~context ~handle:"test.py" source_text
  in
  let static_analysis_configuration = Configuration.StaticAnalysis.create configuration () in
  let record_overrides overrides =
    let record_override_edge ~key:member ~data:subtypes =
      DependencyGraphSharedMemory.add_overriding_types ~member ~subtypes
    in
    Reference.Map.iteri overrides ~f:record_override_edge
  in
  DependencyGraph.create_overrides ~environment ~source |> record_overrides;
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
  CallGraph.create_callgraph
    ~static_analysis_configuration
    ~store_shared_memory:false
    ~environment
    ~source


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
    |> DependencyGraph.from_callgraph
    |> Target.Map.to_alist
  in
  compare_dependency_graph graph ~expected


let assert_reverse_call_graph ~context source ~expected =
  let graph =
    create_call_graph ~context source
    |> DependencyGraph.from_callgraph
    |> DependencyGraph.reverse
    |> Target.Map.to_alist
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
          (TypeEnvironment.ast_environment environment |> AstEnvironment.read_only)
          (Reference.create (String.chop_suffix_exn handle ~suffix:".py"))
        |> fun option -> Option.value_exn option
      in
      source, TypeEnvironment.read_only environment
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


let test_method_overrides context =
  let assert_method_overrides ?(update_environment_with = []) ?(handle = "test.py") source ~expected
    =
    let expected =
      let create_callables (member, overriding_types) =
        !&member, List.map overriding_types ~f:Reference.create
      in
      List.map expected ~f:create_callables
    in
    let source, environment, _ = setup ~update_environment_with ~context ~handle source in
    let overrides_map = DependencyGraph.create_overrides ~environment ~source in
    let expected_overrides = Reference.Map.of_alist_exn expected in
    let equal_elements = List.equal Reference.equal in
    let printer map =
      map |> Reference.Map.sexp_of_t (List.sexp_of_t Reference.sexp_of_t) |> Sexp.to_string
    in
    assert_equal ~cmp:(Reference.Map.equal equal_elements) ~printer expected_overrides overrides_map
  in
  assert_method_overrides
    {|
      class Foo:
        def foo(): pass
      class Bar(Foo):
        def foo(): pass
      class Baz(Bar):
        def foo(): pass
        def baz(): pass
      class Qux(Foo):
        def foo(): pass
    |}
    ~expected:["test.Bar.foo", ["test.Baz"]; "test.Foo.foo", ["test.Bar"; "test.Qux"]];

  (* We don't register any overrides at all for classes in test files. *)
  assert_method_overrides
    {|
      class Foo:
        def foo(): pass
      class Bar(Foo):
        def foo(): pass
      class Test(unittest.case.TestCase):
        class Baz(Foo):
          def foo(): pass
    |}
    ~expected:[];
  assert_method_overrides
    ~update_environment_with:
      [
        {
          handle = "module.py";
          source =
            {|
        import module
        class Baz(module.Foo):
          def foo(): pass
      |};
        };
      ]
    ~handle:"test_module.py"
    {|
      import module
      class Test(unittest.case.TestCase):
        class Bar(module.Foo):
          def foo(): pass
    |}
    ~expected:[]


let test_strongly_connected_components context =
  let assert_strongly_connected_components source ~handle ~expected =
    let expected = List.map expected ~f:(List.map ~f:create_callable) in
    let source, environment, configuration = setup ~context ~handle source in
    let static_analysis_configuration = Configuration.StaticAnalysis.create configuration () in
    let partitions =
      let edges =
        CallGraph.create_callgraph
          ~static_analysis_configuration
          ~store_shared_memory:false
          ~environment
          ~source
        |> DependencyGraph.from_callgraph
      in
      DependencyGraph.partition ~edges
    in
    let printer partitions = Format.asprintf "%a" DependencyGraph.pp_partitions partitions in
    assert_equal ~printer expected partitions
  in
  assert_strongly_connected_components
    {|
    class Foo:
      def __init__(self):
        pass

      def c1(self):
        return self.c1()

      def c2(self):
        return self.c1()
    |}
    ~handle:"s0.py"
    ~expected:
      [
        [`Function "s0.$toplevel"];
        [`Method "s0.Foo.$class_toplevel"];
        [`Method "s0.Foo.__init__"];
        [`Method "s0.Foo.c1"];
        [`Method "s0.Foo.c2"];
      ];
  assert_strongly_connected_components
    {|
    class Foo:
      def __init__(self):
        pass

      def c1(self):
        return self.c2()

      def c2(self):
        return self.c1()

      def c3(self):
        return self.c4()

      def c4(self):
        return self.c3() + self.c2()

      def c5(self):
        return self.c5()
    |}
    ~handle:"s1.py"
    ~expected:
      [
        [`Function "s1.$toplevel"];
        [`Method "s1.Foo.$class_toplevel"];
        [`Method "s1.Foo.__init__"];
        [`Method "s1.Foo.c1"; `Method "s1.Foo.c2"];
        [`Method "s1.Foo.c3"; `Method "s1.Foo.c4"];
        [`Method "s1.Foo.c5"];
      ];
  assert_strongly_connected_components
    {|
    class Foo:
      def __init__(self):
        pass

      def c1(self):
        return self.c2()

      def c2(self):
        return self.c1()

      def c3(self):
        b = Bar()
        return b.c2()

    class Bar:
      def __init__(self):
        return Foo()

      def c1(self):
        f = Foo()
        return f.c1()

      def c2(self):
        f = Foo()
        return f.c3()
    |}
    ~handle:"s2.py"
    ~expected:
      [
        [`Function "s2.$toplevel"];
        [`Method "s2.Bar.$class_toplevel"];
        [`Method "object.__new__"];
        [`Method "s2.Foo.__init__"];
        [`Method "s2.Bar.__init__"];
        [`Method "s2.Foo.c1"; `Method "s2.Foo.c2"];
        [`Method "s2.Bar.c1"];
        [`Method "s2.Bar.c2"; `Method "s2.Foo.c3"];
        [`Method "s2.Foo.$class_toplevel"];
      ]


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
      |> Target.Map.of_alist_exn
    in
    let overrides =
      List.map overrides ~f:(fun (key, values) ->
          Reference.create key, List.map values ~f:(fun value -> Reference.create value))
      |> Reference.Map.of_alist_exn
    in
    let callables_with_dependency_information =
      List.map project_callables ~f:(fun callable ->
          Target.create_method (Reference.create callable), true)
      @ List.map external_callables ~f:(fun callable ->
            Target.create_method (Reference.create callable), false)
    in
    let overrides = DependencyGraph.from_overrides overrides in
    let dependencies =
      DependencyGraph.from_callgraph callgraph |> DependencyGraph.union overrides
    in
    let { DependencyGraph.dependencies = actual_dependencies; pruned_callables = actual_callables } =
      DependencyGraph.prune dependencies ~callables_with_dependency_information
    in
    assert_equal
      ~cmp:(List.equal Target.equal)
      ~printer:(List.to_string ~f:Target.show_pretty)
      (List.map expected_callables ~f:(fun callable ->
           Target.create_method (Reference.create callable)))
      actual_callables;
    assert_equal
      ~cmp:
        (List.equal (fun (left_key, left_values) (right_key, right_values) ->
             Target.equal left_key right_key && List.equal Target.equal left_values right_values))
      ~printer:
        (List.to_string ~f:(fun (key, values) ->
             Format.asprintf
               "%a: %s"
               Target.pp_pretty
               key
               (List.to_string values ~f:Target.show_pretty)))
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
      ["a.foo"; "external.bar"; "external.C.m"; "external.D.m"; "external.called_by_override"]
    ~expected_dependencies:
      [
        "a.foo", ["external.bar"];
        "external.bar", ["O|external.C.m"];
        "external.called_by_override", [];
        "external.C.m", [];
        "external.D.m", ["external.called_by_override"];
        "O|external.C.m", ["external.C.m"; "O|external.D.m"];
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
      ["a.foo"; "external.C.m"; "external.D.m"; "external.E.m"; "external.called_by_override"]
    ~expected_dependencies:
      [
        "a.foo", ["O|external.C.m"];
        "external.called_by_override", [];
        "external.C.m", [];
        "external.D.m", [];
        "external.E.m", ["external.called_by_override"];
        "O|external.C.m", ["external.C.m"; "O|external.D.m"];
        "O|external.D.m", ["external.D.m"; "O|external.E.m"];
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
         "overrides" >:: test_method_overrides;
         "strongly_connected_components" >:: test_strongly_connected_components;
         "prune_callables" >:: test_prune_callables;
       ]
  |> Test.run
