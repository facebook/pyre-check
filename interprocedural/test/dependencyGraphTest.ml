(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
  let sources, _, environment = ScratchProject.build_environment project in
  let source =
    List.find_exn sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } ->
        String.equal relative handle)
  in
  ScratchProject.configuration_of project, source, environment


let create_call_graph ?(update_environment_with = []) ~context source_text =
  let configuration, source, environment =
    setup ~update_environment_with ~context ~handle:"test.py" source_text
  in
  let errors = TypeCheck.run ~configuration ~environment ~source in
  let record_overrides overrides =
    let record_override_edge ~key:member ~data:subtypes =
      DependencyGraphSharedMemory.add_overriding_types ~member ~subtypes
    in
    Reference.Map.iteri overrides ~f:record_override_edge
  in
  DependencyGraph.create_overrides ~environment ~source |> record_overrides;
  if not (List.is_empty errors) then
    Format.asprintf
      "Type errors in %s\n%a"
      source_text
      (Format.pp_print_list TypeCheck.Error.pp)
      errors
    |> failwith;
  DependencyGraph.create_callgraph ~environment ~source


let create_callable = function
  | `Function name -> !&name |> Callable.create_function
  | `Method name -> !&name |> Callable.create_method
  | `Override name -> !&name |> Callable.create_override


let compare_dependency_graph call_graph ~expected =
  let expected =
    let map_callee_callers (callee, callers) =
      ( create_callable callee,
        List.map callers ~f:create_callable |> List.sort ~compare:Callable.compare )
    in
    List.map expected ~f:map_callee_callers
  in
  let call_graph =
    List.map call_graph ~f:(fun (callee, callers) ->
        callee, List.sort callers ~compare:Callable.compare)
  in
  let printer call_graph =
    Sexp.to_string [%message (call_graph : (Callable.t * Callable.t list) list)]
  in
  assert_equal ~printer expected call_graph


let assert_call_graph ?update_environment_with ~context source ~expected =
  let graph =
    create_call_graph ?update_environment_with ~context source
    |> DependencyGraph.from_callgraph
    |> Callable.Map.to_alist
  in
  compare_dependency_graph graph ~expected


let assert_reverse_call_graph ~context source ~expected =
  let graph =
    create_call_graph ~context source
    |> DependencyGraph.from_callgraph
    |> DependencyGraph.reverse
    |> Callable.Map.to_alist
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
      [`Method "test.A.__init__", []; `Method "test.B.__init__", [`Method "test.A.__init__"]];
  assert_call_graph
    ~update_environment_with:
      [{ handle = "foobar.pyi"; source = {|
            def bar(x: str) -> str: ...
          |} }]
    {|
     def foo():
       foobar.bar("foo")
    |}
    ~expected:[`Function "test.foo", [`Function "foobar.bar"]];
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
    ~expected:[`Function "test.foo", [`Function "bar.baz.qux.derp"]];
  assert_call_graph
    {|
       class Base:
         def foo(self) -> None: ...
       class C(Base):
         pass
       def call_foo(c: C) -> None:
         c.foo()
    |}
    ~expected:[`Function "test.call_foo", [`Method "test.Base.foo"]];
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
    ~expected:[`Function "test.call_foo", [`Method "test.Base.foo"; `Method "test.D.foo"]];

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
    ~expected:[`Function "test.call_foo", [`Method "test.Base.foo"; `Method "test.D.foo"]];

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
    ~expected:[`Function "test.call_foo", [`Override "test.Child.foo"; `Method "test.Base.foo"]];
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
    ~expected:[`Function "test.format_str", [`Override "str.format"]];

  assert_call_graph
    {|
      def foo() -> None:
        def bar() -> None:
          "ASD".format("ASD").lower()
        bar()
    |}
    ~expected:[`Function "test.foo", []];
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
    let configuration, source, environment =
      let project = ScratchProject.setup ~context [handle, source] in
      let _, ast_environment, environment = ScratchProject.build_environment project in
      let source =
        AstEnvironment.ReadOnly.get_source
          (AstEnvironment.read_only ast_environment)
          (Reference.create (String.chop_suffix_exn handle ~suffix:".py"))
        |> fun option -> Option.value_exn option
      in
      ScratchProject.configuration_of project, source, environment
    in
    TypeCheck.run ~configuration ~environment ~source |> ignore;
    let defines =
      Preprocessing.defines ~include_toplevels:true source
      |> List.map ~f:(fun { Node.value; _ } -> value)
    in
    let { Define.signature = { name; _ }; body = statements; _ } = List.nth_exn defines 2 in
    let lookup = ResolutionSharedMemory.get name |> fun value -> Option.value_exn value in
    let test_expect (node_id, statement_index, test_expression, expected_type) =
      let key = [%hash: int * int] (node_id, statement_index) in
      let annotations =
        LocalAnnotationMap.get_precondition lookup key |> fun value -> Option.value_exn value
      in
      let global_resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
      let resolution = TypeCheck.resolution global_resolution ~annotations () in
      let statement = List.nth_exn statements statement_index in
      Visit.collect_calls_and_names statement
      |> List.filter ~f:Expression.has_identifier_base
      |> List.hd_exn
      |> fun expression ->
      if String.equal (Expression.show expression) test_expression then
        let module State = TypeCheck.State (struct
          let configuration = configuration

          let define = +Test.mock_define

          module Builder = Dependencies.Callgraph.DefaultBuilder
        end)
        in
        let state = State.create ~resolution () in
        let { State.resolved; _ } = State.forward_expression ~state ~expression in
        match resolved with
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
      [5, 1, "$local_0$a.foo.(...)", "test1.A.foo"; 5, 3, "$local_0$a.foo.(...)", "test1.B.foo"];
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
    ~expected:[5, 0, "$local_0$a.foo.(...).foo.(...)", "test2.A.foo"]


let test_method_overrides context =
  let assert_method_overrides
      ?(update_environment_with = [])
      ?(handle = "test.py")
      source
      ~expected
    =
    let expected =
      let create_callables (member, overriding_types) =
        !&member, List.map overriding_types ~f:Reference.create
      in
      List.map expected ~f:create_callables
    in
    let _, source, environment = setup ~update_environment_with ~context ~handle source in
    let overrides_map = DependencyGraph.create_overrides ~environment ~source in
    let expected_overrides = Reference.Map.of_alist_exn expected in
    let equal_elements = List.equal Reference.equal in
    let printer map =
      map |> Reference.Map.sexp_of_t (List.sexp_of_t Reference.sexp_of_t) |> Sexp.to_string
    in
    assert_equal
      ~cmp:(Reference.Map.equal equal_elements)
      ~printer
      expected_overrides
      overrides_map
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
    let configuration, source, environment = setup ~context ~handle source in
    TypeCheck.run ~configuration ~environment ~source |> ignore;
    let partitions =
      let edges =
        DependencyGraph.create_callgraph ~environment ~source |> DependencyGraph.from_callgraph
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
    ~expected:[[`Method "s0.Foo.__init__"]; [`Method "s0.Foo.c1"]; [`Method "s0.Foo.c2"]];
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
        [`Method "s2.Foo.__init__"];
        [`Method "s2.Bar.__init__"];
        [`Method "s2.Foo.c1"; `Method "s2.Foo.c2"];
        [`Method "s2.Bar.c1"];
        [`Method "s2.Bar.c2"; `Method "s2.Foo.c3"];
      ]


let () =
  Scheduler.Daemon.check_entry_point ();
  "callGraph"
  >::: [
         "type_collection" >:: test_type_collection;
         "build" >:: test_construction;
         "build_reverse" >:: test_construction_reverse;
         "overrides" >:: test_method_overrides;
         "strongly_connected_components" >:: test_strongly_connected_components;
       ]
  |> Test.run
