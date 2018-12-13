(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Interprocedural
open Statement

open Test


let parse_source ?(qualifier=[]) ?handle source =
  parse ~qualifier source ?handle
  |> Preprocessing.preprocess


let create_call_graph ?(update_environment_with = []) source_text =
  let source = parse_source source_text in
  let configuration = Test.mock_configuration in
  let environment = Test.environment ~configuration () in
  let sources =
    source
    :: List.map
      update_environment_with
      ~f:(fun { qualifier; handle; source } -> parse_source ~qualifier ~handle source)
  in
  Service.Environment.populate ~configuration environment sources;
  let type_errors = TypeCheck.check ~configuration ~environment ~source in
  if not (List.is_empty type_errors.errors) then
    Log.dump "Type errors in %s\n%a"
      source_text
      (Format.pp_print_list TypeCheck.Error.pp)
      type_errors.errors;
  DependencyGraph.create_callgraph ~environment ~source


let create_callable = function
  | `Function name ->
      Access.create name
      |> Callable.create_function
  | `Method name ->
      Access.create name
      |> Callable.create_method


let compare_dependency_graph call_graph ~expected =
  let expected =
    let map_callee_callers (callee, callers) =
      create_callable callee, List.map callers ~f:create_callable in
    List.map expected ~f:map_callee_callers
  in
  let printer call_graph =
    Sexp.to_string [%message (call_graph : (Callable.t * Callable.t list) list)]
  in
  assert_equal ~printer expected call_graph


let assert_call_graph ?update_environment_with source ~expected =
  let graph =
    create_call_graph ?update_environment_with source
    |> DependencyGraph.from_callgraph
    |> Callable.Map.to_alist
  in
  compare_dependency_graph graph ~expected


let assert_reverse_call_graph source ~expected =
  let graph =
    create_call_graph source
    |> DependencyGraph.from_callgraph
    |> DependencyGraph.reverse
    |> Callable.Map.to_alist
  in
  compare_dependency_graph graph ~expected


let test_construction _ =
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
    ~expected:[
      `Method "Foo.__init__", [];
      `Method "Foo.bar", [];
      `Method "Foo.qux", [`Method "Foo.bar"];
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
        `Method "Foo.__init__", [];
        `Method "Foo.bar", [`Method "Foo.qux"];
        `Method "Foo.qux", [`Method "Foo.bar"]
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
    ~expected:[
      `Method "A.__init__", [];
      `Method "B.__init__", [`Method "A.__init__"];
    ];

  assert_call_graph
    ~update_environment_with: [
      {
        qualifier = Access.create "foobar";
        handle = "foobar.pyi";
        source =
          {|
            def bar(x: string) -> str: ...
          |};
      }
    ]
    {|
     def foo():
       foobar.bar("foo")
    |}
    ~expected:[`Function "foo", [`Function "foobar.bar"]];

  assert_call_graph
    ~update_environment_with: [
      {
        qualifier = Access.create "bar.baz.qux";
        handle = "bar.baz.pyi";
        source =
          {|
            def derp() -> str: ...
          |};
      }
    ]
    {|
     from bar.baz import qux
     def foo():
       qux.derp()
    |}
    ~expected:[`Function "foo", [`Function "bar.baz.qux.derp"]]



let test_construction_reverse _ =
  assert_reverse_call_graph
    {|
    class Foo:
      def __init__(self):
        pass

      def bar(self):
        return 10

      def qux(self):
        return self.bar()
    |}
    ~expected:[`Method "Foo.bar", [`Method "Foo.qux"]];

  assert_reverse_call_graph
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
        `Method "Foo.bar", [`Method "Foo.qux"; `Method "Foo.baz"];
        `Method "Foo.qux", [`Method "Foo.bar"];
      ]


let test_type_collection _ =
  let assert_type_collection source ~qualifier ~expected =
    let source = parse_source ~qualifier source in
    let configuration = Test.mock_configuration in
    let environment = Test.environment ~configuration () in
    Service.Environment.populate ~configuration environment [source];
    TypeCheck.check ~configuration ~environment ~source |> ignore;
    let defines =
      Preprocessing.defines source ~extract_into_toplevel:true
      |> List.map ~f:(fun { Node.value; _ } -> value)
    in
    let { Define.name; body = statements; _ } = List.nth_exn defines 1 in
    let lookup =
      ResolutionSharedMemory.get name
      |> (fun value -> Option.value_exn value)
      |> Int.Map.of_tree
    in
    let test_expect (node_id, statement_index, test_access, expected_type) =
      let key = [%hash: int * int] (node_id, statement_index) in
      let test_access = Access.create test_access in
      let annotations =
        Map.find_exn lookup key
        |> (fun { ResolutionSharedMemory.precondition; _ } ->
            Access.Map.of_tree precondition)
      in
      let resolution = TypeCheck.resolution environment ~annotations () in
      let statement = List.nth_exn statements statement_index in
      Visit.collect_accesses_with_location statement
      |> List.hd_exn
      |> fun { Node.value = access; _ } ->
      if String.equal (Access.show access) (Access.show test_access) then
        let open Annotated in
        let open Access in
        let last_element =
          Annotated.Access.create access
          |> Annotated.Access.last_element ~resolution
        in
        match last_element with
        | Signature {
            signature =
              Signature.Found {
                callable = { Type.Callable.kind = Type.Callable.Named callable_type; _ };
                _;
              };
            _;
          } ->
            assert_equal expected_type (Expression.Access.show callable_type)
        | _ ->
            assert false
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
    ~qualifier:(Access.create "test1")
    ~expected:
      [
        (5, 1, "$local_0$a.foo.(...)", "test1.A.foo");
        (5, 3, "$local_0$a.foo.(...)", "test1.B.foo")
      ];

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
    ~qualifier:(Access.create "test2")
    ~expected:[(5, 0, "$local_0$a.foo.(...).foo.(...)", "test2.A.foo")]


let test_method_overrides _ =
  let assert_method_overrides source ~expected =
    let expected =
      let create_callables (member, overriding_types) =
        Access.create member, List.map overriding_types ~f:Access.create
      in
      List.map expected ~f:create_callables
    in
    let source = parse_source source in
    let configuration = Test.mock_configuration in
    let environment = Test.environment ~configuration () in
    Service.Environment.populate ~configuration environment [source];
    let overrides_map = DependencyGraph.create_overrides ~environment ~source in
    let expected_overrides = Access.Map.of_alist_exn expected in
    let equal_elements = List.equal ~equal:Access.equal in
    let printer map =
      map
      |> Access.Map.sexp_of_t (List.sexp_of_t Access.sexp_of_t)
      |> Sexp.to_string
    in
    assert_equal
      ~cmp:(Access.Map.equal equal_elements)
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
    ~expected:
      [
        "Bar.foo", ["Baz"];
        "Foo.foo", ["Bar"; "Qux"];
      ]


let test_strongly_connected_components _ =
  let assert_strongly_connected_components source ~qualifier ~expected =
    let qualifier = Access.create qualifier in
    let expected = List.map expected ~f:(List.map ~f:create_callable) in
    let source = parse_source ~qualifier source in
    let configuration = Test.mock_configuration in
    let environment = Test.environment ~configuration () in
    Service.Environment.populate ~configuration environment [source];
    TypeCheck.check ~configuration ~environment ~source |> ignore;
    let partitions =
      let edges =
        DependencyGraph.create_callgraph ~environment ~source
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
    ~qualifier:"s0"
    ~expected:
      [
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
    ~qualifier:"s1"
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
    ~qualifier:"s2"
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
  "callGraph">:::[
    "type_collection">::test_type_collection;
    "build">::test_construction;
    "build_reverse">::test_construction_reverse;
    "overrides">::test_method_overrides;
    "strongly_connected_components">::test_strongly_connected_components;
  ]
  |> Test.run
