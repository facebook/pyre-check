(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Statement
open TypeCheck

open Test


let parse_source ?(qualifier=[]) source =
  parse ~qualifier source
  |> Preprocessing.preprocess


let create_call_graph source =
  let source = parse_source source in
  let configuration = Test.mock_configuration in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate environment [source];
  check configuration environment source |> ignore;
  CallGraph.create ~environment ~source


let compare_call_graph call_graph ~expected =
  let expected =
    let map_callee_callers (callee, callers) =
      Access.create callee, List.map callers ~f:Access.create in
    List.map expected ~f:map_callee_callers
  in
  let printer call_graph =
    Sexp.to_string [%message (call_graph : (Access.t * Access.t list) list)]
  in
  assert_equal ~printer expected call_graph


let assert_call_graph source ~expected =
  let call_graph =
    create_call_graph source
    |> Access.Map.to_alist
  in
  compare_call_graph call_graph ~expected


let assert_reverse_call_graph source ~expected =
  let call_graph =
    create_call_graph source
    |> CallGraph.reverse
    |> Access.Map.to_alist
  in
  compare_call_graph call_graph ~expected


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
    ~expected:["Foo.qux", ["Foo.bar"]];

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
        "Foo.bar", ["Foo.qux"];
        "Foo.qux", ["Foo.bar"]
      ];

  assert_call_graph
    {|
     class A:
       def __init__(self) -> A:
         return self

     class B:
       def __init__(self) -> A:
         return A()
    |}
    ~expected:["B.__init__", ["A.__init__"]];

  assert_call_graph
    {|
     def foo():
       foobar.bar("foo")
    |}
    ~expected:["foo", ["foobar.bar"]];

  assert_call_graph
    {|
     from bar.baz import qux
     def foo():
       qux.derp()
    |}
    ~expected:["foo", ["bar.baz.qux.derp"]]



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
    ~expected:["Foo.bar", ["Foo.qux"]];

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
        "Foo.bar", ["Foo.qux"; "Foo.baz"];
        "Foo.qux", ["Foo.bar"];
      ]


let test_type_collection _ =
  let assert_type_collection source ~qualifier ~expected =
    let source = parse_source ~qualifier source in
    let configuration = Test.mock_configuration in
    let environment = Test.environment ~configuration () in
    Service.Environment.populate environment [source];
    check configuration environment source |> ignore;
    let defines =
      Preprocessing.defines source ~extract_into_toplevel:true
      |> List.map ~f:(fun { Node.value; _ } -> value)
    in
    let { Define.name; body = statements; _ } = List.nth_exn defines 1 in
    let lookup =
      TypeResolutionSharedMemory.get name
      |> (fun value -> Option.value_exn value)
      |> Int.Map.Tree.fold ~init:Int.Map.empty ~f:(fun ~key ~data -> Int.Map.set ~key ~data)
    in
    let test_expect (node_id, statement_index, test_access, expected_type) =
      let key = [%hash: int * int] (node_id, statement_index) in
      let test_access = Access.create test_access in
      let annotations =
        Map.find_exn lookup key
        |> Access.Map.of_tree
      in
      let resolution = Environment.resolution environment ~annotations () in
      let statement = List.nth_exn statements statement_index in
      Visit.collect_accesses_with_location statement
      |> List.hd_exn
      |> fun { Node.value = access; _ } ->
      if String.equal (Access.show access) (Access.show test_access) then
        let open Annotated in
        let open Access.Element in
        let last_element =
          Annotated.Access.create access
          |> Annotated.Access.last_element ~resolution
        in
        match last_element with
        | Signature {
            signature =
              Signature.Found {
                Signature.callable = {
                  Type.Callable.kind = Type.Callable.Named callable_type;
                  _;
                };
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
      let create_accesses (access, accesses) =
        Access.create access, List.map accesses ~f:Access.create
      in
      List.map expected ~f:create_accesses
    in
    let source = parse_source source in
    let configuration = Test.mock_configuration in
    let environment = Test.environment ~configuration () in
    Service.Environment.populate environment [source];
    let overrides_map = Service.StaticAnalysis.overrides_of_source ~environment ~source in
    let expected_overrides = Access.Map.of_alist_exn expected in
    let equal_elements = List.equal ~equal:Access.equal in
    assert_equal
      ~cmp:(Access.Map.equal equal_elements)
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
        "Bar.foo", ["Baz.foo"];
        "Foo.foo", ["Bar.foo"; "Qux.foo"]
      ]


let test_strongly_connected_components _ =
  let assert_strongly_connected_components source ~qualifier ~expected =
    let qualifier = Access.create qualifier in
    let expected = List.map expected ~f:(List.map ~f:Access.create) in
    let source = parse_source ~qualifier source in
    let configuration = Test.mock_configuration in
    let environment = Test.environment ~configuration () in
    Service.Environment.populate environment [source];
    check configuration environment source |> ignore;
    let partitions =
      let edges = CallGraph.create ~environment ~source in
      CallGraph.partition ~edges
    in
    let printer partitions = Format.asprintf "%a" CallGraph.pp_partitions partitions in
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
        ["s0.Foo.c1"];
        ["s0.Foo.c2"];
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
        return self.c3()

      def c5(self):
        return self.c5()
    |}
    ~qualifier:"s1"
    ~expected:
      [
        ["s1.Foo.c3"; "s1.Foo.c4"];
        ["s1.Foo.c2"; "s1.Foo.c1"];
        ["s1.Foo.c5"];
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
        pass

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
        ["s2.Foo.__init__"];
        ["s2.Foo.c1"; "s2.Foo.c2"];
        ["s2.Bar.c1"];
        ["s2.Bar.__init__"];
        ["s2.Bar.c2"; "s2.Foo.c3"];
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
