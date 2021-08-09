(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Test
open Interprocedural

let test_call_graph_of_define context =
  let assert_call_graph_of_define ~source ~define_name ~expected =
    let expected =
      let parse_location location =
        let parse_position position =
          let line_and_column = String.split ~on:':' position in
          {
            Location.line = Int.of_string (List.nth_exn line_and_column 0);
            column = Int.of_string (List.nth_exn line_and_column 1);
          }
        in
        let positions = String.split ~on:'-' location in
        {
          Location.start = parse_position (List.nth_exn positions 0);
          stop = parse_position (List.nth_exn positions 1);
        }
      in
      List.map expected ~f:(fun (key, value) -> parse_location key, value)
      |> Location.Map.of_alist_exn
    in
    let define, test_source, environment =
      let find_define = function
        | { Node.value = define; _ }
          when String.equal (Reference.show (Node.value (Statement.Define.name define))) define_name
          ->
            Some define
        | _ -> None
      in
      let project = Test.ScratchProject.setup ~context ["test.py", source] in
      let { ScratchProject.BuiltTypeEnvironment.type_environment; sources } =
        ScratchProject.build_type_environment project
      in
      let test_source =
        List.find_map_exn
          sources
          ~f:(fun ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) ->
            Option.some_if (String.equal (Reference.show qualifier) "test") source)
      in
      ( List.find_map_exn (Preprocessing.defines test_source) ~f:find_define,
        test_source,
        TypeEnvironment.read_only type_environment )
    in
    let overrides = DependencyGraph.create_overrides ~environment ~source:test_source in
    let _ = DependencyGraphSharedMemory.record_overrides overrides in
    assert_equal
      ~cmp:(Location.Map.equal CallGraph.equal_callees)
      ~printer:(fun map ->
        map
        |> Location.Map.to_alist
        |> List.map ~f:(fun (key, value) ->
               Format.sprintf "%s: %s" (Location.show key) (CallGraph.show_callees value))
        |> String.concat ~sep:"\n")
      expected
      (CallGraph.call_graph_of_define ~environment ~define);
    DependencyGraphSharedMemory.remove_overriding_types (Reference.Map.keys overrides)
  in
  assert_call_graph_of_define
    ~source:{|
     def foo():
         bar()

     def bar():
         pass
  |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "3:4-3:9",
          CallGraph.Callees
            (CallGraph.RegularTargets
               { collapse_tito = true; implicit_self = false; targets = [`Function "test.bar"] }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     def foo(c: C):
         c.m()

     class C:
       def m(self):
         pass
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "3:4-3:9",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [Target.create_method (Reference.create "test.C.m")];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     def foo():
       if 1 > 2:
         f = bar
       else:
         f = baz
       f()
     def baz(): ...
     def bar(): ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:2-7:5",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = false;
                 targets = [`Function "test.bar"; `Function "test.baz"];
               }) );
        ( "3:5-3:10",
          CallGraph.SyntheticCallees
            (String.Map.Tree.of_alist_exn
               [
                 ( "__le__",
                   CallGraph.RegularTargets
                     {
                       collapse_tito = true;
                       implicit_self = true;
                       targets = [`Method { Target.class_name = "int"; method_name = "__le__" }];
                     } );
                 ( "__gt__",
                   CallGraph.RegularTargets
                     {
                       collapse_tito = true;
                       implicit_self = true;
                       targets = [`Method { Target.class_name = "int"; method_name = "__gt__" }];
                     } );
               ]) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     def foo():
       if 1 > 2:
         f = bar
       else:
         f = None
       f()
     def bar(): ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "3:5-3:10",
          CallGraph.SyntheticCallees
            (String.Map.Tree.of_alist_exn
               [
                 ( "__le__",
                   CallGraph.RegularTargets
                     {
                       collapse_tito = true;
                       implicit_self = true;
                       targets = [`Method { Target.class_name = "int"; method_name = "__le__" }];
                     } );
                 ( "__gt__",
                   CallGraph.RegularTargets
                     {
                       collapse_tito = true;
                       implicit_self = true;
                       targets = [`Method { Target.class_name = "int"; method_name = "__gt__" }];
                     } );
               ]) );
        ( "7:2-7:5",
          CallGraph.Callees
            (CallGraph.RegularTargets
               { collapse_tito = true; implicit_self = false; targets = [`Function "test.bar"] }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     from typing import Optional

     def foo(c: Optional[C]):
       c.m()
     class C:
       def m():
         ...

      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [Target.create_method (Reference.create "test.C.m")];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     from typing import Optional

     def foo(c: C):
       c.m()
     class C:
       def m():
         ...
     class D(C):
       def m():
         ...
     class E(D):
       def m():
         ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [Target.create_override (Reference.create "test.C.m")];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     from typing import Optional

     def foo(d: D):
       d.m()
     class C:
       def m():
         ...
     class D(C):
       pass
     class E(D):
       def m():
         ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets =
                   [
                     Target.create_method (Reference.create "test.C.m");
                     Target.create_method (Reference.create "test.E.m");
                   ];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
    class C:
      def __call__(self, a: int): ...
    def foo(c: C):
       c(1)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:3-5:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [Target.create_method (Reference.create "test.C.__call__")];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
    class C:
      @staticmethod
      def __call__(a: int): ...
    def foo(c: C):
       c(1)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:3-6:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = false;
                 targets = [Target.create_method (Reference.create "test.C.__call__")];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
    from typing import Protocol
    class C(Protocol):
      def __call__(self, a: int): ...
    def foo(c: C):
       c(1)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:3-6:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [Target.create_method (Reference.create "test.C.__call__")];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
       class C:
         def __init__(self, a): ...
       def foo():
         C()
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:5",
          CallGraph.Callees
            (CallGraph.ConstructorTargets
               {
                 init_targets = [`Method { Target.class_name = "test.C"; method_name = "__init__" }];
                 new_targets = [`Method { Target.class_name = "object"; method_name = "__new__" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
       class C:
         def __new__(cls, a): ...
       def foo():
         C()
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:5",
          CallGraph.Callees
            (CallGraph.ConstructorTargets
               {
                 init_targets = [`Method { Target.class_name = "object"; method_name = "__init__" }];
                 new_targets = [`Method { Target.class_name = "test.C"; method_name = "__new__" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
        class C:
          @property
          def p(self) -> int: ...
          @p.setter
          def p(self, v: int) -> None: ...
        def foo(c: C):
          c.p = c.p
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "8:2-8:5",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "p$setter" }];
               }) );
        ( "8:8-8:11",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "p" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
        class C:
          @staticmethod
          def f(a: int) -> int: ...
        def foo():
          C.f()
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:2-6:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = false;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
        class C:
          @classmethod
          def f(cls, a: int) -> int: ...
        def foo():
          C.f()
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:2-6:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:{|
        def foo():
          1 > 2
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "3:2-3:7",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "int"; method_name = "__gt__" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      class C:
        def __repr__(self) -> str: ...

      def foo(c: C):
        repr(c)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:2-6:9",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "__repr__" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from functools import partial
      def f(a, b):
        ...

      def foo():
        partial(f, 1)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:2-7:15",
          CallGraph.Callees
            (CallGraph.RegularTargets
               { collapse_tito = true; implicit_self = false; targets = [`Function "test.f"] }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from builtins import to_callable_target

      @to_callable_target
      def callable_target(arg):
        pass

      def foo():
        callable_target.async_schedule(1)
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "9:2-9:35",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = false;
                 targets = [`Function "test.callable_target"];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from builtins import to_callable_target

      class Foo:
        @to_callable_target
        def callable_target(arg):
          pass

      def bar(foo: Foo):
        foo.callable_target(1)
      |}
    ~define_name:"test.bar"
    ~expected:
      [
        ( "10:2-10:24",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets =
                   [`Method { Target.class_name = "TestCallableTarget"; method_name = "__call__" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:{|
      def foo(x=bar()):
        pass

      def bar():
        pass
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "2:10-2:15",
          CallGraph.Callees
            (CallGraph.RegularTargets
               { collapse_tito = true; implicit_self = false; targets = [`Function "test.bar"] }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      class C:
        def f(self, x: int) -> int:
          return x

      class D(C):
        def f(self, x: int) -> int:
          return x

        def g(self) -> None:
          super().f(1)
      |}
    ~define_name:"test.D.g"
    ~expected:
      [
        ( "11:4-11:11",
          CallGraph.Callees
            (CallGraph.ConstructorTargets
               {
                 new_targets = [`Method { Target.class_name = "object"; method_name = "__new__" }];
                 init_targets = [`Method { Target.class_name = "super"; method_name = "__init__" }];
               }) );
        ( "11:4-11:16",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      class C:
        def f(self, x: int) -> int:
          return x

      class D(C):
        def f(self, x: int) -> int:
          return x

      def foo(c: C):
        C.f(c, 1)
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "11:2-11:11",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = false;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      class C:
        @classmethod
        def f(cls, x: int) -> int:
          return x

      class D(C):
        @classmethod
        def f(cls, x: int) -> int:
          return x

      def foo(c: C):
        C.f(c, 1)
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "13:2-13:11",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 collapse_tito = true;
                 implicit_self = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      def hof(f, arg):
        f(arg)

      def bar(x):
        pass

      def foo():
        hof(bar, 1)
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "9:2-9:13",
          CallGraph.Callees
            (CallGraph.HigherOrderTargets
               {
                 higher_order_function =
                   {
                     CallGraph.collapse_tito = true;
                     implicit_self = false;
                     targets = [`Function "test.hof"];
                   };
                 callable_argument =
                   ( 0,
                     {
                       CallGraph.collapse_tito = true;
                       implicit_self = false;
                       targets = [`Function "test.bar"];
                     } );
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
        class Builder:
            def __init__(self) -> None:
                self._saved: Optional[str] = None
                self._not_saved: Optional[str] = None

            def set_saved(self, saved: str) -> "Builder":
                self._saved = saved
                return self

            def set_not_saved(self, not_saved: str) -> "Builder":
                self._not_saved = not_saved
                return self

        def foo():
            builder = Builder()
            builder.set_not_saved("true").set_saved("false")
   |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "16:14-16:23",
          CallGraph.Callees
            (CallGraph.ConstructorTargets
               {
                 new_targets = [`Method { Target.class_name = "object"; method_name = "__new__" }];
                 init_targets =
                   [`Method { Target.class_name = "test.Builder"; method_name = "__init__" }];
               }) );
        ( "17:4-17:33",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = false;
                 targets =
                   [`Method { Target.class_name = "test.Builder"; method_name = "set_not_saved" }];
               }) );
        ( "17:4-17:52",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = false;
                 targets =
                   [`Method { Target.class_name = "test.Builder"; method_name = "set_saved" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from functools import lru_cache
      @lru_cache()
      def f() -> int:
        return 0

      def foo():
        f()
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "8:2-8:5",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "test.f"];
               }) );
      ];

  assert_call_graph_of_define
    ~source:
      {|
      from functools import lru_cache
      class C:
        @lru_cache()
        def m(self, x: int) -> int:
          return x

      def foo(c: C):
        c.m(1)
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "9:2-9:8",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "m" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
    class C:
        def run(self) -> str:
            return ""

    def foo() -> None:
        cs: List[C] = [C()]
        result = [c.run() for c in cs]
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:19-7:22",
          CallGraph.Callees
            (CallGraph.ConstructorTargets
               {
                 new_targets = [`Method { Target.class_name = "object"; method_name = "__new__" }];
                 init_targets = [`Method { Target.class_name = "object"; method_name = "__init__" }];
               }) );
        ( "8:14-8:21",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "run" }];
               }) );
      ];

  (* Ensure we don't infinite loop when resolving callable classes. *)
  assert_call_graph_of_define
    ~source:
      {|
    from typing import Any, Target
    def to_c(callable: Target[..., Any]) -> C:
      ...

    class C:
      @to_c
      def __call__(self) -> "C":
        return self

    def foo(c: C) -> None:
      c()
    |}
    ~define_name:"test.foo"
    ~expected:[];

  assert_call_graph_of_define
    ~source:
      {|
        from contextlib import ContextManager
        def foo():
          with to_cm() as my_int:
            pass
        def to_cm() -> ContextManager[int]: ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "4:7-4:14",
          CallGraph.SyntheticCallees
            (String.Map.Tree.of_alist_exn
               [
                 ( "__enter__",
                   CallGraph.RegularTargets
                     {
                       CallGraph.implicit_self = true;
                       collapse_tito = true;
                       targets =
                         [
                           `Method
                             {
                               Target.class_name = "contextlib.ContextManager";
                               method_name = "__enter__";
                             };
                         ];
                     } );
                 ( "to_cm",
                   CallGraph.RegularTargets
                     {
                       CallGraph.implicit_self = false;
                       collapse_tito = true;
                       targets = [`Function "test.to_cm"];
                     } );
               ]) );
      ];
  (* Only the last attribute is a setter for chained property setter calls. *)
  assert_call_graph_of_define
    ~source:
      {|
        class C:
          @property
          def p(self) -> "C":
            ...
          @p.setter
          def p(self, new_value: "C") -> None:
            ...

        def foo(c: C):
          c.p.p = c
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "11:2-11:5",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "p" }];
               }) );
        ( "11:2-11:7",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "p$setter" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
        from typing import Protocol
        class C(Protocol):
          def f(self) -> int: ...

        def foo(c: C):
          c.f()
          C.f(c)

    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:2-7:7",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
        ( "8:2-8:8",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "f" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      class C:
        @property
        def foo(self) -> int:
          ...

      class D:
        @property
        def foo(self) -> int:
          ...

      class E:
        foo: int = 1

      def uses_foo(c_or_d: C | D, c_or_e: C | E):
        x = c_or_d.foo
        y = c_or_e.foo
    |}
    ~define_name:"test.uses_foo"
    ~expected:
      [
        ( "16:6-16:16",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets =
                   [
                     `Method { Target.class_name = "test.C"; method_name = "foo" };
                     `Method { Target.class_name = "test.D"; method_name = "foo" };
                   ];
               }) );
        ( "17:6-17:16",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "foo" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from typing import TypeVar
      class C:
        @property
        def foo(self) -> int:
          ...

      class D:
        @property
        def foo(self) -> int:
          ...

      TCOrD = TypeVar("TCOrD", C, D)
      def uses_foo(c_or_d: TCOrD):
        x = c_or_d.foo
    |}
    ~define_name:"test.uses_foo"
    ~expected:
      [
        ( "15:6-15:16",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets =
                   [
                     `Method { Target.class_name = "test.C"; method_name = "foo" };
                     `Method { Target.class_name = "test.D"; method_name = "foo" };
                   ];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
        class C:
          @classmethod
          def foo(cls):
            pass
        d = {
          "a": C,
          "b": C,
        }
        def calls_d_method(s: str):
          d[s].foo()
      |}
    ~define_name:"test.calls_d_method"
    ~expected:
      [
        ( "11:2-11:6",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "dict"; method_name = "__getitem__" }];
               }) );
        ( "11:2-11:12",
          Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.C"; method_name = "foo" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      def outer(x: int) -> None:
        def inner(x: int) -> None:
          print(x)

        inner(x)
  |}
    ~define_name:"test.outer"
    ~expected:
      [
        ( "6:2-6:10",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "$local_test?outer$inner"];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      class Foo:
        def outer(self, x: int) -> None:
          def inner(x: int) -> None:
            pass

          inner(x)
  |}
    ~define_name:"test.Foo.outer"
    ~expected:
      [
        ( "7:4-7:12",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "$local_test?Foo?outer$inner"];
               }) );
      ];
  ();
  (* Well-typed decorators are 'safely' ignored (when not inlined). *)
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      @timer("bar")
      def foo(x: int) -> None:
        pass

      def caller() -> None:
        foo(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "20:2-20:8",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "test.foo"];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @timer("bar")
        def bar(self, x: int) -> None:
          pass

      def caller(foo: Foo) -> None:
        foo.bar(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "21:2-21:12",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.Foo"; method_name = "bar" }];
               }) );
      ];
  (* Partially-typed decorators are 'safely' ignored (when not inlined). *)
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      _T = TypeVar("_T")

      class Timer:
        def __call__(self, func: Callable[..., _T]) -> Callable[..., _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      @timer("bar")
      def foo(x: int) -> None:
        pass

      def caller() -> None:
        foo(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "17:2-17:8",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "test.foo"];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      _T = TypeVar("_T")

      class Timer:
        def __call__(self, func: Callable[..., _T]) -> Callable[..., _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @timer("bar")
        def bar(self, x: int) -> None:
          pass

      def caller(foo: Foo) -> None:
        foo.bar(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "18:2-18:12",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.Foo"; method_name = "bar" }];
               }) );
      ];
  (* Untyped decorators are 'safely' ignored (when not inlined). *)
  assert_call_graph_of_define
    ~source:
      {|
      def timer(name: str):
        pass

      @timer("bar")
      def foo(x: int) -> None:
        pass

      def caller() -> None:
        foo(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "10:2-10:8",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "test.foo"];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      def timer(name: str):
        pass

      class Foo:
        @timer("bar")
        def bar(self, x: int) -> None:
          pass

      def caller(foo: Foo) -> None:
        foo.bar(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "11:2-11:12",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.Foo"; method_name = "bar" }];
               }) );
      ];
  (* Well-typed decorators with @classmethod or @staticmethod. *)
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @classmethod
        @timer("bar")
        def bar(cls, x: int) -> None:
          pass

      def caller() -> None:
        Foo.bar(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "22:2-22:12",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.Foo"; method_name = "bar" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @staticmethod
        @timer("bar")
        def bar(x: int) -> None:
          pass

      def caller() -> None:
        Foo.bar(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "22:2-22:12",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.Foo"; method_name = "bar" }];
               }) );
      ];
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @classmethod
        @timer("bar")
        def bar(cls, x: int) -> None:
          pass

        @classmethod
        def caller(cls) -> None:
          cls.bar(1)
    |}
    ~define_name:"test.Foo.caller"
    ~expected:
      [
        ( "23:4-23:14",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = true;
                 collapse_tito = true;
                 targets = [`Method { Target.class_name = "test.Foo"; method_name = "bar" }];
               }) );
      ];
  (* Decorators with type errors. *)
  assert_call_graph_of_define
    ~source:
      {|
      from typing import Callable, TypeVar
      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      @timer(1) # Intended type error here.
      def foo(x: int) -> None:
        pass

      def caller() -> None:
        foo(1)
    |}
    ~define_name:"test.caller"
    ~expected:
      [
        ( "18:2-18:8",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "test.foo"];
               }) );
      ];
  (* Resolving __call__ via __getattr__ when a union including self type is involved. *)
  assert_call_graph_of_define
    ~source:
      {|
      from __future__ import annotations
      from typing import Union

      class CallViaGetattr:
        def __getattr__(self, name: str) -> Union[None, CallViaGetattr]:
          return None

      def baz(x: CallViaGetattr) -> None:
        y = print(x.attribute)
    |}
    ~define_name:"test.baz"
    ~expected:
      [
        ( "10:6-10:24",
          CallGraph.Callees
            (CallGraph.RegularTargets
               {
                 CallGraph.implicit_self = false;
                 collapse_tito = true;
                 targets = [`Function "print"];
               }) );
      ];
  (* Detecting a __call__ picked up via __getattr__ redirection *)
  assert_call_graph_of_define
    ~source:
      {|
      from __future__ import annotations
      from typing import Union

      class CallableClass:
        def __call__(self) -> None:
          return None

      class CallViaGetattr:
        def __getattr__(self, name: str) -> Union[None, CallableClass]:
          return CallableClass()

      def baz(x: CallViaGetattr) -> None:
        y = print(x.attribute)
    |}
    ~define_name:"test.baz"
    ~expected:
      [
        ( "14:6-14:24",
          CallGraph.Callees
            (CallGraph.HigherOrderTargets
               {
                 higher_order_function =
                   {
                     CallGraph.implicit_self = false;
                     collapse_tito = true;
                     targets = [`Function "print"];
                   };
                 callable_argument =
                   ( 0,
                     {
                       implicit_self = true;
                       collapse_tito = true;
                       targets =
                         [
                           `Method
                             { Target.class_name = "test.CallableClass"; method_name = "__call__" };
                         ];
                     } );
               }) );
      ];
  ()


let test_resolve_ignoring_optional context =
  let assert_resolved_without_optional ~source ~expression ~expected =
    let resolution =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.build_resolution
    in
    CallGraph.resolve_ignoring_optional ~resolution (Test.parse_single_expression expression)
    |> assert_equal ~printer:Type.show expected
  in
  assert_resolved_without_optional
    ~source:{|
    class Data:
      def __init__(self, x: int) -> None: ...
  |}
    ~expression:"x.Data()"
    ~expected:(Type.Primitive "x.Data")


let () =
  "interproceduralCallGraph"
  >::: [
         "call_graph_of_define" >:: test_call_graph_of_define;
         "resolve_ignoring_optional" >:: test_resolve_ignoring_optional;
       ]
  |> Test.run
