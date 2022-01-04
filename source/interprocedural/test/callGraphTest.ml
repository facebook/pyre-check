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
open Test
open Interprocedural
open CallGraph

let test_call_graph_of_define context =
  let assert_call_graph_of_define ?(object_targets = []) ~source ~define_name ~expected () =
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
      List.fold
        expected
        ~init:DefineCallGraph.empty
        ~f:(fun call_graph_of_define (location, callees) ->
          DefineCallGraph.add call_graph_of_define ~location:(parse_location location) ~callees)
    in
    let define, test_source, environment =
      let find_define = function
        | { Node.value = define; _ }
          when String.equal (Statement.Define.name define |> Reference.show) define_name ->
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
    let register_model target =
      FixpointState.add_predefined
        FixpointState.Epoch.predefined
        target
        AnalysisResult.obscure_model
    in
    List.iter ~f:register_model object_targets;
    let overrides = DependencyGraph.create_overrides ~environment ~source:test_source in
    let _ = DependencyGraphSharedMemory.record_overrides overrides in
    assert_equal
      ~cmp:DefineCallGraph.equal
      ~printer:DefineCallGraph.show
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.bar")]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (Target.create_method (Reference.create "test.C.m"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create (`Function "test.bar");
                      CallTarget.create (`Function "test.baz");
                    ]
                  ~return_type:Type.Any
                  ())) );
        ( "3:5-3:10",
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "__le__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method { Target.class_name = "int"; method_name = "__le__" });
                          ]
                        ~return_type:Type.bool
                        ()) );
                 ( "__gt__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method { Target.class_name = "int"; method_name = "__gt__" });
                          ]
                        ~return_type:Type.bool
                        ()) );
               ]) );
      ]
    ();
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
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "__le__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method { Target.class_name = "int"; method_name = "__le__" });
                          ]
                        ~return_type:Type.bool
                        ()) );
                 ( "__gt__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method { Target.class_name = "int"; method_name = "__gt__" });
                          ]
                        ~return_type:Type.bool
                        ()) );
               ]) );
        ( "7:2-7:5",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.bar")]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (Target.create_method (Reference.create "test.C.m"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (Target.create_override (Reference.create "test.C.m"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (Target.create_method (Reference.create "test.C.m"));
                      CallTarget.create
                        ~implicit_self:true
                        (Target.create_method (Reference.create "test.E.m"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        ~implicit_dunder_call:true
                        (Target.create_method (Reference.create "test.C.__call__"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_dunder_call:true
                        (Target.create_method (Reference.create "test.C.__call__"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
    class C:
      def __call__(self, a: int): ...
    def foo(c: C):
       c.__call__(1)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:3-5:16",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (Target.create_method (Reference.create "test.C.__call__"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        ~implicit_dunder_call:true
                        (Target.create_method (Reference.create "test.C.__call__"));
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~init_targets:[`Method { Target.class_name = "test.C"; method_name = "__init__" }]
                  ~new_targets:[`Method { Target.class_name = "object"; method_name = "__new__" }]
                  ~return_type:(Type.Primitive "test.C")
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~init_targets:[`Method { Target.class_name = "object"; method_name = "__init__" }]
                  ~new_targets:[`Method { Target.class_name = "test.C"; method_name = "__new__" }]
                  ~return_type:(Type.Primitive "test.C")
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
       from unknown import A
       class B(A):
         def __init__(self, a): ...
       def foo():
         B()
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:2-6:5",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~init_targets:[`Method { Target.class_name = "test.B"; method_name = "__init__" }]
                  ~new_targets:[`Method { Target.class_name = "object"; method_name = "__new__" }]
                  ~return_type:(Type.Primitive "test.B")
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
       from unknown import A
       class B(A):
         def __new__(cls, a): ...
       def foo():
         B()
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:2-6:5",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~init_targets:[`Method { Target.class_name = "object"; method_name = "__init__" }]
                  ~new_targets:[`Method { Target.class_name = "test.B"; method_name = "__new__" }]
                  ~return_type:(Type.Primitive "test.B")
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [`Method { Target.class_name = "test.C"; method_name = "p$setter" }];
                 global_targets = [];
                 return_type = Type.none;
                 is_attribute = false;
               }) );
        ( "8:8-8:11",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [`Method { Target.class_name = "test.C"; method_name = "p" }];
                 global_targets = [];
                 return_type = Type.integer;
                 is_attribute = false;
               }) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:{|
        def foo():
          1 > 2
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "3:2-3:7",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "int"; method_name = "__gt__" });
                    ]
                  ~return_type:Type.bool
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "__repr__" });
                    ]
                  ~return_type:(Type.Primitive "str")
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.f")]
                  ~return_type:(Type.parametric "functools.partial" [Single Type.Any])
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.callable_target")]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        ~implicit_dunder_call:true
                        (`Method
                          { Target.class_name = "TestCallableTarget"; method_name = "__call__" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.bar")]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~new_targets:[`Method { Target.class_name = "object"; method_name = "__new__" }]
                  ~init_targets:[`Method { Target.class_name = "super"; method_name = "__init__" }]
                  ~return_type:(Type.Primitive "test.C")
                  ())) );
        ( "11:4-11:16",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
      def hof(f, arg) -> bool:
        f(arg)

      def bar(x) -> int:
        pass

      def foo():
        hof(bar, 1)
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "9:2-9:13",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.hof")]
                  ~return_type:Type.bool
                  ~higher_order_parameter:
                    {
                      index = 0;
                      return_type = Type.integer;
                      call_targets = [CallTarget.create (`Function "test.bar")];
                    }
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~new_targets:[`Method { Target.class_name = "object"; method_name = "__new__" }]
                  ~init_targets:
                    [`Method { Target.class_name = "test.Builder"; method_name = "__init__" }]
                  ~return_type:(Type.Primitive "test.Builder")
                  ())) );
        ( "17:4-17:33",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        ~collapse_tito:false
                        (`Method
                          { Target.class_name = "test.Builder"; method_name = "set_not_saved" });
                    ]
                  ~return_type:(Type.Primitive "test.Builder")
                  ())) );
        ( "17:4-17:52",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        ~collapse_tito:false
                        (`Method { Target.class_name = "test.Builder"; method_name = "set_saved" });
                    ]
                  ~return_type:(Type.Primitive "test.Builder")
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.f")]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "m" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~new_targets:[`Method { Target.class_name = "object"; method_name = "__new__" }]
                  ~init_targets:[`Method { Target.class_name = "object"; method_name = "__init__" }]
                  ~return_type:(Type.Primitive "test.C")
                  ())) );
        ( "8:14-8:21",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "run" });
                    ]
                  ~return_type:(Type.Primitive "str")
                  ())) );
      ]
    ();

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
    ~expected:
      [
        ( "12:2-12:5",
          LocationCallees.Singleton
            (ExpressionCallees.from_call (CallCallees.create_unresolved Type.Any)) );
      ]
    ();

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
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "__enter__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              ~collapse_tito:true
                              (`Method
                                {
                                  Target.class_name = "contextlib.ContextManager";
                                  method_name = "__enter__";
                                });
                          ]
                        ~return_type:Type.integer
                        ()) );
                 ( "to_cm",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:[CallTarget.create (`Function "test.to_cm")]
                        ~return_type:
                          (Type.parametric "contextlib.ContextManager" [Single Type.integer])
                        ()) );
               ]) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [`Method { Target.class_name = "test.C"; method_name = "p" }];
                 global_targets = [];
                 return_type = Type.Primitive "test.C";
                 is_attribute = false;
               }) );
        ( "11:2-11:7",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [`Method { Target.class_name = "test.C"; method_name = "p$setter" }];
                 global_targets = [];
                 return_type = Type.none;
                 is_attribute = false;
               }) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
        ( "8:2-8:8",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create (`Method { Target.class_name = "test.C"; method_name = "f" });
                    ]
                  ~return_type:Type.integer
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [
                     `Method { Target.class_name = "test.C"; method_name = "foo" };
                     `Method { Target.class_name = "test.D"; method_name = "foo" };
                   ];
                 global_targets = [];
                 return_type = Type.integer;
                 is_attribute = false;
               }) );
        ( "17:6-17:16",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [`Method { Target.class_name = "test.C"; method_name = "foo" }];
                 global_targets = [];
                 return_type = Type.integer;
                 is_attribute = true;
               }) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [
                     `Method { Target.class_name = "test.C"; method_name = "foo" };
                     `Method { Target.class_name = "test.D"; method_name = "foo" };
                   ];
                 global_targets = [];
                 return_type = Type.integer;
                 is_attribute = false;
               }) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "dict"; method_name = "__getitem__" });
                    ]
                  ~return_type:(Type.meta (Type.Primitive "test.C"))
                  ())) );
        ( "11:2-11:12",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "foo" });
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "$local_test?outer$inner")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "$local_test?Foo?outer$inner")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
     class C:
       def m(self) -> str:
         return "world"

     def foo(c: C) -> str:
       return f"hello {c.m()}"
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:18-7:23",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.C"; method_name = "m" });
                    ]
                  ~return_type:(Type.Primitive "str")
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
     class C:
       @property
       def attribute(self) -> Callable[[], int]:
         return lambda: 0

     def foo(c: C) -> str:
       return c.attribute()
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "8:9-8:20",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets =
                   [`Method { Target.class_name = "test.C"; method_name = "attribute" }];
                 global_targets = [];
                 return_type = Type.Top;
                 is_attribute = false;
               }) );
        ( "8:9-8:22",
          LocationCallees.Singleton
            (ExpressionCallees.from_call (CallCallees.create_unresolved Type.Any)) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
     def foo() -> None:
       pass
     def bar() -> None:
       pass
     def test(x) -> str:
       try:
         return foo()
       finally:
         bar(x)
      |}
    ~define_name:"test.test"
    ~expected:
      [
        ( "8:11-8:16",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.foo")]
                  ~return_type:Type.none
                  ())) );
        ( "10:4-10:10",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.bar")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
  assert_call_graph_of_define
    ~source:
      {|
     def foo() -> None:
       pass
     def bar() -> None:
       pass
     def test(x) -> str:
       try:
         raise Exception()
       finally:
         bar(x)
      |}
    ~define_name:"test.test"
    ~expected:
      [
        ( "8:10-8:21",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~new_targets:[`Method { Target.class_name = "object"; method_name = "__new__" }]
                  ~init_targets:[`Method { Target.class_name = "object"; method_name = "__init__" }]
                  ~return_type:(Type.Primitive "Exception")
                  ())) );
        ( "10:4-10:10",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.bar")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
  (* TODO(T105570363): Resolve calls with mixed function and methods. *)
  assert_call_graph_of_define
    ~source:
      {|
      class Foo:
        def bar(self) -> None:
          pass

      def baz(self) -> None:
        pass

      def f(foo: Foo):
        for g in [foo.bar, baz]:
          g()
  |}
    ~define_name:"test.f"
    ~expected:
      [
        ( "10:6-10:7",
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "__iter__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method { Target.class_name = "list"; method_name = "__iter__" });
                          ]
                        ~return_type:
                          (Type.iterator
                             (Type.Union
                                [
                                  Type.Callable
                                    {
                                      kind = Anonymous;
                                      implementation =
                                        { annotation = Type.none; parameters = Defined [] };
                                      overloads = [];
                                    };
                                  Type.Callable
                                    {
                                      kind = Named !&"test.baz";
                                      implementation =
                                        {
                                          annotation = Type.none;
                                          parameters =
                                            Defined
                                              [
                                                Named
                                                  {
                                                    name = "self";
                                                    annotation = Type.Top;
                                                    default = false;
                                                  };
                                              ];
                                        };
                                      overloads = [];
                                    };
                                ]))
                        ()) );
                 ( "__next__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method
                                { Target.class_name = "typing.Iterator"; method_name = "__next__" });
                          ]
                        ~return_type:
                          (Type.Union
                             [
                               Type.Callable
                                 {
                                   kind = Anonymous;
                                   implementation =
                                     { annotation = Type.none; parameters = Defined [] };
                                   overloads = [];
                                 };
                               Type.Callable
                                 {
                                   kind = Named !&"test.baz";
                                   implementation =
                                     {
                                       annotation = Type.none;
                                       parameters =
                                         Defined
                                           [
                                             Named
                                               {
                                                 name = "self";
                                                 annotation = Type.Top;
                                                 default = false;
                                               };
                                           ];
                                     };
                                   overloads = [];
                                 };
                             ])
                        ()) );
               ]) );
        ( "11:4-11:7",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.baz")]
                  ~return_type:Type.none
                  ~unresolved:true
                  ())) );
      ]
    ();
  (* TODO(T105570363): Resolve calls with mixed function and constructors. *)
  assert_call_graph_of_define
    ~source:
      {|
      class Foo:
        pass

      def bar(self) -> None:
        pass

      def f():
        for g in [Foo, bar]:
          g()
  |}
    ~define_name:"test.f"
    ~expected:
      [
        ( "9:6-9:7",
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "__iter__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method { Target.class_name = "list"; method_name = "__iter__" });
                          ]
                        ~return_type:
                          (Type.iterator
                             (Type.Union
                                [
                                  Type.Callable
                                    {
                                      kind = Anonymous;
                                      implementation =
                                        {
                                          annotation = Type.Primitive "test.Foo";
                                          parameters = Defined [];
                                        };
                                      overloads = [];
                                    };
                                  Type.Callable
                                    {
                                      kind = Named !&"test.bar";
                                      implementation =
                                        {
                                          annotation = Type.none;
                                          parameters =
                                            Defined
                                              [
                                                Named
                                                  {
                                                    name = "self";
                                                    annotation = Type.Top;
                                                    default = false;
                                                  };
                                              ];
                                        };
                                      overloads = [];
                                    };
                                ]))
                        ()) );
                 ( "__next__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              ~implicit_self:true
                              (`Method
                                { Target.class_name = "typing.Iterator"; method_name = "__next__" });
                          ]
                        ~return_type:
                          (Type.Union
                             [
                               Type.Callable
                                 {
                                   kind = Anonymous;
                                   implementation =
                                     {
                                       annotation = Type.Primitive "test.Foo";
                                       parameters = Defined [];
                                     };
                                   overloads = [];
                                 };
                               Type.Callable
                                 {
                                   kind = Named !&"test.bar";
                                   implementation =
                                     {
                                       annotation = Type.none;
                                       parameters =
                                         Defined
                                           [
                                             Named
                                               {
                                                 name = "self";
                                                 annotation = Type.Top;
                                                 default = false;
                                               };
                                           ];
                                     };
                                   overloads = [];
                                 };
                             ])
                        ()) );
               ]) );
        ( "10:4-10:7",
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.bar")]
                  ~return_type:Type.none
                  ~unresolved:true
                  ())) );
      ]
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.foo")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.Foo"; method_name = "bar" });
                    ]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.foo")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.Foo"; method_name = "bar" });
                    ]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.foo")]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.Foo"; method_name = "bar" });
                    ]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.Foo"; method_name = "bar" });
                    ]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        (`Method { Target.class_name = "test.Foo"; method_name = "bar" });
                    ]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create
                        ~implicit_self:true
                        (`Method { Target.class_name = "test.Foo"; method_name = "bar" });
                    ]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "test.foo")]
                  ~return_type:Type.Any
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "print")]
                  ~return_type:Type.none
                  ())) );
      ]
    ();
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
          LocationCallees.Singleton
            (ExpressionCallees.from_call
               (CallCallees.create
                  ~call_targets:[CallTarget.create (`Function "print")]
                  ~return_type:Type.none
                  ~higher_order_parameter:
                    {
                      index = 0;
                      return_type = Type.Any;
                      call_targets =
                        [
                          CallTarget.create
                            ~implicit_self:true
                            ~implicit_dunder_call:true
                            (`Method
                              { Target.class_name = "test.CallableClass"; method_name = "__call__" });
                        ];
                    }
                  ())) );
      ]
    ();

  assert_call_graph_of_define
    ~object_targets:[`Object "test.Token.token"]
    ~source:
      {|
      class Token:
        token: str = ""

      def foo(obj: Token):
        return obj.token
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:9-6:18",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets = [];
                 global_targets = [`Object "test.Token.token"];
                 return_type = Type.Primitive "str";
                 is_attribute = true;
               }) );
      ]
    ();
  assert_call_graph_of_define
    ~object_targets:[`Object "test.A.attribute"; `Object "test.C.attribute"]
    ~source:
      {|
      from typing import Union

      class A:
        attribute: str = ""

      class B:
        attribute: str = ""

      class C:
        attribute: str = ""

      def foo(obj: Union[A, B, C]):
        return obj.attribute
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "14:9-14:22",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets = [];
                 global_targets = [`Object "test.A.attribute"; `Object "test.C.attribute"];
                 return_type = Type.Primitive "str";
                 is_attribute = true;
               }) );
      ]
    ();
  assert_call_graph_of_define
    ~object_targets:[`Object "test.Token.token"]
    ~source:
      {|
      from typing import Optional

      class Token:
        token: str = ""

      class Request:
        access_token: Optional[Token] = None

      def foo(request: Request):
        return request.access_token.token
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "11:9-11:35",
          LocationCallees.Singleton
            (ExpressionCallees.from_attribute_access
               {
                 AttributeAccessCallees.property_targets = [];
                 global_targets = [`Object "test.Token.token"];
                 return_type = Type.Top;
                 is_attribute = true;
               }) );
      ]
    ();
  assert_call_graph_of_define
    ~object_targets:[`Object "test.Token.token"]
    ~source:
      {|
      class Token:
        token: str = ""

      def foo(obj: Token):
        return getattr(obj, "token", None)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:9-6:36",
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "getattr",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:[CallTarget.create (`Function "getattr")]
                        ~return_type:Type.Any
                        ()) );
                 ( "token",
                   ExpressionCallees.from_attribute_access
                     {
                       AttributeAccessCallees.property_targets = [];
                       global_targets = [`Object "test.Token.token"];
                       return_type = Type.Primitive "str";
                       is_attribute = true;
                     } );
               ]) );
      ]
    ();
  assert_call_graph_of_define
    ~object_targets:[`Object "test.Token.token"]
    ~source:
      {|
      class Token:
        token: str = ""

      def foo(obj: Token, x: str):
        return object.__setattr__(obj, "token", x)
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "6:9-6:44",
          LocationCallees.Compound
            (String.Map.Tree.of_alist_exn
               [
                 ( "__setattr__",
                   ExpressionCallees.from_call
                     (CallCallees.create
                        ~call_targets:
                          [
                            CallTarget.create
                              (`Method
                                { Target.class_name = "object"; method_name = "__setattr__" });
                          ]
                        ~return_type:Type.none
                        ()) );
                 ( "token",
                   ExpressionCallees.from_attribute_access
                     {
                       AttributeAccessCallees.property_targets = [];
                       global_targets = [`Object "test.Token.token"];
                       return_type = Type.none;
                       is_attribute = true;
                     } );
               ]) );
      ]
    ();
  assert_call_graph_of_define
    ~object_targets:[`Object "test.x"]
    ~source:{|
      x = "x"

      def foo():
        return x
    |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:9-5:10",
          LocationCallees.Singleton
            (ExpressionCallees.from_identifier
               { IdentifierCallees.global_targets = [`Object "test.x"] }) );
      ]
    ();
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
