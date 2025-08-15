(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open OUnit2
open Analysis
open Ast
open Test
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module Global = PyrePysaEnvironment.ModelQueries.Global

let test_source_is_unit_test context =
  let assert_is_unit_test ?(expected = true) ?(extra_sources = []) source =
    let project = ScratchProject.setup ~context (["test.py", source] @ extra_sources) in
    let { Test.ScratchProject.BuiltTypeEnvironment.type_environment; _ }, _ =
      Test.ScratchProject.build_type_environment_and_postprocess project
    in
    let global_module_paths_api = Test.ScratchProject.global_module_paths_api project in
    let source =
      SourceCodeApi.source_of_qualifier
        (Test.ScratchProject.get_untracked_source_code_api project)
        (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    let pyre_pysa_read_only_api =
      PyrePysaEnvironment.ReadOnly.create ~type_environment ~global_module_paths_api
    in
    assert_equal
      expected
      (PyrePysaEnvironment.ReadOnly.source_is_unit_test pyre_pysa_read_only_api ~source)
  in
  let assert_not_unit_test = assert_is_unit_test ~expected:false in
  assert_is_unit_test "class C(unittest.case.TestCase): ...";
  assert_not_unit_test {|
    from unittest import TestCase
    class C: pass
  |};
  assert_is_unit_test
    {|
    class C:
      def foo():
        class Nested(unittest.case.TestCase): ...
  |};
  ()


let test_resolve_qualified_name_to_global context =
  let assert_resolve ~context sources name ~expect =
    PyrePysaEnvironment.ModelQueries.invalidate_cache ();
    let pyre_api =
      ScratchProject.setup ~context sources |> ScratchProject.pyre_pysa_read_only_api
    in
    let actual =
      PyrePysaEnvironment.ModelQueries.resolve_qualified_name_to_global
        pyre_api
        (Ast.Reference.create name)
    in
    let printer = function
      | None -> "None"
      | Some global -> Global.show global
    in
    assert_equal ~printer expect actual
  in
  let create_parameter ?(annotation = Type.Any) ?(default = false) name =
    Type.Callable.CallableParamType.Named { name = "$parameter$" ^ name; annotation; default }
  in
  let get_callable_type = function
    | Type.Callable t -> t
    | _ -> failwith "unreachable"
  in
  let create_callable ?name ?(overloads = []) ?(parameters = []) ?(annotation = Type.Any) () =
    Type.Callable.create
      ?name:(name >>| Reference.create)
      ~overloads
      ~parameters:(Type.Callable.Defined parameters)
      ~annotation
      ()
    |> get_callable_type
  in
  (* Most common cases. *)
  assert_resolve
    ~context
    ["test.py", {|
      def foo():
        return
    |}]
    "test.foo"
    ~expect:(Some (Global.Function (create_callable ~name:"test.foo" ())));
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        def bar(self):
          return
    |}]
    "test.Foo.bar"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~parameters:[create_parameter ~annotation:(Type.Primitive "test.Foo") "self"]
               ())));
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          from typing import Callable
          foo: Callable[[], None] = lambda: None
        |}
      );
    ]
    "test.foo"
    ~expect:(Some (Global.Function (create_callable ~annotation:Type.NoneType ())));
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          from typing import Callable
          class Foo:
            bar: Callable[[], None] = lambda: None
        |}
      );
    ]
    "test.Foo.bar"
    ~expect:(Some (Global.Function (create_callable ~annotation:Type.NoneType ())));
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        pass
    |}]
    "test.Foo"
    ~expect:(Some Global.Class);
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        class Bar:
          pass
    |}]
    "test.Foo.Bar"
    ~expect:(Some Global.Class);
  assert_resolve
    ~context
    ["test.py", {|
      def foo():
        return None
    |}]
    "test"
    ~expect:(Some Global.Module);
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        x: int = 1
    |}]
    "test.Foo.x"
    ~expect:(Some Global.Attribute);
  assert_resolve
    ~context
    ["test.py", {|
      x: int = 1
    |}]
    "test.x"
    ~expect:(Some Global.Attribute);
  assert_resolve
    ~context
    ["test.py", {|
      from typing import Any
      x: Any = 1
    |}]
    "test.x"
    ~expect:(Some Global.UnknownAttribute);
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          from typing import Type
          class Foo:
            pass
          x: Type[Foo] = Foo
        |}
      );
    ]
    "test.x"
    ~expect:(Some Global.Attribute);

  (* Symbol is not found. *)
  assert_resolve
    ~context
    ["test.py", {|
      def foo():
        return
    |}]
    "test.bar"
    ~expect:None;
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        def bar():
          return
    |}]
    "test.Foo.baz"
    ~expect:None;
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        pass
    |}]
    "test.Bar"
    ~expect:None;
  assert_resolve ~context ["foo.py", "x: int = 1"] "bar" ~expect:None;

  (* Decorators. *)
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          class Memoize:
            def __init__(self, f):
              self.f = f
            def __call__(self, *args, **kwargs):
              pass

          def memoize(f) -> Memoize:
            return Memoize(f)

          @memoize
          def foo(x: int) -> int:
            return x
        |}
      );
    ]
    "test.foo"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~annotation:Type.integer
               ~name:"test.foo"
               ~parameters:[create_parameter ~annotation:Type.integer "x"]
               ())));
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          class Memoize:
            def __init__(self, f):
              self.f = f
            def __call__(self, *args, **kwargs):
              pass

          def memoize(f) -> Memoize:
            return Memoize(f)

          class Foo:
            @memoize
            def bar(self, x: int) -> int:
              return x
        |}
      );
    ]
    "test.Foo.bar"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~annotation:Type.integer
               ~parameters:
                 [
                   create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                   create_parameter ~annotation:Type.integer "x";
                 ]
               ())));

  (* Overloads *)
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          from typing import overload
          class Foo:
            @overload
            def bar(self, x: int) -> str: ...
            @overload
            def bar(self, x: str) -> int: ...
        |}
      );
    ]
    "test.Foo.bar"
    ~expect:
      (Some
         (Global.Function
            (Type.Callable.create
               ~parameters:
                 (Type.Callable.Defined
                    [
                      create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                      create_parameter ~annotation:Type.integer "x";
                    ])
               ~annotation:Type.string
               ~overloads:
                 [
                   {
                     parameters =
                       Defined
                         [
                           create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                           create_parameter ~annotation:Type.integer "x";
                         ];
                     annotation = Type.string;
                   };
                   {
                     parameters =
                       Defined
                         [
                           create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                           create_parameter ~annotation:Type.string "x";
                         ];
                     annotation = Type.integer;
                   };
                 ]
               ()
            |> get_callable_type)));

  (* Top. *)
  assert_resolve
    ~context
    [
      ( "test.py",
        {|
          class Foo:
            def bar(self):
              pass
            baz = bar
        |}
      );
    ]
    "test.Foo.baz"
    ~expect:(Some Global.UnknownAttribute);

  (* Definition in type stub. *)
  assert_resolve
    ~context
    ["test.pyi", {|
      def foo() -> None: ...
    |}]
    "test.foo"
    ~expect:(Some (Global.Function (create_callable ~annotation:Type.NoneType ~name:"test.foo" ())));
  assert_resolve
    ~context
    ["test.pyi", {|
      class Foo:
        def bar(self) -> None: ...
    |}]
    "test.Foo.bar"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~annotation:Type.NoneType
               ~parameters:[create_parameter ~annotation:(Type.Primitive "test.Foo") "self"]
               ())));
  assert_resolve
    ~context
    [
      ( "test.pyi",
        {|
          from typing import Callable
          foo: Callable[[], None]
        |} );
    ]
    "test.foo"
    ~expect:(Some (Global.Function (create_callable ~annotation:Type.NoneType ())));
  assert_resolve
    ~context
    ["test.pyi", {|
      x: int = 1
    |}]
    "test.x"
    ~expect:(Some Global.Attribute);

  (* Deeply nested code, where outer packages are not importable *)
  assert_resolve
    ~context
    [
      "outer/middle/inner/a.py", {|
      def foo() -> None: ...
    |};
      "outer/middle/inner/b.py", {|
      from .a import foo
    |};
    ]
    "outer.middle.inner.b.foo"
    ~expect:
      (Some
         (Global.Function
            (create_callable ~annotation:Type.NoneType ~name:"outer.middle.inner.a.foo" ())));
  assert_resolve
    ~context
    [
      "outer/middle/inner/a.py", {|
      class Foo:
        def bar(self): ...
    |};
      "outer/middle/inner/b.py", {|
      from .a import Foo
    |};
    ]
    "outer.middle.inner.b.Foo.bar"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~name:"outer.middle.inner.a.Foo.bar"
               ~parameters:
                 [create_parameter ~annotation:(Type.Primitive "outer.middle.inner.a.Foo") "self"]
               ())));
  ()


let () =
  "pyrePysaApi"
  >::: [
         "source_is_unit_test" >:: test_source_is_unit_test;
         "resolve_qualified_name_to_global" >:: test_resolve_qualified_name_to_global;
       ]
  |> Test.run
