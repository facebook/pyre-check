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
module Function = PyrePysaEnvironment.ModelQueries.Function
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
        ~is_property_getter:false
        ~is_property_setter:false
        (Ast.Reference.create name)
    in
    let printer = function
      | None -> "None"
      | Some global -> Global.show global
    in
    assert_equal ~printer expect actual
  in
  let create_parameter ?(annotation = Type.Any) name =
    PyrePysaEnvironment.ModelQueries.FunctionParameter.Named
      {
        name = "$parameter$" ^ name;
        annotation = PyrePysaEnvironment.PysaType.from_pyre1_type annotation;
        has_default = false;
      }
  in
  let create_callable
      ~define_name
      ?imported_name
      ?(is_method = false)
      ?(overloads = [])
      ?(parameters = [])
      ?(return_annotation = Type.Any)
      ()
    =
    let define_name = Reference.create define_name in
    {
      Function.define_name;
      imported_name = imported_name >>| Reference.create;
      undecorated_signature =
        Some
          {
            PyrePysaEnvironment.ModelQueries.FunctionSignature.overloads =
              PyrePysaEnvironment.ModelQueries.FunctionParameters.List parameters :: overloads;
            return_annotation = PyrePysaEnvironment.PysaType.from_pyre1_type return_annotation;
          };
      is_property_getter = false;
      is_property_setter = false;
      is_method;
    }
  in
  (* Most common cases. *)
  assert_resolve
    ~context
    ["test.py", {|
      def foo():
        return
    |}]
    "test.foo"
    ~expect:
      (Some (Global.Function (create_callable ~define_name:"test.foo" ~imported_name:"test.foo" ())));
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
               ~define_name:"test.Foo.bar"
               ~is_method:true
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
    ~expect:
      (Some
         (Global.Function
            (create_callable ~define_name:"test.foo" ~return_annotation:Type.NoneType ())));
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
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~define_name:"test.Foo.bar"
               ~is_method:true
               ~return_annotation:Type.NoneType
               ())));
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        pass
    |}]
    "test.Foo"
    ~expect:(Some (Global.Class { class_name = "test.Foo" }));
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        class Bar:
          pass
    |}]
    "test.Foo.Bar"
    ~expect:(Some (Global.Class { class_name = "test.Foo.Bar" }));
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
    ~expect:
      (Some (Global.Attribute { name = Reference.create "test.Foo.x"; parent_is_class = true }));
  assert_resolve
    ~context
    ["test.py", {|
      x: int = 1
    |}]
    "test.x"
    ~expect:(Some (Global.Attribute { name = Reference.create "test.x"; parent_is_class = false }));
  assert_resolve
    ~context
    ["test.py", {|
      from typing import Any
      x: Any = 1
    |}]
    "test.x"
    ~expect:
      (Some (Global.UnknownAttribute { name = Reference.create "test.x"; parent_is_class = false }));
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
    ~expect:(Some (Global.Attribute { name = Reference.create "test.x"; parent_is_class = false }));

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
               ~return_annotation:Type.integer
               ~define_name:"test.foo"
               ~imported_name:"test.foo"
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
               ~define_name:"test.Foo.bar"
               ~is_method:true
               ~return_annotation:Type.integer
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
            {
              Function.define_name = Reference.create "test.Foo.bar";
              imported_name = None;
              undecorated_signature =
                Some
                  {
                    PyrePysaEnvironment.ModelQueries.FunctionSignature.overloads =
                      [
                        PyrePysaEnvironment.ModelQueries.FunctionParameters.List
                          [
                            create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                            create_parameter ~annotation:Type.integer "x";
                          ];
                        PyrePysaEnvironment.ModelQueries.FunctionParameters.List
                          [
                            create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                            create_parameter ~annotation:Type.integer "x";
                          ];
                        PyrePysaEnvironment.ModelQueries.FunctionParameters.List
                          [
                            create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                            create_parameter ~annotation:Type.string "x";
                          ];
                      ];
                    return_annotation = PyrePysaEnvironment.PysaType.from_pyre1_type Type.string;
                  };
              is_property_getter = false;
              is_property_setter = false;
              is_method = true;
            }));

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
    ~expect:
      (Some
         (Global.UnknownAttribute { name = Reference.create "test.Foo.baz"; parent_is_class = true }));

  (* Definition in type stub. *)
  assert_resolve
    ~context
    ["test.pyi", {|
      def foo() -> None: ...
    |}]
    "test.foo"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~return_annotation:Type.NoneType
               ~define_name:"test.foo"
               ~imported_name:"test.foo"
               ())));
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
               ~define_name:"test.Foo.bar"
               ~is_method:true
               ~return_annotation:Type.NoneType
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
    ~expect:
      (Some
         (Global.Function
            (create_callable ~define_name:"test.foo" ~return_annotation:Type.NoneType ())));
  assert_resolve
    ~context
    ["test.pyi", {|
      x: int = 1
    |}]
    "test.x"
    ~expect:(Some (Global.Attribute { name = Reference.create "test.x"; parent_is_class = false }));

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
            (create_callable
               ~return_annotation:Type.NoneType
               ~define_name:"outer.middle.inner.b.foo"
               ~imported_name:"outer.middle.inner.a.foo"
               ())));
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
               ~define_name:"outer.middle.inner.b.Foo.bar"
               ~imported_name:"outer.middle.inner.a.Foo.bar"
               ~is_method:true
               ~parameters:
                 [create_parameter ~annotation:(Type.Primitive "outer.middle.inner.a.Foo") "self"]
               ())));
  ()


let test_scalar_type_properties =
  let module ScalarTypeProperties = PyrePysaEnvironment.ScalarTypeProperties in
  let assert_scalar_properties annotation expected context =
    let project = Test.ScratchProject.setup ~context [] in
    let pyre_api = project |> Test.ScratchProject.pyre_pysa_read_only_api in
    let actual =
      PyrePysaEnvironment.ReadOnly.scalar_type_properties
        pyre_api
        (PyrePysaEnvironment.PysaType.from_pyre1_type annotation)
    in
    assert_equal ~printer:ScalarTypeProperties.show ~cmp:ScalarTypeProperties.equal expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties Type.bool ScalarTypeProperties.bool;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties Type.enumeration ScalarTypeProperties.enumeration;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties Type.integer ScalarTypeProperties.integer;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties (Type.optional Type.bool) ScalarTypeProperties.bool;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties (Type.optional Type.enumeration) ScalarTypeProperties.enumeration;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties (Type.optional Type.integer) ScalarTypeProperties.integer;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties Type.none ScalarTypeProperties.none;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties Type.Any ScalarTypeProperties.none;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties (Type.awaitable Type.bool) ScalarTypeProperties.bool;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties (Type.awaitable Type.enumeration) ScalarTypeProperties.enumeration;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties (Type.awaitable Type.integer) ScalarTypeProperties.integer;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties
           (Type.awaitable (Type.optional Type.bool))
           ScalarTypeProperties.bool;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties
           (Type.awaitable (Type.optional Type.enumeration))
           ScalarTypeProperties.enumeration;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_scalar_properties
           (Type.awaitable (Type.optional Type.integer))
           ScalarTypeProperties.integer;
    ]


let () =
  "pyrePysaApi"
  >::: [
         "source_is_unit_test" >:: test_source_is_unit_test;
         "resolve_qualified_name_to_global" >:: test_resolve_qualified_name_to_global;
         test_scalar_type_properties;
       ]
  |> Test.run
