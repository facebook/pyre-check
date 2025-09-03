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
open Interprocedural
module PysaType = PyrePysaApi.PysaType
module ScalarTypeProperties = PyrePysaApi.ScalarTypeProperties
module FunctionParameter = PyrePysaApi.ModelQueries.FunctionParameter
module FunctionParameters = PyrePysaApi.ModelQueries.FunctionParameters
module FunctionSignature = PyrePysaApi.ModelQueries.FunctionSignature
module Function = PyrePysaApi.ModelQueries.Function
module Global = PyrePysaApi.ModelQueries.Global

let convert_to_pyrefly_global global =
  (* Manually convert all the type used in this file. *)
  let convert_to_pyrefly_type annotation =
    match PysaType.as_pyre1_type annotation with
    | Some Type.NoneType ->
        PysaType.from_pyrefly_type
          {
            PyrePysaEnvironment.PyreflyType.string = "None";
            scalar_properties = ScalarTypeProperties.none;
            class_names = [];
          }
    | Some Type.Any ->
        PysaType.from_pyrefly_type
          {
            PyrePysaEnvironment.PyreflyType.string = "Any";
            scalar_properties = ScalarTypeProperties.none;
            class_names = [];
          }
    | Some (Type.Primitive "int") ->
        PysaType.from_pyrefly_type
          {
            PyrePysaEnvironment.PyreflyType.string = "int";
            scalar_properties = ScalarTypeProperties.integer;
            class_names = ["builtins.int"];
          }
    | Some (Type.Primitive "str") ->
        PysaType.from_pyrefly_type
          {
            PyrePysaEnvironment.PyreflyType.string = "str";
            scalar_properties = ScalarTypeProperties.none;
            class_names = ["builtins.str"];
          }
    | Some (Type.Primitive "test.Foo") ->
        PysaType.from_pyrefly_type
          {
            PyrePysaEnvironment.PyreflyType.string = "test.Foo";
            scalar_properties = ScalarTypeProperties.none;
            class_names = ["test.Foo"];
          }
    | Some annotation ->
        failwith (Format.asprintf "unimplemented: pyrefly representation for %a" Type.pp annotation)
    | None -> annotation
  in
  let convert_to_pyrefly_parameter = function
    | FunctionParameter.PositionalOnly { name; index; annotation; has_default } ->
        FunctionParameter.PositionalOnly
          {
            name = name >>| Identifier.sanitized;
            index;
            annotation = convert_to_pyrefly_type annotation;
            has_default;
          }
    | FunctionParameter.Named { name; annotation; has_default } ->
        FunctionParameter.Named
          {
            name = Identifier.sanitized name;
            annotation = convert_to_pyrefly_type annotation;
            has_default;
          }
    | FunctionParameter.KeywordOnly { name; annotation; has_default } ->
        FunctionParameter.KeywordOnly
          {
            name = Identifier.sanitized name;
            annotation = convert_to_pyrefly_type annotation;
            has_default;
          }
    | FunctionParameter.Variable { name } ->
        FunctionParameter.Variable { name = name >>| Identifier.sanitized }
    | FunctionParameter.Keywords { name; annotation } ->
        FunctionParameter.Keywords
          { name = name >>| Identifier.sanitized; annotation = convert_to_pyrefly_type annotation }
  in
  let convert_to_pyrefly_parameters = function
    | FunctionParameters.List parameters ->
        FunctionParameters.List (List.map ~f:convert_to_pyrefly_parameter parameters)
    | parameter -> parameter
  in
  let convert_to_pyrefly_signature { FunctionSignature.parameters; return_annotation } =
    {
      FunctionSignature.parameters = convert_to_pyrefly_parameters parameters;
      return_annotation = convert_to_pyrefly_type return_annotation;
    }
  in
  match global with
  | Global.Function ({ Function.undecorated_signatures; _ } as function_) ->
      let undecorated_signatures =
        undecorated_signatures >>| List.map ~f:convert_to_pyrefly_signature
      in
      Global.Function
        {
          function_ with
          (* pyrefly doesn't have the concept of imported names. *)
          imported_name = None;
          undecorated_signatures;
        }
  | global -> global


let test_resolve_qualified_name_to_global context =
  let assert_resolve ~context ?pyrefly_expect sources name ~expect =
    let pyre_api =
      Test.ScratchPyrePysaProject.setup ~context ~requires_type_of_expressions:false sources
      |> Test.ScratchPyrePysaProject.read_only_api
    in
    let actual =
      PyrePysaApi.ModelQueries.resolve_qualified_name_to_global
        pyre_api
        ~is_property_getter:false
        ~is_property_setter:false
        (Ast.Reference.create name)
    in
    let expect =
      match pyrefly_expect with
      | Some pyrefly_expect when PyrePysaApi.ReadOnly.is_pyrefly pyre_api -> pyrefly_expect
      | _ -> expect
    in
    let expect =
      if PyrePysaApi.ReadOnly.is_pyrefly pyre_api then
        expect >>| convert_to_pyrefly_global
      else
        expect
    in
    let () = PyrePysaApi.ModelQueries.invalidate_cache pyre_api in
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
  let create_signature ?(return_annotation = Type.NoneType) parameters =
    {
      PyrePysaEnvironment.ModelQueries.FunctionSignature.parameters =
        PyrePysaEnvironment.ModelQueries.FunctionParameters.List parameters;
      return_annotation = PyrePysaEnvironment.PysaType.from_pyre1_type return_annotation;
    }
  in
  let create_callable
      ~define_name
      ?imported_name
      ?(is_method = false)
      ?(signatures = [create_signature []])
      ()
    =
    let define_name = Reference.create define_name in
    {
      Function.define_name;
      imported_name = imported_name >>| Reference.create;
      undecorated_signatures = Some signatures;
      is_property_getter = false;
      is_property_setter = false;
      is_method;
    }
  in
  (* Most common cases. *)
  assert_resolve
    ~context
    ["test.py", {|
      def foo() -> None:
        return
    |}]
    "test.foo"
    ~expect:
      (Some (Global.Function (create_callable ~define_name:"test.foo" ~imported_name:"test.foo" ())));
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        def bar(self) -> None:
          return
    |}]
    "test.Foo.bar"
    ~expect:
      (Some
         (Global.Function
            (create_callable
               ~define_name:"test.Foo.bar"
               ~is_method:true
               ~signatures:
                 [
                   create_signature [create_parameter ~annotation:(Type.Primitive "test.Foo") "self"];
                 ]
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
            (create_callable
               ~define_name:"test.foo"
               ~signatures:[create_signature ~return_annotation:Type.NoneType []]
               ())))
    ~pyrefly_expect:
      (Some (Global.Attribute { name = Reference.create "test.foo"; parent_is_class = false }));
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
               ~signatures:[create_signature ~return_annotation:Type.NoneType []]
               ())))
    ~pyrefly_expect:
      (Some (Global.Attribute { name = Reference.create "test.Foo.bar"; parent_is_class = true }));
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
      def foo() -> None:
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
      (Some (Global.UnknownAttribute { name = Reference.create "test.x"; parent_is_class = false }))
    ~pyrefly_expect:
      (Some (Global.Attribute { name = Reference.create "test.x"; parent_is_class = false }));
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
      def foo() -> None:
        return
    |}]
    "test.bar"
    ~expect:None;
  assert_resolve
    ~context
    ["test.py", {|
      class Foo:
        def bar() -> None:
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
               ~define_name:"test.foo"
               ~imported_name:"test.foo"
               ~signatures:
                 [
                   create_signature
                     ~return_annotation:Type.integer
                     [create_parameter ~annotation:Type.integer "x"];
                 ]
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
               ~signatures:
                 [
                   create_signature
                     ~return_annotation:Type.integer
                     [
                       create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                       create_parameter ~annotation:Type.integer "x";
                     ];
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
            (create_callable
               ~define_name:"test.Foo.bar"
               ~is_method:true
               ~signatures:
                 [
                   create_signature
                     ~return_annotation:Type.string
                     [
                       create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                       create_parameter ~annotation:Type.integer "x";
                     ];
                   create_signature
                     ~return_annotation:Type.string
                     [
                       create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                       create_parameter ~annotation:Type.integer "x";
                     ];
                   create_signature
                     ~return_annotation:Type.integer
                     [
                       create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                       create_parameter ~annotation:Type.string "x";
                     ];
                 ]
               ())))
    ~pyrefly_expect:
      (Some
         (Global.Function
            (create_callable
               ~define_name:"test.Foo.bar"
               ~is_method:true
               ~signatures:
                 [
                   create_signature
                     ~return_annotation:Type.string
                     [
                       create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                       create_parameter ~annotation:Type.integer "x";
                     ];
                   create_signature
                     ~return_annotation:Type.integer
                     [
                       create_parameter ~annotation:(Type.Primitive "test.Foo") "self";
                       create_parameter ~annotation:Type.string "x";
                     ];
                 ]
               ())));

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
         (Global.UnknownAttribute { name = Reference.create "test.Foo.baz"; parent_is_class = true }))
    ~pyrefly_expect:None;

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
               ~define_name:"test.foo"
               ~imported_name:"test.foo"
               ~signatures:[create_signature ~return_annotation:Type.NoneType []]
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
               ~signatures:
                 [
                   create_signature
                     ~return_annotation:Type.NoneType
                     [create_parameter ~annotation:(Type.Primitive "test.Foo") "self"];
                 ]
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
            (create_callable
               ~define_name:"test.foo"
               ~signatures:[create_signature ~return_annotation:Type.NoneType []]
               ())))
    ~pyrefly_expect:
      (Some (Global.Attribute { name = Reference.create "test.foo"; parent_is_class = false }));
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
               ~define_name:"outer.middle.inner.b.foo"
               ~imported_name:"outer.middle.inner.a.foo"
               ~signatures:[create_signature ~return_annotation:Type.NoneType []]
               ())))
    ~pyrefly_expect:None;
  assert_resolve
    ~context
    [
      "outer/middle/inner/a.py", {|
      class Foo:
        def bar(self) -> None: ...
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
               ~signatures:
                 [
                   create_signature
                     [
                       create_parameter
                         ~annotation:(Type.Primitive "outer.middle.inner.a.Foo")
                         "self";
                     ];
                 ]
               ())))
    ~pyrefly_expect:None;
  ()


let () =
  "pyrePysaApi"
  >::: ["resolve_qualified_name_to_global" >:: test_resolve_qualified_name_to_global]
  |> Test.run
