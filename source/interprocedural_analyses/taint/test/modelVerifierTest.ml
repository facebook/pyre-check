(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Core
open OUnit2
open Test
open Ast
open Taint
module Global = ModelVerifier.Global

let assert_resolve ~context sources name ~expect =
  let resolution = ScratchProject.setup ~context sources |> ScratchProject.build_resolution in
  let actual = ModelVerifier.resolve_global ~resolution (Ast.Reference.create name) in
  let printer = function
    | None -> "None"
    | Some global -> Global.show global
  in
  assert_equal ~printer expect actual


let create_parameter ?(annotation = Type.Any) ?(default = false) name =
  Type.Callable.RecordParameter.Named { name = "$parameter$" ^ name; annotation; default }


let create_callable ?name ?(overloads = []) ?(parameters = []) ?(annotation = Type.Any) () =
  Type.Callable.create
    ?name:(name >>| Reference.create)
    ~overloads
    ~parameters:(Type.Callable.Defined parameters)
    ~annotation
    ()


let test_resolve context =
  (* Most common cases. *)
  assert_resolve
    ~context
    ["test.py", {|
      def foo():
        return
    |}]
    "test.foo"
    ~expect:(Some (Global.Attribute (create_callable ~name:"test.foo" ())));
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
         (Global.Attribute
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
    ~expect:(Some (Global.Attribute (create_callable ~annotation:Type.NoneType ())));
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
    ~expect:(Some (Global.Attribute (create_callable ~annotation:Type.NoneType ())));
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
    ~expect:(Some (Global.Attribute Type.integer));
  assert_resolve
    ~context
    ["test.py", {|
      x: int = 1
    |}]
    "test.x"
    ~expect:(Some (Global.Attribute Type.integer));
  assert_resolve
    ~context
    ["test.py", {|
      from typing import Any
      x: Any = 1
    |}]
    "test.x"
    ~expect:(Some (Global.Attribute Type.Any));
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
    ~expect:
      (Some
         (Global.Attribute
            (Type.Parametric { name = "type"; parameters = [Single (Type.Primitive "test.Foo")] })));

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
         (Global.Attribute
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
         (Global.Attribute
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
         (Global.Attribute
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
    ~expect:(Some (Global.Attribute Type.Top));

  (* Definition in type stub. *)
  assert_resolve
    ~context
    ["test.pyi", {|
      def foo() -> None: ...
    |}]
    "test.foo"
    ~expect:
      (Some (Global.Attribute (create_callable ~annotation:Type.NoneType ~name:"test.foo" ())));
  assert_resolve
    ~context
    ["test.pyi", {|
      class Foo:
        def bar(self) -> None: ...
    |}]
    "test.Foo.bar"
    ~expect:
      (Some
         (Global.Attribute
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
    ~expect:(Some (Global.Attribute (create_callable ~annotation:Type.NoneType ())));
  assert_resolve
    ~context
    ["test.pyi", {|
      x: int = 1
    |}]
    "test.x"
    ~expect:(Some (Global.Attribute Type.integer))


let () = "model_verifier" >::: ["resolve" >:: test_resolve] |> Test.run
