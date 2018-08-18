(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre
open Statement

open Test
open AnnotatedTest

module Callable = Annotated.Callable


let test_return_annotation _ =
  let assert_return_annotation return_annotation async expected =
    let return_annotation =
      let environment =
        populate {|
          class foo():
            def bar(): pass
        |}
      in
      {
        Statement.Define.name = Access.create "derp";
        parameters = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async;
        generated = false;
        parent = None;
      }
      |> (fun define ->
          Callable.return_annotation ~define ~resolution:(Environment.resolution environment ()))
    in
    assert_equal ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation (Some (Type.expression Type.integer)) false Type.integer;
  assert_return_annotation (Some (Type.expression Type.integer)) true (Type.awaitable Type.integer)


let test_apply_decorators _ =
  let assert_apply_decorators define expected_return_annotation =
    let resolution =
      populate {|
        _T = typing.TypeVar("T")
        _T2 = typing.TypeVar("T2")
        _T3 = typing.TypeVar("T3")
        class typing.Iterator(typing.Generic[_T]):
          pass
        class typing.Generator(typing.Iterator[_T], typing.Generic[_T, _T2, _T3]):
          pass
        class contextlib.GeneratorContextManager(contextlib.ContextManager[_T], typing.Generic[_T]):
          pass
      |}
      |> fun environment -> Environment.resolution environment ()
    in
    let applied_return_annotation =
      Callable.apply_decorators ~define ~resolution
      |> (fun define -> Callable.return_annotation ~define ~resolution)
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected_return_annotation
      applied_return_annotation
  in
  let create_define ~decorators ~return_annotation =
    {
      Statement.Define.name = Access.create "define";
      parameters = [];
      body = [+Pass];
      decorators;
      docstring = None;
      return_annotation;
      async = false;
      generated = false;
      parent = None;
    }
  in
  assert_apply_decorators
    (create_define ~decorators:[] ~return_annotation:(Some !"str"))
    Type.string;
  assert_apply_decorators
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~return_annotation:(Some (+String (StringLiteral.create "typing.Iterator[str]"))))
    (Type.Parametric {
        Type.name = ~~"contextlib.GeneratorContextManager";
        parameters = [Type.string];
      });
  assert_apply_decorators
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~return_annotation:
         (Some (+String (StringLiteral.create "typing.Generator[str, None, None]"))))
    (Type.Parametric {
        Type.name = ~~"contextlib.GeneratorContextManager";
        parameters = [Type.string];
      })


let test_create _ =
  let assert_callable ?parent source expected =
    let resolution =
      populate source
      |> fun environment -> Environment.resolution environment ()
    in
    let callable =
      let parent = parent >>| Access.create in
      parse source
      |> Preprocessing.defines ~include_stubs:true
      |> List.map ~f:Node.value
      |> List.map ~f:(fun define -> { define with Statement.Define.parent })
      |> Callable.create ~resolution
      |> fun callable -> Type.Callable callable
    in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      (parse_callable expected)
      callable
  in

  assert_callable "def foo() -> int: ..." "typing.Callable('foo')[[], int]";
  assert_callable "async def foo() -> int: ..." "typing.Callable('foo')[[], typing.Awaitable[int]]";

  assert_callable
    "def foo(a, b) -> str: ..."
    "typing.Callable('foo')[[Named(a, $unknown), Named(b, $unknown)], str]";
  assert_callable
    "def foo(a: int, b) -> str: ..."
    "typing.Callable('foo')[[Named(a, int), Named(b, $unknown)], str]";
  assert_callable
    "def foo(a: int = 1) -> str: ..."
    "typing.Callable('foo')[[Named(a, int, default)], str]";

  assert_callable
    "def foo(a, *args, **kwargs) -> str: ..."
    "typing.Callable('foo')[[Named(a, $unknown), Variable(args), Keywords(kwargs)], str]";
  assert_callable
    "def foo(**kwargs: typing.Dict[str, typing.Any]) -> str: ..."
    "typing.Callable('foo')[[Keywords(kwargs, typing.Dict[str, typing.Any])], str]";

  assert_callable
    ~parent:"module.Foo"
    "def module.Foo.foo(a: int, b) -> str: ..."
    "typing.Callable('module.Foo.foo')[[Named(a, int), Named(b, $unknown)], str]";

  assert_callable
    ~parent:"module.Foo"
    {|
      @overload
      def module.Foo.foo(a: int) -> int: ...
      @overload
      def module.Foo.foo(a: str) -> str: ...
    |}
    (* Note that the overload order is reversed from the declaration - shouldn't matter. *)
    "typing.Callable('module.Foo.foo')[[Named(a, str)], str][[Named(a, int)], int]";

  let assert_implicit_argument ?parent source expected =
    let resolution =
      populate source
      |> fun environment -> Environment.resolution environment ()
    in
    let implicit_argument =
      let parent = parent >>| Access.create in
      parse_single_define source
      |> (fun define -> { define with Statement.Define.parent })
      |> (fun define -> Callable.create ~resolution [define])
      |> fun { Type.Callable.implicit; _ } -> implicit
    in
    assert_equal
      expected
      implicit_argument
  in
  assert_implicit_argument "def foo(self) -> None: ..." Type.Callable.Function;
  assert_implicit_argument ~parent:"module.Foo" "def foo(self) -> None: ..." Type.Callable.Instance;
  assert_implicit_argument
    ~parent:"module.Foo"
    {|
      @classmethod
      def foo(cls, other) -> None: ...
    |}
    Type.Callable.Class;
  assert_implicit_argument
    ~parent:"module.Foo"
    {|
      @staticmethod
      def foo(other) -> None: ...
    |}
    Type.Callable.Function


let () =
  "define">:::[
    "return_annotation">::test_return_annotation;
    "apply_decorators">::test_apply_decorators;
    "create">::test_create;
  ]
  |> run_test_tt_main;
