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
        Statement.Define.signature = {
          name = Reference.create "derp";
          parameters = [];
          decorators = [];
          docstring = None;
          return_annotation;
          async;
          parent = None;
        };
        body = [+Pass];
      }
      |> (fun define ->
          Callable.return_annotation ~define ~resolution:(TypeCheck.resolution environment ()))
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation (Some (Type.expression Type.integer)) false Type.integer;
  assert_return_annotation
    (Some (Type.expression Type.integer))
    true
    (Type.coroutine [Type.Any; Type.Any; Type.integer])


let test_apply_decorators _ =
  let create_define ~decorators ~parameters ~return_annotation =
    {
      Statement.Define.signature = {
        name = Reference.create "define";
        parameters;
        decorators;
        docstring = None;
        return_annotation;
        async = false;
        parent = None;
      };
      body = [+Pass];
    }
  in

  (* Contextlib related tests *)
  let assert_apply_contextlib_decorators define expected_return_annotation =
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
      |> fun environment -> TypeCheck.resolution environment ()
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
  assert_apply_contextlib_decorators
    (create_define ~decorators:[] ~parameters:[] ~return_annotation:(Some !"str"))
    Type.string;
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:(Some (+String (StringLiteral.create "typing.Iterator[str]"))))
    (Type.parametric "contextlib.GeneratorContextManager" [Type.string]);
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:[!"contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some (+String (StringLiteral.create "typing.Generator[str, None, None]"))))
    (Type.parametric "contextlib.GeneratorContextManager" [Type.string]);

  (* Click related tests *)
  let assert_apply_click_decorators ~expected_count define =
    let actual_count =
      let resolution =
        populate ""
        |> fun environment -> TypeCheck.resolution environment ()
      in
      Callable.apply_decorators ~define ~resolution
      |> fun { Statement.Define.signature = { parameters; _ }; _ } -> List.length parameters
    in
    assert_equal
      ~cmp:Int.equal
      ~printer:Int.to_string
      expected_count
      actual_count
  in
  let make_click_decorator name =
    let access =
      Access.combine
        !"click"
        (Access.call ~location:Location.Reference.any ~name ())
    in
    +Access access
  in
  create_define
    ~decorators:[]
    ~parameters:[Parameter.create ~name:"test" ()]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:1;
  create_define
    ~decorators:[make_click_decorator "neither_command_nor_group"]
    ~parameters:[Parameter.create ~name:"test" ()]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:1;
  create_define
    ~decorators:[make_click_decorator "command"]
    ~parameters:[Parameter.create ~name:"test" ()]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:2;
  create_define
    ~decorators:[make_click_decorator "group"]
    ~parameters:[Parameter.create ~name:"test" ()]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:2;

  (* Custom decorators. *)
  let resolution = resolution () in
  create_define
    ~decorators:[!"$strip_first_parameter"]
    ~parameters:[Parameter.create ~name:"self" (); Parameter.create ~name:"other" ()]
    ~return_annotation:None
  |> (fun define -> Callable.apply_decorators ~define ~resolution)
  |> (fun { Define.signature = { parameters; _ }; _ } ->
      assert_equal
        ~cmp:(List.equal ~equal:(Parameter.equal Expression.equal))
        [Parameter.create ~name:"other" ()]
        parameters)


let test_create _ =
  let assert_callable ?expected_implicit ?parent ~expected source =
    let resolution =
      populate source
      |> fun environment -> TypeCheck.resolution environment ()
    in

    let expected = parse_callable expected in
    let check_implicit { Type.Callable.implicit = actual; _ } =
      match expected_implicit with
      (* Verify implicit if we're checking for it explicitly, ignore otherwise
         for convenience. *)
      | Some expected ->
          assert_equal
            ~cmp:(Option.equal Type.Callable.equal_implicit)
            (Some expected)
            actual
      | _ ->
          ()
    in
    let implicit =
      match expected with
      | Type.Callable { Type.Callable.implicit; _ } ->
          implicit
      | _ ->
          None
    in
    let callable =
      let parent_annotation = parent >>| fun parent -> Type.Primitive parent in
      let parent = parent >>| Reference.create in
      parse source
      |> Preprocessing.defines ~include_stubs:true
      |> List.rev
      |> List.map ~f:Node.value
      |> List.map ~f:(fun define ->
          let signature = { define.Define.signature with parent } in
          { define with signature }
        )
      |> Callable.create ~parent:parent_annotation ~resolution
      |> (fun callable -> check_implicit callable; callable)
      |> fun callable -> Type.Callable { callable with Type.Callable.implicit }
    in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      callable
  in

  assert_callable "def foo() -> int: ..." ~expected:"typing.Callable('foo')[[], int]";
  assert_callable
    "async def foo() -> int: ..."
    ~expected:"typing.Callable('foo')[[], typing.Coroutine[typing.Any, typing.Any, int]]";

  assert_callable
    "def foo(a, b) -> str: ..."
    ~expected:"typing.Callable('foo')[[Named(a, $unknown), Named(b, $unknown)], str]";
  assert_callable
    "def foo(a: int, b) -> str: ..."
    ~expected:"typing.Callable('foo')[[Named(a, int), Named(b, $unknown)], str]";
  assert_callable
    "def foo(a: int = 1) -> str: ..."
    ~expected:"typing.Callable('foo')[[Named(a, int, default)], str]";

  assert_callable
    "def foo(a, *args, **kwargs) -> str: ..."
    ~expected:"typing.Callable('foo')[[Named(a, $unknown), Variable(args), Keywords(kwargs)], str]";
  assert_callable
    "def foo(**kwargs: typing.Dict[str, typing.Any]) -> str: ..."
    ~expected:"typing.Callable('foo')[[Keywords(kwargs, typing.Dict[str, typing.Any])], str]";

  assert_callable
    ~parent:"module.Foo"
    "def module.Foo.foo(a: int, b) -> str: ..."
    ~expected:"typing.Callable('module.Foo.foo')[[Named(b, $unknown)], str]";

  assert_callable
    ~parent:"module.Foo"
    {|
      @overload
      def module.Foo.foo(self, a: int) -> int: ...
      @overload
      def module.Foo.foo(self, a: str) -> str: ...
    |}
    ~expected:(
      "typing.Callable('module.Foo.foo')[..., $unknown]" ^
      "[[[Named(a, str)], str][[Named(a, int)], int]]");

  assert_callable
    ~parent:"module.Foo"
    {|
        @overload
        def module.Foo.foo(self, a: int) -> int: ...
        @overload
        def module.Foo.foo(self, a: str, b: int) -> str: ...
      |}
    ~expected:(
      "typing.Callable('module.Foo.foo')[..., $unknown]" ^
      "[[[Named(a, str), Named(b, int)], str][[Named(a, int)], int]]");

  assert_callable
    ~parent:"module.Foo"
    {|
        def module.Foo.foo(self, a: int) -> int: ...
        def module.Foo.foo(self, a: str) -> str: ...
      |}
    ~expected:(
      "typing.Callable('module.Foo.foo')[[Named(a, str)], str]");

  assert_callable
    ~parent:"module.Foo"
    ~expected_implicit:{
      implicit_annotation = Type.Primitive "module.Foo";
      name = "self";
    }
    {|
      def module.Foo.foo(self, a: int) -> int: ...
    |}
    ~expected:("typing.Callable('module.Foo.foo')[[Named(a, int)], int]")


let () =
  "define">:::[
    "return_annotation">::test_return_annotation;
    "apply_decorators">::test_apply_decorators;
    "create">::test_create;
  ]
  |> Test.run;
