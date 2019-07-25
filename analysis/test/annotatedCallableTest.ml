(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
        Statement.Define.signature =
          {
            name = !&"derp";
            parameters = [];
            decorators = [];
            docstring = None;
            return_annotation;
            async;
            parent = None;
          };
        body = [+Pass];
      }
      |> fun define ->
      Callable.return_annotation ~define ~resolution:(Environment.resolution environment ())
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation (Some (Type.expression Type.integer)) false Type.integer;
  assert_return_annotation
    (Some (Type.expression Type.integer))
    true
    (Type.coroutine (Concrete [Type.Any; Type.Any; Type.integer]))


let test_apply_decorators _ =
  let create_define ~decorators ~parameters ~return_annotation =
    let decorators = List.map ~f:parse_single_expression decorators in
    {
      Statement.Define.signature =
        {
          name = !&"define";
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
    let resolution = populate "" |> fun environment -> Environment.resolution environment () in
    let applied_return_annotation =
      Callable.apply_decorators ~resolution define
      |> fun { Type.Callable.annotation; _ } -> annotation
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected_return_annotation
      applied_return_annotation;

    (* Test decorators with old AST. *)
    let applied_return_annotation =
      Callable.apply_decorators ~resolution define
      |> fun { Type.Callable.annotation; _ } -> annotation
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
       ~decorators:["contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:(Some (+String (StringLiteral.create "typing.Iterator[str]"))))
    (Type.parametric "contextlib._GeneratorContextManager" (Concrete [Type.string]));
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:["contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some (+String (StringLiteral.create "typing.Generator[str, None, None]"))))
    (Type.parametric "contextlib._GeneratorContextManager" (Concrete [Type.string]));

  (* Click related tests *)
  let assert_apply_click_decorators ~expected_count define =
    let actual_count =
      let resolution = populate "" |> fun environment -> Environment.resolution environment () in
      Callable.apply_decorators ~resolution define
      |> fun { Type.Callable.parameters; _ } ->
      match parameters with
      | Undefined -> 0
      | ParameterVariadicTypeVariable _ -> 0
      | Defined parameters -> List.length parameters
    in
    assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_count actual_count
  in
  let create_parameter ~name = Parameter.create ~location:Location.Reference.any ~name () in
  create_define ~decorators:[] ~parameters:[create_parameter ~name:"test"] ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:1;
  create_define
    ~decorators:["click.neither_command_nor_group()"]
    ~parameters:[create_parameter ~name:"test"]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:1;
  create_define
    ~decorators:["click.command()"]
    ~parameters:[create_parameter ~name:"test"]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:2;
  create_define
    ~decorators:["click.group()"]
    ~parameters:[create_parameter ~name:"test"]
    ~return_annotation:None
  |> assert_apply_click_decorators ~expected_count:2;

  (* Custom decorators. *)
  let resolution = resolution () |> Resolution.global_resolution in
  create_define
    ~decorators:["$strip_first_parameter"]
    ~parameters:[create_parameter ~name:"self"; create_parameter ~name:"other"]
    ~return_annotation:None
  |> (fun define -> Callable.apply_decorators ~resolution define)
  |> fun { Type.Callable.parameters; _ } ->
  assert_equal
    ~printer:Type.Callable.show_parameters
    ~cmp:Type.Callable.equal_parameters
    (Type.Callable.Defined
       [Type.Callable.Parameter.Named { name = "other"; annotation = Type.Top; default = false }])
    parameters


let test_create_overload _ =
  let assert_overload source expected =
    let resolution = resolution () |> Resolution.global_resolution in
    assert_equal
      ~cmp:(Type.Callable.equal_overload Type.equal)
      expected
      ( source
      |> Test.parse_single_define
      |> fun define -> Callable.create_overload ~resolution define )
  in
  assert_overload
    {|
      def foo(x: int) -> None:
        pass
    |}
    {
      Type.Callable.annotation = Type.none;
      parameters =
        Type.Callable.Defined
          [Type.Callable.Parameter.Named { name = "x"; default = false; annotation = Type.integer }];
      define_location = None;
    }


let test_create _ =
  let assert_callable ?expected_implicit ?parent ~expected source =
    let resolution = populate source |> fun environment -> Environment.resolution environment () in
    let expected =
      GlobalResolution.parse_annotation resolution (parse_single_expression expected)
    in
    let check_implicit { Type.Callable.implicit = actual; _ } =
      match expected_implicit with
      (* Verify implicit if we're checking for it explicitly, ignore otherwise for convenience. *)
      | Some expected ->
          assert_equal ~cmp:(Option.equal Type.Callable.equal_implicit) (Some expected) actual
      | _ -> ()
    in
    let implicit =
      match expected with
      | Type.Callable { Type.Callable.implicit; _ } -> implicit
      | _ -> None
    in
    let callable =
      let parent_annotation = parent >>| fun parent -> Type.Primitive parent in
      let parent = parent >>| Reference.create in
      let defines = parse source |> Preprocessing.defines ~include_stubs:true |> List.rev in
      let { Define.signature = { Define.name; _ }; _ } = List.hd_exn defines |> Node.value in
      let to_overload define =
        Define.is_overloaded_method define, Callable.create_overload ~resolution define
      in
      defines
      |> List.map ~f:Node.value
      |> List.map ~f:(fun define ->
             let signature = { define.Define.signature with parent } in
             { define with signature })
      |> (fun defines -> List.map defines ~f:to_overload)
      |> Callable.create ~resolution ~parent:parent_annotation ~name:(Reference.show name)
      |> (fun callable ->
           check_implicit callable;
           callable)
      |> fun callable -> Type.Callable { callable with Type.Callable.implicit }
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected callable
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
    "def foo(__a: int, _b: str) -> str: ..."
    ~expected:"typing.Callable('foo')[[int, Named(_b, str)], str]";
  assert_callable
    "def foo(a, *args, **kwargs) -> str: ..."
    ~expected:"typing.Callable('foo')[[Named(a, $unknown), Variable(), Keywords()], str]";
  assert_callable
    "def foo(**kwargs: typing.Dict[str, typing.Any]) -> str: ..."
    ~expected:"typing.Callable('foo')[[Keywords(typing.Dict[str, typing.Any])], str]";
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
    ~expected:
      ( "typing.Callable('module.Foo.foo')[..., $unknown]"
      ^ "[[[Named(a, str)], str][[Named(a, int)], int]]" );
  assert_callable
    ~parent:"module.Foo"
    {|
        @overload
        def module.Foo.foo(self, a: int) -> int: ...
        @overload
        def module.Foo.foo(self, a: str, b: int) -> str: ...
      |}
    ~expected:
      ( "typing.Callable('module.Foo.foo')[..., $unknown]"
      ^ "[[[Named(a, str), Named(b, int)], str][[Named(a, int)], int]]" );
  assert_callable
    ~parent:"module.Foo"
    {|
        def module.Foo.foo(self, a: int) -> int: ...
        def module.Foo.foo(self, a: str) -> str: ...
      |}
    ~expected:"typing.Callable('module.Foo.foo')[[Named(a, str)], str]";
  assert_callable
    ~parent:"module.Foo"
    ~expected_implicit:{ implicit_annotation = Type.Primitive "module.Foo"; name = "self" }
    {|
      def module.Foo.foo(self, a: int) -> int: ...
    |}
    ~expected:"typing.Callable('module.Foo.foo')[[Named(a, int)], int]";
  assert_callable "def foo(*) -> int: ..." ~expected:"typing.Callable('foo')[[], int]";
  assert_callable
    "def foo(*, a: int) -> int: ..."
    ~expected:"typing.Callable('foo')[[KeywordOnly(a, int)], int]";
  assert_callable
    "def foo(*, a: int = 7) -> int: ..."
    ~expected:"typing.Callable('foo')[[KeywordOnly(a, int, default)], int]";
  assert_callable
    "def foo(x: str, *, a: int = 7) -> int: ..."
    ~expected:"typing.Callable('foo')[[Named(x, str), KeywordOnly(a, int, default)], int]";
  assert_callable
    "def foo(*var: int, a: int = 7) -> int: ..."
    ~expected:"typing.Callable('foo')[[Variable(int), KeywordOnly(a, int, default)], int]";
  assert_callable
    {|
      Ts = pyre_extensions.ListVariadic("Ts")
      def foo(x: str, y: int, *args: Ts, z: bool) -> typing.Tuple[Ts]: ...
    |}
    ~expected:
      "typing.Callable('foo')[[Named(x, str), Named(y, int), Variable(Ts), KeywordOnly(z, bool)], \
       typing.Tuple[Ts]]";
  ()


let () =
  "define"
  >::: [ "return_annotation" >:: test_return_annotation;
         "apply_decorators" >:: test_apply_decorators;
         "create_ovelroad" >:: test_create_overload;
         "create" >:: test_create ]
  |> Test.run
