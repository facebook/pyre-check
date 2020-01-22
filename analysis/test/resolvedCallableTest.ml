(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Expression
open Statement
open Test

let test_apply_decorators context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
  let create_define ~decorators ~parameters ~return_annotation =
    (let decorators = List.map ~f:parse_single_expression decorators in
     {
       Define.Signature.name = + !&"define";
       parameters;
       decorators;
       return_annotation;
       async = false;
       generator = false;
       parent = None;
       nesting_define = None;
     })
    |> Node.create_with_default_location
  in
  (* Contextlib related tests *)
  let assert_apply_contextlib_decorators define expected_return_annotation =
    let applied_return_annotation =
      GlobalResolution.create_overload ~resolution define
      |> fun { Type.Callable.annotation; _ } -> annotation
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected_return_annotation
      applied_return_annotation;

    (* Test decorators with old AST. *)
    let applied_return_annotation =
      GlobalResolution.create_overload ~resolution define
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
       ~return_annotation:(Some (+Expression.String (StringLiteral.create "typing.Iterator[str]"))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string]);
  assert_apply_contextlib_decorators
    (create_define
       ~decorators:["contextlib.contextmanager"]
       ~parameters:[]
       ~return_annotation:
         (Some (+Expression.String (StringLiteral.create "typing.Generator[str, None, None]"))))
    (Type.parametric "contextlib._GeneratorContextManager" [Single Type.string]);

  (* Click related tests *)
  let assert_apply_click_decorators ~expected_count define =
    let actual_count =
      let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution in
      GlobalResolution.create_overload ~resolution define
      |> fun { Type.Callable.parameters; _ } ->
      match parameters with
      | Undefined -> 0
      | ParameterVariadicTypeVariable _ -> 0
      | Defined parameters -> List.length parameters
    in
    assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_count actual_count
  in
  let create_parameter ~name = Parameter.create ~location:Location.any ~name () in
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
  create_define
    ~decorators:["$strip_first_parameter"]
    ~parameters:[create_parameter ~name:"self"; create_parameter ~name:"other"]
    ~return_annotation:None
  |> (fun define -> GlobalResolution.create_overload ~resolution define)
  |> fun { Type.Callable.parameters; _ } ->
  assert_equal
    ~printer:Type.Callable.show_parameters
    ~cmp:Type.Callable.equal_parameters
    (Type.Callable.Defined
       [Type.Callable.Parameter.Named { name = "other"; annotation = Type.Top; default = false }])
    parameters


let test_create context =
  let assert_callable ?expected_implicit ?parent ~expected source =
    let { ScratchProject.BuiltGlobalEnvironment.ast_environment; global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let expected =
      GlobalResolution.parse_annotation resolution (parse_single_expression expected)
    in
    let check_implicit { Type.Callable.implicit = actual; _ } =
      match expected_implicit with
      (* Verify implicit if we're checking for it explicitly, ignore otherwise for convenience. *)
      | Some expected ->
          let printer = function
            | None -> "No implicit"
            | Some implicit -> Type.Callable.show_implicit implicit
          in
          assert_equal
            ~printer
            ~cmp:(Option.equal Type.Callable.equal_implicit)
            (Some expected)
            actual
      | _ -> ()
    in
    let implicit =
      match expected with
      | Type.Callable { Type.Callable.implicit; _ } -> implicit
      | _ -> None
    in
    let callable =
      let add_decorators ({ Ast.Statement.Define.Signature.decorators; _ } as define) =
        let decorators =
          if Option.is_some parent then
            decorators
          else
            Test.parse_single_expression "staticmethod" :: decorators
        in
        { define with decorators }
      in
      let parent = Option.value parent ~default:"toast" in
      let parent = parent |> Reference.create in
      let defines =
        AstEnvironment.ReadOnly.get_source
          (AstEnvironment.read_only ast_environment)
          (Reference.create "test")
        |> (fun option -> Option.value_exn option)
        |> Preprocessing.defines ~include_stubs:true
        |> List.rev
      in
      let { Define.signature = { name = _; _ }; _ } = List.hd_exn defines |> Node.value in
      defines
      |> List.map ~f:Node.value
      |> List.map ~f:(fun define -> { define.Define.signature with parent = Some parent })
      |> List.map ~f:add_decorators
      |> (fun signatures ->
           Node.create_with_default_location
             {
               Ast.Statement.Attribute.name = "A";
               kind = Method { signatures; static = false; final = false };
             })
      |> GlobalResolution.create_attribute
           ~resolution
           ~parent:
             (Node.create_with_default_location
                {
                  ClassSummary.name = parent;
                  bases = [];
                  decorators = [];
                  attribute_components = Ast.Statement.Class.AttributeComponents.empty ();
                })
      |> Annotated.Attribute.annotation
      |> Annotation.annotation
      |> (function
           | Callable callable -> callable
           | _ -> failwith "not a callable")
      |> (fun callable ->
           check_implicit callable;
           callable)
      |> fun callable -> Type.Callable { callable with Type.Callable.implicit }
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected callable
  in
  assert_callable "def foo() -> int: ..." ~expected:"typing.Callable('test.foo')[[], int]";
  assert_callable
    "async def foo() -> int: ..."
    ~expected:"typing.Callable('test.foo')[[], typing.Coroutine[typing.Any, typing.Any, int]]";
  assert_callable
    "def foo(a, b) -> str: ..."
    ~expected:"typing.Callable('test.foo')[[Named(a, $unknown), Named(b, $unknown)], str]";
  assert_callable
    "def foo(a: int, b) -> str: ..."
    ~expected:"typing.Callable('test.foo')[[Named(a, int), Named(b, $unknown)], str]";
  assert_callable
    "def foo(a: int = 1) -> str: ..."
    ~expected:"typing.Callable('test.foo')[[Named(a, int, default)], str]";
  assert_callable
    "def foo(__a: int, _b: str) -> str: ..."
    ~expected:"typing.Callable('test.foo')[[int, Named(_b, str)], str]";
  assert_callable
    "def foo(a, *args, **kwargs) -> str: ..."
    ~expected:"typing.Callable('test.foo')[[Named(a, $unknown), Variable(), Keywords()], str]";
  assert_callable
    "def foo(**kwargs: typing.Dict[str, typing.Any]) -> str: ..."
    ~expected:"typing.Callable('test.foo')[[Keywords(typing.Dict[str, typing.Any])], str]";
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
    ~expected_implicit:
      { implicit_annotation = Type.Primitive "module.Foo"; name = "$parameter$self" }
    {|
      def module.Foo.foo(self, a: int) -> int: ...
    |}
    ~expected:"typing.Callable('module.Foo.foo')[[Named(a, int)], int]";
  assert_callable "def foo(*) -> int: ..." ~expected:"typing.Callable('test.foo')[[], int]";
  assert_callable
    "def foo(*, a: int) -> int: ..."
    ~expected:"typing.Callable('test.foo')[[KeywordOnly($parameter$a, int)], int]";
  assert_callable
    "def foo(*, a: int = 7) -> int: ..."
    ~expected:"typing.Callable('test.foo')[[KeywordOnly($parameter$a, int, default)], int]";
  assert_callable
    "def foo(x: str, *, a: int = 7) -> int: ..."
    ~expected:
      "typing.Callable('test.foo')[[Named($parameter$x, str), KeywordOnly($parameter$a, int, \
       default)], int]";
  assert_callable
    "def foo(*var: int, a: int = 7) -> int: ..."
    ~expected:
      "typing.Callable('test.foo')[[Variable(int), KeywordOnly($parameter$a, int, default)], int]";
  assert_callable
    {|
      Ts = pyre_extensions.ListVariadic("Ts")
      def foo(x: str, y: int, *args: Ts, z: bool) -> typing.Tuple[Ts]: ...
    |}
    ~expected:
      "typing.Callable('test.foo')[[Named($parameter$x, str), Named($parameter$y, int), \
       Variable(test.Ts), KeywordOnly($parameter$z, bool)], typing.Tuple[test.Ts]]";
  ();
  assert_callable
    ~parent:"module.Foo"
    {|
        @overload
        def module.Foo.foo(self, a: int) -> int: ...
        @overload
        def module.Foo.foo(self, a: str, b: int) -> str: ...
      |}
    ~expected_implicit:
      { Type.Callable.name = "$parameter$self"; implicit_annotation = Type.Primitive "module.Foo" }
    ~expected:
      ( "typing.Callable('module.Foo.foo')[..., $unknown]"
      ^ "[[[Named(a, str), Named(b, int)], str][[Named(a, int)], int]]" )


let () =
  "resolvedCallable"
  >::: ["apply_decorators" >:: test_apply_decorators; "create" >:: test_create]
  |> Test.run
