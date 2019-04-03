(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Statement
open Test


let test_set_local _ =
  let assert_local ~resolution ~access ~expected =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:(fun _ -> None))
      (Resolution.get_local resolution ~access:(!+access) >>| Annotation.annotation)
  in

  let resolution = Test.resolution ~sources:[] () in
  assert_local ~resolution ~access:"local" ~expected:None;

  let resolution =
    Resolution.set_local
      resolution
      ~access:(!+"local")
      ~annotation:(Annotation.create Type.integer)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "int");

  let resolution =
    Resolution.set_local
      resolution
      ~access:(!+"local")
      ~annotation:(Annotation.create Type.float)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "float")


let test_parse_annotation _ =
  let assert_parse_annotation ?(allow_untracked=false) ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:(fun _ -> None))
      (parse_single_expression expression
       |> Resolution.parse_annotation ~allow_untracked resolution)
  in

  let resolution =
    Test.resolution
      ~sources:([
          parse ~qualifier:(!&"empty") ~handle:"empty.pyi" "class Empty: ...";
          parse
            ~qualifier:(!&"empty.stub")
            ~local_mode:Source.PlaceholderStub
            ~handle:"empty/stub.pyi"
            "";
        ] @ (Test.typeshed_stubs ()))
      ()
  in
  assert_parse_annotation ~resolution ~expected:"int" "int";
  assert_parse_annotation
    ~allow_untracked:true
    ~resolution
    ~expected:"qualifier.int"
    "$local_qualifier$int";
  assert_parse_annotation ~resolution ~expected:"typing.Any" "empty.stub.Annotation";
  assert_parse_annotation
    ~resolution
    ~expected:"typing.Dict[str, typing.Any]"
    "typing.Dict[str, empty.stub.Annotation]"


let make_resolution source =
  let configuration = Configuration.Analysis.create () in
  let populate source =
    let environment =
      let environment = Environment.Builder.create () in
      Test.populate
        ~configuration
        (Environment.handler environment)
        (parse source :: typeshed_stubs ());
      environment
    in
    Environment.handler environment
  in
  populate source
  |> fun environment -> TypeCheck.resolution environment ()


let test_parse_reference _ =
  let resolution =
    make_resolution
      {|
      import typing
      class Foo: ...
      MyType = int
    |}
  in
  let assert_parse_reference reference expected =
    assert_equal
      ~printer:Type.show expected
      (Resolution.parse_reference resolution (!&reference))
  in
  assert_parse_reference "undefined" Type.Top;
  assert_parse_reference "MyType" Type.integer;
  assert_parse_reference "Foo" (Type.Primitive "Foo");
  assert_parse_reference "typing.List" (Type.list Type.Any)


let test_resolve_literal _ =
  let resolution =
    make_resolution
      {|
      class C:
        def __init__(self) -> None:
          pass
      T = typing.TypeVar("T")
      class G(typing.Generic[T]):
        def __init__(self, x: T) -> None:
          pass
      def foo()->int:
        ...
      i = 1
      j = foo()
      s = 'asdf'
      t = 1, 1.0
      none = None
      awaitable: typing.Awaitable[int]
    |}
  in
  let assert_resolve_literal source expected =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    assert_equal ~printer:Type.show expected (Resolution.resolve_literal resolution expression)
  in
  assert_resolve_literal "i" Type.Top;
  assert_resolve_literal "await i" Type.Top;
  assert_resolve_literal "await awaitable" Type.Top;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Any;
  assert_resolve_literal "j" Type.Top;
  assert_resolve_literal "foo()" Type.Top;
  assert_resolve_literal "C()" (Type.Primitive "C");
  assert_resolve_literal "G(7)" Type.Top;
  assert_resolve_literal "C" (Type.meta (Type.Primitive "C"));
  assert_resolve_literal "G" Type.Top;

  (* None *)
  assert_resolve_literal "None" (Type.none);
  assert_resolve_literal "[None]" (Type.list Type.none);

  (* Dictionary *)
  assert_resolve_literal "{'a': 1}" (Type.dictionary ~key:Type.string ~value:Type.integer);
  assert_resolve_literal "{'a': i}" (Type.Any);
  assert_resolve_literal "{**foo}" Type.Any;
  assert_resolve_literal "{'a': 1, **foo}" Type.Any;

  (* Boolean Operator *)
  assert_resolve_literal "1 or 2" (Type.integer);
  assert_resolve_literal "True or 1" (Type.union [Type.bool; Type.integer]);
  assert_resolve_literal "True or i" (Type.Any);

  (* List *)
  assert_resolve_literal "[1]" (Type.list Type.integer);
  assert_resolve_literal "[1, 'string']" (Type.list (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "[1, i]" (Type.Any);

  (* Set *)
  assert_resolve_literal "{1}" (Type.set Type.integer);
  assert_resolve_literal "{1, 'string'}" (Type.set (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "{1, i}" (Type.Any);

  (* Ternary *)
  assert_resolve_literal "1 if x else 2" (Type.integer);
  assert_resolve_literal "'hi' if x else 1" (Type.union [Type.string; Type.integer]);
  assert_resolve_literal "1 if i else i" (Type.Any)


let test_resolve_mutable_literals _ =
  let resolution =
    make_resolution
      {|
      class C: ...
      class D(C): ...
      class Q: ...
    |}
  in
  let assert_resolve_mutable_literals ~source ~against expected_output =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> Resolution.parse_annotation resolution
    in
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    let resolved = Resolution.resolve resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    assert_equal
      ~printer:Type.show
      (parse_annotation expected_output)
      (Resolution.resolve_mutable_literals resolution ~expression ~resolved ~expected)
  in
  assert_resolve_mutable_literals
    ~source:"[D()]"
    ~against:"typing.List[C]"
    "typing.List[C]";
  assert_resolve_mutable_literals
    ~source:"[Q()]"
    ~against:"typing.List[C]"
    "typing.List[Q]";

  assert_resolve_mutable_literals
    ~source:"[y for y in [D()]]"
    ~against:"typing.List[C]"
    "typing.List[C]";
  assert_resolve_mutable_literals
    ~source:"[y for y in [Q()]]"
    ~against:"typing.List[C]"
    "typing.List[Q]";

  assert_resolve_mutable_literals
    ~source:"{ 's': D() }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, C]";
  assert_resolve_mutable_literals
    ~source:"{ 's': Q() }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, Q]";

  assert_resolve_mutable_literals
    ~source:"{ 's': y for y in [D()] }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, C]";
  assert_resolve_mutable_literals
    ~source:"{ 's': y for y in [Q()] }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, Q]";

  assert_resolve_mutable_literals
    ~source:"{ D() }"
    ~against:"typing.Set[C]"
    "typing.Set[C]";
  assert_resolve_mutable_literals
    ~source:"{ Q() }"
    ~against:"typing.Set[C]"
    "typing.Set[Q]";

  assert_resolve_mutable_literals  "{ y for y in [D()] }"
    ~source:"typing.Set[C]"
    ~against:"typing.Set[C]";
  assert_resolve_mutable_literals  "{ y for y in [Q()] }"
    ~source:"typing.Set[C]"
    ~against:"typing.Set[Q]"


let test_function_definitions _ =
  let assert_functions sources function_name expected =
    let sources =
      let source (path, content) = path, trim_extra_indentation content in
      List.map sources ~f:source
    in

    let files =
      let file (path, content) = File.create ~content (mock_path path) in
      List.map sources ~f:file
    in
    let { Service.Parser.syntax_error; system_error; _ } =
      Service.Parser.parse_sources
        ~configuration:mock_configuration
        ~scheduler:(Scheduler.mock ())
        ~preprocessing_state:None
        ~files
    in
    assert_true (List.is_empty syntax_error);
    assert_true (List.is_empty system_error);

    let resolution =
      let sources =
        let source (path, content) =
          parse
            ~qualifier:(!&(String.chop_suffix_exn path ~suffix:".py"))
            ~handle:path
            content
          |> Preprocessing.qualify
        in
        List.map sources ~f:source
      in
      resolution ~sources ()
    in
    let functions =
      Resolution.function_definitions resolution (!&function_name)
      >>| List.map
        ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } -> Reference.show name)
      |> Option.value ~default:[]
    in
    assert_equal ~printer:(String.concat ~sep:", ") expected functions
  in
  assert_functions ["foo.py", "def foo(): pass\n"] "foo.foo" ["foo.foo"];
  assert_functions
    [
      "bar.py",
      {|
        @overload
        def bar(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |};
    ]
    "bar.bar"
    ["bar.bar"; "bar.bar"];
  assert_functions
    [
      "baz.py",
      {|
        def foo(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |};
    ]
    "baz.foo"
    ["baz.foo"];
  assert_functions
    []
    "undefined.undefined"
    [];
  assert_functions
    [
      "yarp.py",
      {|
        def foo():
          def nested(): pass
      |};
    ]
    "yarp.foo.nested"
    ["yarp.foo.nested"];
  assert_functions
    [
      "builtins.py",
      {|
        def len(): pass
      |};
    ]
    "len"
    ["len"];
  ()


let () =
  "resolution">:::[
    "set_local">::test_set_local;
    "parse_annotation">::test_parse_annotation;
    "parse_reference">::test_parse_reference;
    "resolve_literal">::test_resolve_literal;
    "resolve_mutable_literals">::test_resolve_mutable_literals;
    "function_definitions">::test_function_definitions;
  ]
  |> Test.run
