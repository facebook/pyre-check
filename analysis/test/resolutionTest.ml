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
      (Resolution.get_local resolution ~access:(Access.create access) >>| Annotation.annotation)
  in

  let resolution = Test.resolution ~sources:[] () in
  assert_local ~resolution ~access:"local" ~expected:None;

  let resolution =
    Resolution.set_local
      resolution
      ~access:(Access.create "local")
      ~annotation:(Annotation.create Type.integer)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "int");

  let resolution =
    Resolution.set_local
      resolution
      ~access:(Access.create "local")
      ~annotation:(Annotation.create Type.float)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "float")


let test_parse_annotation _ =
  let assert_parse_annotation ~resolution ~expression ~expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:(fun _ -> None))
      (parse_single_expression expression |> Resolution.parse_annotation resolution)
  in

  let resolution =
    Test.resolution
      ~sources:[
        parse ~qualifier:(Access.create "empty") ~handle:"empty.pyi" "class Empty: ...";
        parse
          ~qualifier:(Access.create "empty.stub")
          ~local_mode:Source.PlaceholderStub
          ~handle:"empty/stub.pyi"
          "";
      ]
      ()
  in
  assert_parse_annotation ~resolution ~expression:"int" ~expected:"int";
  assert_parse_annotation ~resolution ~expression:"$local_qualifier$int" ~expected:"qualifier.int";
  assert_parse_annotation ~resolution ~expression:"empty.stub.Annotation" ~expected:"typing.Any";
  assert_parse_annotation
    ~resolution
    ~expression:"typing.Dict[str, empty.stub.Annotation]"
    ~expected:"typing.Dict[str, typing.Any]"


let make_resolution source =
  let configuration = Configuration.Analysis.create () in
  let populate source =
    let environment =
      let environment = Environment.Builder.create () in
      Service.Environment.populate
        ~configuration
        (Environment.handler ~configuration environment)
        (parse source :: typeshed_stubs ());
      environment
    in
    Environment.handler ~configuration environment
  in
  populate source
  |> fun environment -> TypeCheck.resolution environment ()


let test_resolve_literal _ =
  let resolution =
    make_resolution
      {|
      class C:
        def __init__(self) -> None:
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
    assert_equal ~printer:Type.show (Resolution.resolve_literal resolution expression) expected
  in
  assert_resolve_literal "i" Type.Top;
  assert_resolve_literal "await i" Type.Top;
  assert_resolve_literal "await awaitable" Type.Top;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Top;
  assert_resolve_literal "j" Type.Top;
  assert_resolve_literal "foo()" Type.Top;
  assert_resolve_literal "C()" (Type.Primitive "C");
  assert_resolve_literal "C" (Type.meta (Type.Primitive "C"));
  assert_resolve_literal "none" Type.none


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


let test_can_be_bound _ =
  let resolution =
    make_resolution
      {|
      class C: ...
      class D(C): ...
      class Q: ...
    |}
  in
  let assert_resolve_mutable_literals variable target expected =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> Resolution.parse_annotation resolution
    in
    let variable = parse_annotation variable in
    let target = parse_annotation target in
    assert_equal
      expected
      (Resolution.can_be_bound resolution ~variable ~target)
  in
  assert_resolve_mutable_literals "typing.TypeVar('_T')" "C" true;
  assert_resolve_mutable_literals "typing.TypeVar('_T')" "D" true;
  assert_resolve_mutable_literals "typing.TypeVar('_T')" "Q" true;

  assert_resolve_mutable_literals "typing.TypeVar('_T', $parameter$bound='C')" "C" true;
  assert_resolve_mutable_literals "typing.TypeVar('_T', $parameter$bound='C')" "D" true;
  assert_resolve_mutable_literals "typing.TypeVar('_T', $parameter$bound='C')" "Q" false;
  assert_resolve_mutable_literals "typing.TypeVar('_T', $parameter$bound='D')" "C" false;

  assert_resolve_mutable_literals "typing.TypeVar('_T', C, Q)" "C" true;
  assert_resolve_mutable_literals "typing.TypeVar('_T', C, Q)" "D" true;
  assert_resolve_mutable_literals "typing.TypeVar('_T', D, Q)" "C" false;
  ()


let () =
  "resolution">:::[
    "set_local">::test_set_local;
    "parse_annotation">::test_parse_annotation;
    "resolve_literal">::test_resolve_literal;
    "resolve_mutable_literals">::test_resolve_mutable_literals;
    "can_be_bound">::test_can_be_bound;
  ]
  |> Test.run
