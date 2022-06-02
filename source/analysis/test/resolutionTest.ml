(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Pyre
open Statement
open Test
open WeakenMutableLiterals

let test_new_and_refine context =
  let create_name = Expression.create_name ~location:Location.any in
  let assert_local ~name ~expected resolution =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:Type.empty_aliases)
      (Resolution.get_local ~reference:!&name resolution >>| Annotation.annotation)
  in
  let assert_local_with_attributes ?(global_fallback = true) ~name ~expected resolution =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:Type.empty_aliases)
      (Resolution.get_local_with_attributes
         ~global_fallback
         ~name:(Expression.create_name ~location:Location.any name)
         resolution
      >>| Annotation.annotation)
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  (* nothing to start out with *)
  assert_local ~name:"local" ~expected:None resolution;
  (* create a local `local` and make sure the type is right *)
  let resolution =
    Resolution.new_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create_mutable Type.object_primitive)
  in
  assert_local ~name:"local" ~expected:(Some "object") resolution;
  (* create an attribute `local.x.y` and make sure the type is right, also refine it *)
  assert_local_with_attributes ~name:"local.x.y" ~expected:None resolution;
  let resolution =
    Resolution.new_local_with_attributes
      resolution
      ~name:(create_name "local.x.y")
      ~annotation:(Annotation.create_mutable Type.object_primitive)
  in
  assert_local_with_attributes ~name:"local.x.y" ~expected:(Some "object") resolution;
  (* Make sure we can refine `local.x.y` *)
  let resolution =
    Resolution.refine_local_with_attributes
      resolution
      ~name:(create_name "local.x.y")
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_local_with_attributes ~name:"local.x.y" ~expected:(Some "int") resolution;
  (* refine `local.x` and make sure it refines, and doesn't destroy `local.x.y` *)
  let resolution =
    Resolution.refine_local_with_attributes
      resolution
      ~name:(create_name "local.x")
      ~annotation:(Annotation.create_mutable Type.float)
  in
  assert_local_with_attributes ~name:"local.x" ~expected:(Some "float") resolution;
  assert_local_with_attributes ~name:"local.x.y" ~expected:(Some "int") resolution;
  (* bind a new type to `local.x`. This should destroy `local.x.y` *)
  let resolution =
    Resolution.new_local_with_attributes
      resolution
      ~name:(create_name "local.x")
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_local_with_attributes ~name:"local.x" ~expected:(Some "int") resolution;
  assert_local_with_attributes ~name:"local.x.y" ~expected:None resolution;
  (* refine `local`. This should not destroy `local.x`. *)
  let resolution =
    Resolution.refine_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create_mutable Type.float)
  in
  assert_local ~name:"local" ~expected:(Some "float") resolution;
  assert_local_with_attributes ~name:"local.x" ~expected:(Some "int") resolution;
  (* bind a new type to `local`. This should destroy `local.x`. *)
  let resolution =
    Resolution.new_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_local ~name:"local" ~expected:(Some "int") resolution;
  assert_local_with_attributes ~name:"local.x" ~expected:None resolution;
  ()


let test_parse_annotation context =
  let assert_parse_annotation ~validation ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:Type.empty_aliases)
      (GlobalResolution.parse_annotation ~validation resolution expression)
  in
  let resolution =
    let resolution =
      ScratchProject.setup
        ~context
        ["empty.pyi", "class Empty: ... "; "empty/stub.pyi", "# pyre-placeholder-stub"]
      |> ScratchProject.build_resolution
    in
    Resolution.global_resolution resolution
  in
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:"int"
    !"int";
  assert_parse_annotation
    ~validation:NoValidation
    ~resolution
    ~expected:"qualifier.int"
    !"$local_qualifier$int";
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:"typing.Any"
    !"empty.stub.Annotation";
  assert_parse_annotation
    ~validation:ValidatePrimitivesAndTypeParameters
    ~resolution
    ~expected:"typing.Dict[str, typing.Any]"
    (parse_single_expression "typing.Dict[str, empty.stub.Annotation]")


let make_resolution ~context source =
  ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution


let test_parse_reference context =
  let resolution =
    make_resolution ~context {|
      import typing
      class Foo: ...
      MyType = int
    |}
    |> Resolution.global_resolution
  in
  let assert_parse_reference reference expected =
    assert_equal
      ~printer:Type.show
      expected
      (GlobalResolution.parse_reference resolution !&reference)
  in
  assert_parse_reference "undefined" Type.Top;
  assert_parse_reference "test.MyType" Type.integer;
  assert_parse_reference "test.Foo" (Type.Primitive "test.Foo");
  assert_parse_reference "typing.List" (Type.Primitive "list")


let test_partition_name context =
  let resolution =
    make_resolution
      ~context
      {|
      from dataclasses import dataclass
      from typing import Optional, Final
      @dataclass(frozen=True)
      class InnerFrozenDataClass():
        x: Optional[int]
      @dataclass(frozen=True)
      class FrozenDataClass():
        inner: InnerFrozenDataClass
      @dataclass
      class UnfrozenDataClass():
        inner: InnerFrozenDataClass
      def foo() -> None:
        unfrozen_dataclass: Final[UnfrozenDataClass] = ...
        frozen_dataclass: Final[FrozenDataClass] = ...
        if unfrozen_dataclass.inner.x is not None:
          reveal_type(unfrozen_dataclass.inner.x)
        if frozen_dataclass.inner.x is not None:
          reveal_type(frozen_dataclass.inner.x)
    |}
  in
  let assert_resolve_name expression (object_reference, attribute_path) =
    let name = Expression.create_name ~location:Location.any expression in
    let test_reference, test_attribute_path, _ = Resolution.partition_name resolution ~name in
    assert_equal
      (test_reference, test_attribute_path)
      (Reference.create object_reference, Reference.create attribute_path)
  in
  assert_resolve_name "unfrozen_dataclass.inner.x" ("unfrozen_dataclass", "inner.x");
  assert_resolve_name "frozen_dataclass.inner.x" ("frozen_dataclass", "inner.x");
  assert_resolve_name "a.b.c" ("a", "b.c")


let test_resolve_literal context =
  let resolution =
    make_resolution
      ~context
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
    |> Resolution.global_resolution
  in
  let assert_resolve_literal source expected =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    assert_equal
      ~printer:Type.show
      expected
      (GlobalResolution.resolve_literal resolution expression)
  in
  assert_resolve_literal "i" Type.Any;
  assert_resolve_literal "await i" Type.Any;
  assert_resolve_literal "await awaitable" Type.Any;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Any;
  assert_resolve_literal "j" Type.Any;
  assert_resolve_literal "foo()" Type.Any;
  assert_resolve_literal "C()" (Type.Primitive "C");
  assert_resolve_literal "G(7)" Type.Any;
  assert_resolve_literal "C" (Type.meta (Type.Primitive "C"));
  assert_resolve_literal "G" Type.Any;

  (* None *)
  assert_resolve_literal "None" Type.Any;
  assert_resolve_literal "[None]" (Type.list Type.Any);

  (* Dictionary *)
  assert_resolve_literal "{'a': 1}" (Type.dictionary ~key:Type.string ~value:Type.integer);
  assert_resolve_literal "{'a': i}" (Type.dictionary ~key:Type.string ~value:Type.Any);
  assert_resolve_literal "{'a': [], 'b': [1]}" (Type.dictionary ~key:Type.string ~value:Type.Any);
  assert_resolve_literal "{**foo}" (Type.dictionary ~key:Type.Any ~value:Type.Any);
  assert_resolve_literal "{'a': 1, **foo}" (Type.dictionary ~key:Type.Any ~value:Type.Any);

  (* Boolean Operator *)
  assert_resolve_literal "1 or 2" Type.integer;
  assert_resolve_literal "True or 1" (Type.union [Type.bool; Type.integer]);
  assert_resolve_literal "True or i" Type.Any;

  (* List *)
  assert_resolve_literal "[1]" (Type.list Type.integer);
  assert_resolve_literal "[1, 'string']" (Type.list (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "[1, i]" (Type.list Type.Any);

  (* Set *)
  assert_resolve_literal "{1}" (Type.set Type.integer);
  assert_resolve_literal "{1, 'string'}" (Type.set (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "{1, i}" (Type.set Type.Any);

  (* Tuple *)
  assert_resolve_literal "(1,)" (Type.Tuple (Concrete [Type.integer]));
  assert_resolve_literal "(1, 'string')" (Type.Tuple (Concrete [Type.integer; Type.string]));
  assert_resolve_literal "(1, i)" (Type.Tuple (Concrete [Type.integer; Type.Any]));

  (* Ternary *)
  assert_resolve_literal "1 if x else 2" Type.integer;
  assert_resolve_literal "'hi' if x else 1" (Type.union [Type.string; Type.integer]);
  assert_resolve_literal "1 if i else i" Type.Any


let test_resolve_exports context =
  let assert_resolve ~sources name expected =
    let resolution =
      ScratchProject.setup ~context sources |> ScratchProject.build_global_resolution
    in
    let reference =
      GlobalResolution.legacy_resolve_exports resolution ~reference:(Reference.create name)
    in
    assert_equal ~printer:Reference.show ~cmp:Reference.equal (Reference.create expected) reference
  in
  assert_resolve ~sources:[] "a.b" "a.b";
  assert_resolve ~sources:["a.py", "from b import foo"; "b.py", "foo = 1"] "a.foo" "b.foo";
  assert_resolve
    ~sources:
      [
        "a.py", "from b import foo";
        "b.py", "from c import bar as foo";
        "c.py", "from d import cow as bar";
        "d.py", "cow = 1";
      ]
    "a.foo"
    "d.cow";
  assert_resolve
    ~sources:
      ["qualifier.py", "from qualifier.foo import foo"; "qualifier/foo/__init__.py", "foo = 1"]
    "qualifier.foo.foo"
    "qualifier.foo.foo";
  assert_resolve
    ~sources:
      [
        "placeholder.py", "# pyre-placeholder-stub";
        "a.py", "from placeholder.nonexistent import foo";
      ]
    "a.foo"
    "placeholder.nonexistent.foo";
  assert_resolve
    ~sources:
      [
        "qualifier/__init__.py", "from qualifier.a import bar as a";
        "qualifier/a.py", "foo = 1\nbar = 1";
      ]
    "qualifier.a.foo"
    "qualifier.a.foo"


let test_resolve_mutable_literals context =
  let resolution =
    make_resolution
      ~context
      {|
      import enum
      class C: ...
      class D(C): ...
      class Q: ...
      class MyEnum(enum.Enum):
        ONE = 1
        TWO = 2
    |}
  in
  let assert_resolve_mutable_literals ~source ~against expected_output =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> GlobalResolution.parse_annotation (Resolution.global_resolution resolution)
    in
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    let resolved = Resolution.resolve_expression_to_type resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    let actual_weakened_type =
      GlobalResolution.resolve_mutable_literals
        (Resolution.global_resolution resolution)
        ~resolve:(Resolution.resolve_expression_to_type resolution)
        ~expression
        ~resolved
        ~expected
    in
    let expected_weakened_type =
      WeakenMutableLiterals.make_weakened_type (parse_annotation expected_output)
    in
    assert_equal
      ~printer:[%show: WeakenMutableLiterals.weakened_type]
      expected_weakened_type
      actual_weakened_type
  in
  assert_resolve_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.List[test.C]"
    "typing.List[test.C]";
  assert_resolve_mutable_literals
    ~source:"[test.Q()]"
    ~against:"typing.List[test.C]"
    "typing.List[test.Q]";
  assert_resolve_mutable_literals
    ~source:"[y for y in [test.D()]]"
    ~against:"typing.List[test.C]"
    "typing.List[test.C]";
  assert_resolve_mutable_literals
    ~source:"[y for y in [test.Q()]]"
    ~against:"typing.List[test.C]"
    "typing.List[test.Q]";
  assert_resolve_mutable_literals
    ~source:"{ 's': test.D() }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.C]";
  assert_resolve_mutable_literals
    ~source:"{ 's': test.Q() }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.Q]";
  assert_resolve_mutable_literals
    ~source:"{ 's': y for y in [test.D()] }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.C]";
  assert_resolve_mutable_literals
    ~source:"{ 's': y for y in [test.Q()] }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.Q]";
  assert_resolve_mutable_literals
    ~source:"{ test.D() }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.C]";
  assert_resolve_mutable_literals
    ~source:"{ test.Q() }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.Q]";
  assert_resolve_mutable_literals
    ~source:"{ y for y in [test.D()] }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.C]";
  assert_resolve_mutable_literals
    ~source:"{ y for y in [test.Q()] }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.Q]";
  assert_resolve_mutable_literals
    ~source:"( test.D(), )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.C, ...]";
  assert_resolve_mutable_literals
    ~source:"( test.Q(), )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.Q]";
  assert_resolve_mutable_literals
    ~source:"( test.C(), test.D() )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.C, ...]";
  assert_resolve_mutable_literals
    ~source:"( test.C(), test.D() )"
    ~against:"typing.Tuple[test.D, ...]"
    "typing.Tuple[test.C, test.D]";
  assert_resolve_mutable_literals
    ~source:"( test.C(), test.Q() )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.C, test.Q]";
  assert_resolve_mutable_literals
    ~source:"{}"
    ~against:"typing.Dict[str, int]"
    "typing.Dict[str, int]";
  assert_resolve_mutable_literals
    ~source:"{test.D(): 2}"
    ~against:"typing.Dict[test.C, int]"
    "typing.Dict[test.C, int]";
  assert_resolve_mutable_literals
    ~source:"{test.D(): 3}"
    ~against:"typing.Dict[test.C, typing.Any]"
    "typing.Dict[test.C, typing.Any]";
  assert_resolve_mutable_literals
    ~source:"{'foo': []}"
    ~against:"typing.Dict[int, typing.List[int]]"
    "typing.Dict[str, typing.List[int]]";
  assert_resolve_mutable_literals
    ~source:"{'foo': {}}"
    ~against:"typing.Dict[str, typing.Dict[str, int]]"
    "typing.Dict[str, typing.Dict[str, int]]";
  assert_resolve_mutable_literals
    ~source:"1"
    ~against:"typing.Union[int, str]"
    "typing.Union[int, str]";
  assert_resolve_mutable_literals
    ~source:"{test.C: 1, test.D: 2}"
    ~against:"typing.Dict[typing.Type[test.C], int]"
    "typing.Dict[typing.Type[test.C], int]";
  assert_resolve_mutable_literals
    ~source:"{'foo': (1, 2, 3, 4)}"
    ~against:"typing.Dict[str, typing.Iterable[int]]"
    "typing.Dict[str, typing.Iterable[int]]";

  assert_resolve_mutable_literals
    ~source:"{'x': {'s': test.D()}}"
    ~against:"typing.Dict[str, typing.Dict[str, test.C]]"
    "typing.Dict[str, typing.Dict[str, test.C]]";
  assert_resolve_mutable_literals
    ~source:"{'x': {'s': test.D()}}"
    ~against:"typing.Dict[str, typing.Dict[str, test.Q]]"
    "typing.Dict[str, typing.Dict[str, test.D]]";

  assert_resolve_mutable_literals
    ~source:"[[test.D()]]"
    ~against:"typing.List[typing.List[test.C]]"
    "typing.List[typing.List[test.C]]";
  assert_resolve_mutable_literals
    ~source:"[[test.D()]]"
    ~against:"typing.List[typing.List[test.Q]]"
    "typing.List[typing.List[test.D]]";

  assert_resolve_mutable_literals
    ~source:"{{test.D()}}"
    ~against:"typing.Set[typing.Set[test.C]]"
    "typing.Set[typing.Set[test.C]]";
  assert_resolve_mutable_literals
    ~source:"{{test.D()}}"
    ~against:"typing.Set[typing.Set[test.Q]]"
    "typing.Set[typing.Set[test.D]]";

  assert_resolve_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.C]]"
    "typing.Tuple[typing.List[test.C]]";
  assert_resolve_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.Q]]"
    "typing.Tuple[typing.List[test.D]]";
  (* Tuple length mismatch *)
  assert_resolve_mutable_literals
    ~source:"([test.D()], [test.D()])"
    ~against:"typing.Tuple[typing.List[test.C]]"
    "typing.Tuple[typing.List[test.D], typing.List[test.D]]";
  assert_resolve_mutable_literals
    ~source:"([test.D()], [test.D()])"
    ~against:"typing.Tuple[typing.List[test.C], typing.List[test.Q]]"
    "typing.Tuple[typing.List[test.C], typing.List[test.D]]";
  assert_resolve_mutable_literals
    ~source:"([test.C()], [test.D()])"
    ~against:"typing.Tuple[typing.List[test.C], typing.List[test.C]]"
    "typing.Tuple[typing.List[test.C], typing.List[test.C]]";
  assert_resolve_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.C], ...]"
    "typing.Tuple[typing.List[test.C], ...]";
  assert_resolve_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.Q], ...]"
    "typing.Tuple[typing.List[test.D]]";
  assert_resolve_mutable_literals
    ~source:"([test.D()], [test.Q()])"
    ~against:"typing.Tuple[typing.List[test.C], ...]"
    "typing.Tuple[typing.List[test.D], typing.List[test.Q]]";
  assert_resolve_mutable_literals
    ~source:"([test.D()], [test.C()])"
    ~against:"typing.Tuple[typing.List[test.C], ...]"
    "typing.Tuple[typing.List[test.C], ...]";
  assert_resolve_mutable_literals
    ~source:"( test.D(), *(test.C(), test.C()))"
    ~against:"typing.Tuple[test.D, test.D, test.D]"
    "typing.Tuple[test.D, test.C, test.C]";
  assert_resolve_mutable_literals
    ~source:"( test.D(), *(test.C(), test.C()))"
    ~against:"typing.Tuple[test.D, ...]"
    "typing.Tuple[test.D, test.C, test.C]";

  assert_resolve_mutable_literals
    ~source:"{'foo': 3}"
    ~against:"typing.Dict[str, typing.Union[typing.Dict[str, int], int]]"
    "typing.Dict[str, typing.Union[typing.Dict[str, int], int]]";
  assert_resolve_mutable_literals
    ~source:"{**{1: True}, **{2: False}}"
    ~against:"typing.Dict[typing.Union[int, str], bool]"
    "typing.Dict[typing.Union[int, str], bool]";
  assert_resolve_mutable_literals
    ~source:"{**{1: True}, **{2: False}}"
    ~against:"typing.Dict[str, typing.Optional[bool]]"
    "typing.Dict[str, typing.Optional[bool]]";

  (* Handle variance for `Mapping`, etc. *)
  assert_resolve_mutable_literals
    ~source:"{test.D(): test.D()}"
    ~against:"typing.Mapping[test.C, test.C]"
    "typing.Mapping[test.C, test.C]";
  assert_resolve_mutable_literals
    ~source:"{test.D(): test.Q()}"
    ~against:"typing.Mapping[test.C, test.C]"
    "typing.Mapping[test.D, test.Q]";
  assert_resolve_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Sequence[test.C]"
    "typing.Sequence[test.C]";
  assert_resolve_mutable_literals
    ~source:"[test.Q()]"
    ~against:"typing.Sequence[test.C]"
    "typing.Sequence[test.Q]";
  assert_resolve_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Iterable[test.C]"
    "typing.Iterable[test.C]";
  assert_resolve_mutable_literals
    ~source:"[test.Q()]"
    ~against:"typing.Iterable[test.C]"
    "typing.Iterable[test.Q]";
  assert_resolve_mutable_literals
    ~source:"{test.D()}"
    ~against:"typing.AbstractSet[test.C]"
    "typing.AbstractSet[test.C]";
  assert_resolve_mutable_literals
    ~source:"{test.Q()}"
    ~against:"typing.AbstractSet[test.C]"
    "typing.AbstractSet[test.Q]";

  (* Literal literals. *)
  assert_resolve_mutable_literals
    ~source:"['a']"
    ~against:"typing.List[typing_extensions.Literal['a']]"
    "typing.List[typing_extensions.Literal['a']]";
  assert_resolve_mutable_literals
    ~source:"['a']"
    ~against:"typing.List[typing_extensions.Literal['b']]"
    "typing.List[typing_extensions.Literal['a']]";
  assert_resolve_mutable_literals
    ~source:"[1]"
    ~against:"typing.List[typing_extensions.Literal[1]]"
    "typing.List[typing_extensions.Literal[1]]";
  assert_resolve_mutable_literals
    ~source:"[1]"
    ~against:"typing.List[typing_extensions.Literal[2]]"
    "typing.List[typing_extensions.Literal[1]]";
  assert_resolve_mutable_literals
    ~source:"[True]"
    ~against:"typing.List[typing_extensions.Literal[True]]"
    "typing.List[typing_extensions.Literal[True]]";
  assert_resolve_mutable_literals
    ~source:"[True]"
    ~against:"typing.List[typing_extensions.Literal[False]]"
    "typing.List[typing_extensions.Literal[True]]";
  assert_resolve_mutable_literals
    ~source:"test.MyEnum.ONE"
    ~against:"typing_extensions.Literal[test.MyEnum.ONE]"
    "typing_extensions.Literal[test.MyEnum.ONE]";
  assert_resolve_mutable_literals
    ~source:"test.MyEnum.TWO"
    ~against:"typing_extensions.Literal[test.MyEnum.ONE]"
    "typing_extensions.Literal[test.MyEnum.TWO]";
  ()


let test_resolve_mutable_literal_to_complex_type context =
  let resolution =
    make_resolution ~context {|
      class C: ...
      class D(C): ...
      class Q: ...
    |}
  in
  let assert_resolve_mutable_literals ~source ~against expected_output =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> GlobalResolution.parse_annotation (Resolution.global_resolution resolution)
    in
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    let resolved = Resolution.resolve_expression_to_type resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    let actual_weakened_type =
      GlobalResolution.resolve_mutable_literals
        (Resolution.global_resolution resolution)
        ~resolve:(Resolution.resolve_expression_to_type resolution)
        ~expression
        ~resolved
        ~expected
    in
    let expected_weakened_type =
      WeakenMutableLiterals.make_weakened_type (parse_annotation expected_output)
    in
    assert_equal
      ~printer:[%show: WeakenMutableLiterals.weakened_type]
      expected_weakened_type
      actual_weakened_type
  in
  (* Optionals. *)
  assert_resolve_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Optional[typing.List[test.C]]"
    "typing.Optional[typing.List[test.C]]";
  assert_resolve_mutable_literals
    ~source:"[[test.D()]]"
    ~against:"typing.Optional[typing.List[typing.List[test.C]]]"
    "typing.Optional[typing.List[typing.List[test.C]]]";
  assert_resolve_mutable_literals
    ~source:{|["foo"]|}
    ~against:"typing.Optional[typing.List[typing.Union[typing.List[str], str]]]"
    "typing.Optional[typing.List[typing.Union[typing.List[str], str]]]";
  assert_resolve_mutable_literals
    ~source:"{test.D()}"
    ~against:"typing.Optional[typing.Set[test.C]]"
    "typing.Optional[typing.Set[test.C]]";
  assert_resolve_mutable_literals
    ~source:"{{test.D()}}"
    ~against:"typing.Optional[typing.Set[typing.Set[test.C]]]"
    "typing.Optional[typing.Set[typing.Set[test.C]]]";
  assert_resolve_mutable_literals
    ~source:"(test.D(),)"
    ~against:"typing.Optional[typing.Tuple[test.C, ...]]"
    "typing.Optional[typing.Tuple[test.C, ...]]";
  assert_resolve_mutable_literals
    ~source:"((test.D(),),)"
    ~against:"typing.Optional[typing.Tuple[typing.Tuple[test.C, ...], ...]]"
    "typing.Optional[typing.Tuple[typing.Tuple[test.C, ...], ...]]";
  assert_resolve_mutable_literals
    ~source:"{test.D(): 2}"
    ~against:"typing.Optional[typing.Dict[test.C, int]]"
    "typing.Optional[typing.Dict[test.C, int]]";
  assert_resolve_mutable_literals
    ~source:"{test.D(): 2}"
    ~against:"typing.Optional[typing.Mapping[test.C, int]]"
    "typing.Optional[typing.Mapping[test.C, int]]";

  (* Unions. *)
  assert_resolve_mutable_literals
    ~source:"[test.C()]"
    ~against:"typing.Union[typing.List[test.C], int, str]"
    "typing.Union[typing.List[test.C], int, str]";
  assert_resolve_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Union[typing.List[test.C], int, str]"
    "typing.Union[typing.List[test.C], int, str]";
  assert_resolve_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Union[int, str]"
    "typing.List[test.D]";
  assert_resolve_mutable_literals
    ~source:"{test.D()}"
    ~against:"typing.Union[typing.Set[test.C], int, str]"
    "typing.Union[typing.Set[test.C], int, str]";
  assert_resolve_mutable_literals
    ~source:"(test.D(),)"
    ~against:"typing.Union[typing.Tuple[test.C, ...], int, str]"
    "typing.Union[typing.Tuple[test.C, ...], int, str]";
  assert_resolve_mutable_literals
    ~source:"{1: test.D()}"
    ~against:"typing.Union[typing.Dict[int, test.C], int, str]"
    "typing.Union[typing.Dict[int, test.C], int, str]";
  assert_resolve_mutable_literals
    ~source:"{1: test.D()}"
    ~against:"typing.Union[typing.Mapping[int, test.C], int, str]"
    "typing.Union[typing.Mapping[int, test.C], int, str]";

  (* Distribute the resolved Union type over the mutable container before weakening. For example,
     List[Union[List[C], List[Q]]] to List[List[Union[C, Q]]]. *)
  assert_resolve_mutable_literals
    ~source:"[[test.C()], [test.Q()]]"
    ~against:"typing.List[typing.List[typing.Union[test.C, test.Q]]]"
    "typing.List[typing.List[typing.Union[test.C, test.Q]]]";
  assert_resolve_mutable_literals
    ~source:"[{test.C()}, {test.Q()}]"
    ~against:"typing.List[typing.Set[typing.Union[test.C, test.Q]]]"
    "typing.List[typing.Set[typing.Union[test.C, test.Q]]]";
  assert_resolve_mutable_literals
    ~source:"[{'foo': test.C()}, {'bar': test.Q()}]"
    ~against:"typing.List[typing.Dict[str, typing.Union[test.C, test.Q]]]"
    "typing.List[typing.Dict[str, typing.Union[test.C, test.Q]]]";
  assert_resolve_mutable_literals
    ~source:"[{'foo': None}, {'foo': 'bar'}]"
    ~against:"typing.List[typing.Dict[str, typing.Optional[str]]]"
    "typing.List[typing.Dict[str,typing.Optional[str]]]";
  assert_resolve_mutable_literals
    ~source:"{'hello': {'foo': None}, 'world': {'foo': 'bar'}}"
    ~against:"typing.Dict[str, typing.Dict[str, typing.Optional[str]]]"
    "typing.Dict[str,typing.Dict[str, typing.Optional[str]]]";
  (* Weakening against an explicit List[Union[List[C], List[Q]]] still works. *)
  assert_resolve_mutable_literals
    ~source:"[[test.C()], [test.Q()]]"
    ~against:"typing.List[typing.Union[typing.List[test.C], typing.List[test.Q]]]"
    "typing.List[typing.Union[typing.List[test.C], typing.List[test.Q]]]";
  ()


let test_resolve_mutable_literals_typed_dictionary context =
  let resolution =
    make_resolution
      ~context
      {|
      import mypy_extensions
      class ClassBasedMovie(mypy_extensions.TypedDict):
        name: str
        year: int

      class OuterTypedDict(mypy_extensions.TypedDict):
        outer_foo: ClassBasedMovie
        outer_bar: ClassBasedMovie

      class Child(ClassBasedMovie):
        rating: int

      class RegularClass:
        name: str
        year: int

      class NonTotalMovie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int

      class YearRequired(mypy_extensions.TypedDict):
        year: int
      class NameNotRequiredYearRequired(YearRequired, total=False):
        name: str
    |}
  in
  let resolve_expression_with_fresh_namespace resolution expression =
    Type.Variable.Namespace.reset ();
    Resolution.resolve_expression_to_type resolution expression
  in
  let assert_resolve_mutable_literals ~source ~against_type expected_weakened_type =
    let expression = parse_single_expression source in
    let resolved = resolve_expression_with_fresh_namespace resolution expression in
    let expression = Some expression in
    let location_insensitive_equal_mismatch
        { Node.value = expected_mismatch; _ }
        { Node.value = actual_mismatch; _ }
      =
      [%compare.equal: WeakenMutableLiterals.typed_dictionary_mismatch]
        expected_mismatch
        actual_mismatch
    in
    let location_insensitive_equal_weakened_type
        { WeakenMutableLiterals.resolved = expected_resolved; typed_dictionary_errors = expected }
        { WeakenMutableLiterals.resolved = actual_resolved; typed_dictionary_errors = actual }
      =
      List.equal location_insensitive_equal_mismatch expected actual
      && Type.equal expected_resolved actual_resolved
    in
    let actual_weakened_type =
      GlobalResolution.resolve_mutable_literals
        (Resolution.global_resolution resolution)
        ~resolve:(Resolution.resolve_expression_to_type resolution)
        ~expression
        ~resolved
        ~expected:against_type
    in
    assert_equal
      ~cmp:location_insensitive_equal_weakened_type
      ~printer:[%show: weakened_type]
      expected_weakened_type
      actual_weakened_type
  in
  let parse_annotation annotation =
    annotation
    |> parse_single_expression
    |> GlobalResolution.parse_annotation (Resolution.global_resolution resolution)
  in
  let create_failed_typed_dictionary_type ~resolved_type mismatches =
    make_weakened_type
      ~typed_dictionary_errors:(List.map ~f:Node.create_with_default_location mismatches)
      resolved_type
  in
  let name_type_mismatch ~resolved =
    create_failed_typed_dictionary_type
      ~resolved_type:(parse_annotation resolved)
      [
        FieldTypeMismatch
          {
            field_name = "name";
            expected_type = Type.string;
            actual_type = Type.literal_integer 37;
            class_name = "test.ClassBasedMovie";
          };
      ]
  in
  let name_and_year_type_mismatch ~resolved =
    create_failed_typed_dictionary_type
      ~resolved_type:(parse_annotation resolved)
      [
        FieldTypeMismatch
          {
            field_name = "name";
            expected_type = Type.string;
            actual_type = Type.literal_integer 37;
            class_name = "test.ClassBasedMovie";
          };
        FieldTypeMismatch
          {
            field_name = "year";
            expected_type = Type.integer;
            actual_type = Type.literal_string "NaN";
            class_name = "test.ClassBasedMovie";
          };
      ]
  in
  assert_resolve_mutable_literals
    ~source:"{}"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (create_failed_typed_dictionary_type
       ~resolved_type:
         (resolve_expression_with_fresh_namespace resolution (parse_single_expression "{}"))
       [
         MissingRequiredField { field_name = "name"; class_name = "test.ClassBasedMovie" };
         MissingRequiredField { field_name = "year"; class_name = "test.ClassBasedMovie" };
       ]);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (make_weakened_type (Type.Primitive "test.ClassBasedMovie"));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999, 'rating': 10 }"
    ~against_type:(Type.Primitive "test.Child")
    (make_weakened_type (Type.Primitive "test.Child"));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.RegularClass")
    (make_weakened_type
       (Type.dictionary ~key:Type.string ~value:(Type.union [Type.integer; Type.string])));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.Child")
    (create_failed_typed_dictionary_type
       ~resolved_type:(parse_annotation "typing.Dict[str, typing.Union[int, str]]")
       [MissingRequiredField { field_name = "rating"; class_name = "test.Child" }]);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (name_type_mismatch ~resolved:"typing.Dict[str, int]");
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999, 'extra_key': 1 }"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (make_weakened_type
       ~typed_dictionary_errors:
         [
           UndefinedField { field_name = "extra_key"; class_name = "test.ClassBasedMovie" }
           |> Node.create_with_default_location;
         ]
       (Type.Primitive "test.ClassBasedMovie"));
  assert_resolve_mutable_literals
    ~source:"{'hello': { 'name': 'The Matrix', 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie"))
    (make_weakened_type
       (Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie")));
  assert_resolve_mutable_literals
    ~source:
      "{'outer_foo': { 'name': 'The Matrix', 'year': 1999 }, 'outer_bar': { 'name': 'The Matrix', \
       'year': 1999 }}"
    ~against_type:(Type.Primitive "test.OuterTypedDict")
    (make_weakened_type (Type.Primitive "test.OuterTypedDict"));
  assert_resolve_mutable_literals
    ~source:
      "{'hello': { 'name': 'The Matrix', 'year': 1999 }, 'world': { 'name': 37, 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie"))
    (name_type_mismatch
       ~resolved:
         "typing.Dict[str, typing.Union[typing.Dict[str, int], typing.Dict[str, typing.Union[int, \
          str]], test.ClassBasedMovie]]");
  assert_resolve_mutable_literals
    ~source:"{'hello': { 'name': 37, 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie"))
    (name_type_mismatch ~resolved:"typing.Dict[str, typing.Dict[str, int]]");
  assert_resolve_mutable_literals
    ~source:
      "{'outer_foo': { 'name': 37, 'year': 1999 }, 'outer_bar': { 'name': 'The Matrix', 'year': \
       'NaN' }}"
    ~against_type:(Type.Primitive "test.OuterTypedDict")
    (name_and_year_type_mismatch
       ~resolved:"typing.Dict[str, typing.Union[typing.Dict[str, int], typing.Dict[str, str]]]");
  assert_resolve_mutable_literals
    ~source:"{'outer_dict': {'outer_foo': { 'name': 37, 'year': 1999 }}}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.OuterTypedDict"))
    (name_type_mismatch ~resolved:"typing.Dict[str, typing.Dict[str, typing.Dict[str, int]]]");
  let source = "{'outer_dict': {'outer_foo': {}}}" in
  assert_resolve_mutable_literals
    ~source
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.OuterTypedDict"))
    (create_failed_typed_dictionary_type
       ~resolved_type:
         (resolve_expression_with_fresh_namespace resolution (parse_single_expression source))
       [
         MissingRequiredField { field_name = "name"; class_name = "test.ClassBasedMovie" };
         MissingRequiredField { field_name = "year"; class_name = "test.ClassBasedMovie" };
       ]);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer])
    (make_weakened_type (Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer]));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.optional (Type.Primitive "test.ClassBasedMovie"))
    (make_weakened_type (Type.optional (Type.Primitive "test.ClassBasedMovie")));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.optional (Type.Primitive "test.ClassBasedMovie"))
    (name_type_mismatch ~resolved:"typing.Dict[str, int]");
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer])
    (make_weakened_type (Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer]));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer])
    (name_type_mismatch ~resolved:"typing.Dict[str, int]");

  (* Weaken to get the dictionary in a Union instead of giving an error for the TypedDict. *)
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:
      (Type.union
         [
           Type.Primitive "test.ClassBasedMovie";
           Type.dictionary ~key:Type.string ~value:Type.integer;
         ])
    (make_weakened_type
       (Type.union
          [
            Type.Primitive "test.ClassBasedMovie";
            Type.dictionary ~key:Type.string ~value:Type.integer;
          ]));

  assert_resolve_mutable_literals
    ~source:"[{ 'name': 'The Matrix', 'year': 1999 }]"
    ~against_type:(Type.list (Type.Primitive "test.ClassBasedMovie"))
    (make_weakened_type (Type.list (Type.Primitive "test.ClassBasedMovie")));
  assert_resolve_mutable_literals
    ~source:"[{ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' }]"
    ~against_type:(Type.list (Type.Primitive "test.ClassBasedMovie"))
    (name_and_year_type_mismatch
       ~resolved:"typing.List[typing.Union[typing.Dict[str, int], typing.Dict[str, str]]]");
  assert_resolve_mutable_literals
    ~source:"({ 'name': 'The Matrix', 'year': 1999 },)"
    ~against_type:(Type.tuple [Type.Primitive "test.ClassBasedMovie"])
    (make_weakened_type (Type.tuple [Type.Primitive "test.ClassBasedMovie"]));
  assert_resolve_mutable_literals
    ~source:"({ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' })"
    ~against_type:
      (Type.tuple [Type.Primitive "test.ClassBasedMovie"; Type.Primitive "test.ClassBasedMovie"])
    (name_and_year_type_mismatch
       ~resolved:"typing.Tuple[typing.Dict[str, int], typing.Dict[str, str]];");
  assert_resolve_mutable_literals
    ~source:"({ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' })"
    ~against_type:
      (Type.Tuple
         (Type.OrderedTypes.create_unbounded_concatenation (Type.Primitive "test.ClassBasedMovie")))
    (name_and_year_type_mismatch
       ~resolved:"typing.Tuple[typing.Dict[str, int], typing.Dict[str, str]];");
  assert_resolve_mutable_literals
    ~source:"[{ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' }]"
    ~against_type:(Type.sequence (Type.Primitive "test.ClassBasedMovie"))
    (name_and_year_type_mismatch
       ~resolved:"typing.Sequence[typing.Union[typing.Dict[str, int], typing.Dict[str, str]]];");

  (* Non-total typed dictionary. *)
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (make_weakened_type (Type.Primitive "test.NonTotalMovie"));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix'}"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (make_weakened_type (Type.Primitive "test.NonTotalMovie"));
  assert_resolve_mutable_literals
    ~source:"{}"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (make_weakened_type (Type.Primitive "test.NonTotalMovie"));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (make_weakened_type (Type.Primitive "test.NameNotRequiredYearRequired"));
  assert_resolve_mutable_literals
    ~source:"{'year': 1999 }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (make_weakened_type (Type.Primitive "test.NameNotRequiredYearRequired"));
  (* Note that we don't add [year] to the resolved type for a mismatch because [year] is a required
     field. *)
  assert_resolve_mutable_literals
    ~source:"{'name': 'The Matrix' }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (create_failed_typed_dictionary_type
       ~resolved_type:(parse_annotation "typing.Dict[str, str]")
       [
         MissingRequiredField
           { field_name = "year"; class_name = "test.NameNotRequiredYearRequired" };
       ]);
  assert_resolve_mutable_literals
    ~source:"{}"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (create_failed_typed_dictionary_type
       ~resolved_type:
         (resolve_expression_with_fresh_namespace resolution (parse_single_expression "{}"))
       [
         MissingRequiredField
           { field_name = "year"; class_name = "test.NameNotRequiredYearRequired" };
       ]);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37}"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (create_failed_typed_dictionary_type
       ~resolved_type:(parse_annotation "typing.Dict[str, int]")
       [
         FieldTypeMismatch
           {
             field_name = "name";
             expected_type = Type.string;
             actual_type = Type.literal_integer 37;
             class_name = "test.NonTotalMovie";
           };
       ]);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (create_failed_typed_dictionary_type
       ~resolved_type:(parse_annotation "typing.Dict[str, int]")
       [
         FieldTypeMismatch
           {
             field_name = "name";
             expected_type = Type.string;
             actual_type = Type.literal_integer 37;
             class_name = "test.NameNotRequiredYearRequired";
           };
       ]);
  ()


let test_distribute_union_over_parametric _ =
  let assert_distributed actual expected =
    assert_equal ~cmp:[%equal: Type.t option] ~printer:[%show: Type.t option] expected actual
  in
  assert_distributed
    (distribute_union_over_parametric ~parametric_name:"list" ~number_of_parameters:1 Type.integer)
    None;
  assert_distributed
    (distribute_union_over_parametric
       ~parametric_name:"list"
       ~number_of_parameters:1
       (Type.union [Type.list Type.integer; Type.integer]))
    None;
  assert_distributed
    (distribute_union_over_parametric
       ~parametric_name:"list"
       ~number_of_parameters:2
       (Type.union [Type.list Type.integer; Type.list Type.string]))
    None;
  assert_distributed
    (distribute_union_over_parametric
       ~parametric_name:"list"
       ~number_of_parameters:1
       (Type.union [Type.list Type.integer; Type.list Type.string]))
    (Some (Type.list (Type.union [Type.integer; Type.string])));
  assert_distributed
    (distribute_union_over_parametric
       ~parametric_name:"dict"
       ~number_of_parameters:2
       (Type.union
          [
            Type.dictionary ~key:Type.integer ~value:Type.string;
            Type.dictionary ~key:Type.string ~value:Type.integer;
          ]))
    (Some
       (Type.dictionary
          ~key:(Type.union [Type.integer; Type.string])
          ~value:(Type.union [Type.integer; Type.string])));
  ()


let test_resolve_mutable_literal_to_recursive_type context =
  let resolution =
    make_resolution
      ~context
      {|
      from typing import Dict, List, Set, Union
      Tree = Union[int, List["Tree"]]
      TreeSet = Union[int, Set["TreeSet"]]
      JSON = Union[int, str, List["JSON"], Dict[str, "JSON"]]
    |}
  in
  let assert_resolve_mutable_literals ~source ~against expected_output =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> GlobalResolution.parse_annotation (Resolution.global_resolution resolution)
    in
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    let resolved = Resolution.resolve_expression_to_type resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    let actual_weakened_type =
      GlobalResolution.resolve_mutable_literals
        (Resolution.global_resolution resolution)
        ~resolve:(Resolution.resolve_expression_to_type resolution)
        ~expression
        ~resolved
        ~expected
    in
    let expected_weakened_type =
      WeakenMutableLiterals.make_weakened_type (parse_annotation expected_output)
    in
    assert_equal
      ~printer:[%show: WeakenMutableLiterals.weakened_type]
      expected_weakened_type
      actual_weakened_type
  in
  assert_resolve_mutable_literals ~source:"1" ~against:"test.Tree" "test.Tree";
  assert_resolve_mutable_literals ~source:"[]" ~against:"test.Tree" "test.Tree";
  assert_resolve_mutable_literals ~source:"[1]" ~against:"test.Tree" "test.Tree";
  assert_resolve_mutable_literals ~source:"[1, [2]]" ~against:"test.Tree" "test.Tree";
  assert_resolve_mutable_literals ~source:"[1, *[2]]" ~against:"test.Tree" "test.Tree";
  assert_resolve_mutable_literals ~source:"[1, [2, [3, 4]]]" ~against:"test.Tree" "test.Tree";
  assert_resolve_mutable_literals
    ~source:{| [1, [2, "hello"]] |}
    ~against:"test.Tree"
    "typing.List[typing.Union[typing.List[typing.Union[int, str]], int]]";

  assert_resolve_mutable_literals ~source:"{1, {2, {3, 4}}}" ~against:"test.TreeSet" "test.TreeSet";
  assert_resolve_mutable_literals
    ~source:"{1, True}"
    ~against:"test.TreeSet"
    "typing.Set[typing.Union[int, bool]]";

  assert_resolve_mutable_literals ~source:"{}" ~against:"test.JSON" "test.JSON";
  assert_resolve_mutable_literals ~source:"[1]" ~against:"test.JSON" "test.JSON";
  assert_resolve_mutable_literals
    ~source:{| {"a": 1, **{"b": {"c": 3}}} |}
    ~against:"test.JSON"
    "test.JSON";
  assert_resolve_mutable_literals
    ~source:{| {"a": 1, "b": [2, "hello"], "c": {"d": {}}} |}
    ~against:"test.JSON"
    "test.JSON";
  assert_resolve_mutable_literals
    ~source:{| {"a": {"b": True}} |}
    ~against:"test.JSON"
    "typing.Dict[str, typing.Dict[str, bool]]";
  ()


let test_get_typed_dictionary context =
  let resolution =
    make_resolution
      ~context
      {|
      import mypy_extensions
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int

      class NonTotalMovie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int

      class Child(Movie):
        rating: int

      class RegularClass:
        def __init__(self, x: int) -> None:
          self.x = x
    |}
  in
  let assert_typed_dictionary ~annotation expected_type =
    assert_equal
      ~printer:[%show: Type.t Type.Record.TypedDictionary.record option]
      expected_type
      (GlobalResolution.get_typed_dictionary
         ~resolution:(Resolution.global_resolution resolution)
         annotation)
  in
  assert_typed_dictionary ~annotation:(Type.Primitive "test.RegularClass") None;
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.Movie")
    (Some
       {
         name = "test.Movie";
         fields =
           [
             { Type.Record.TypedDictionary.name = "name"; annotation = Type.string; required = true };
             {
               Type.Record.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = true;
             };
           ];
       });
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.Child")
    (Some
       {
         name = "test.Child";
         fields =
           [
             { Type.Record.TypedDictionary.name = "name"; annotation = Type.string; required = true };
             {
               Type.Record.TypedDictionary.name = "rating";
               annotation = Type.integer;
               required = true;
             };
             {
               Type.Record.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = true;
             };
           ];
       });
  assert_typed_dictionary
    ~annotation:(Type.Primitive "test.NonTotalMovie")
    (Some
       {
         name = "test.NonTotalMovie";
         fields =
           [
             {
               Type.Record.TypedDictionary.name = "name";
               annotation = Type.string;
               required = false;
             };
             {
               Type.Record.TypedDictionary.name = "year";
               annotation = Type.integer;
               required = false;
             };
           ];
       });
  ()


let test_function_definitions context =
  let assert_functions sources function_name expected =
    let project = ScratchProject.setup ~context sources in
    let resolution = ScratchProject.build_resolution project in
    let resolution = Resolution.global_resolution resolution in
    let functions =
      GlobalResolution.function_definitions resolution !&function_name
      >>| List.map ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
              Reference.show name)
      |> Option.value ~default:[]
    in
    assert_equal ~printer:(String.concat ~sep:", ") expected functions
  in
  assert_functions ["foo.py", "def foo(): pass\n"] "foo.foo" ["foo.foo"];
  assert_functions
    [
      ( "bar.py",
        {|
        @overload
        def bar(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |}
      );
    ]
    "bar.bar"
    ["bar.bar"; "bar.bar"];
  assert_functions
    ["baz.py", {|
        def foo(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |}]
    "baz.foo"
    ["baz.foo"];
  assert_functions [] "undefined.undefined" [];
  assert_functions
    ["yarp.py", {|
        def foo():
          def nested(): pass
      |}]
    "yarp.foo.nested"
    [];
  ()


(* We don't reverse order when returning the classes. *)

let test_source_is_unit_test context =
  let assert_is_unit_test ?(expected = true) ?(extra_sources = []) source =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context (["test.py", source] @ extra_sources)
      |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let source =
      AstEnvironment.ReadOnly.get_processed_source
        (AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment)
        (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    assert_equal expected (GlobalResolution.source_is_unit_test resolution ~source)
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
  assert_not_unit_test
    ~extra_sources:["placeholder.py", "# pyre-placeholder-stub"]
    {|
    import placeholder
    class C(placeholder.Missing):
      ...
  |}


let test_fallback_attribute context =
  let assert_fallback_attribute ?(instantiated = None) ~name source annotation =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let global_resolution = GlobalResolution.create global_environment in
    let resolution = TypeCheck.resolution global_resolution (module TypeCheck.DummyContext) in

    let attribute =
      let qualifier = Reference.create "test" in
      let source =
        AstEnvironment.ReadOnly.get_processed_source
          (AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment)
          qualifier
      in
      let last_statement_exn = function
        | { Source.statements; _ } when List.length statements > 0 -> List.last_exn statements
        | _ -> failwith "Could not parse last statement"
      in

      let source = Option.value_exn source in
      last_statement_exn source
      |> Node.value
      |> (function
           | Statement.Class definition -> ClassSummary.create ~qualifier definition
           | _ -> failwith "Last statement was not a class")
      |> ClassSummary.name
      |> Reference.show
      |> Resolution.fallback_attribute ~instantiated ~resolution ~name
    in
    let printer optional_type = optional_type >>| Type.show |> Option.value ~default:"None" in
    assert_equal
      ~cmp:(Option.equal Type.equal)
      ~printer
      annotation
      (attribute >>| Annotated.Attribute.annotation >>| Annotation.annotation)
  in
  assert_fallback_attribute ~name:"attribute" {|
      class Foo:
        pass
    |} None;
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int:
          return 1
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int: ...
      class Bar(Foo):
        pass
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"__iadd__"
    {|
      class Foo:
        def __add__(self, other: Foo) -> int:
          pass
    |}
    (Some
       (Parametric
          {
            name = "BoundMethod";
            parameters =
              [
                Single
                  (parse_callable
                     "typing.Callable('test.Foo.__add__')[[Named(self, test.Foo), Named(other, \
                      test.Foo)], int]");
                Single (Primitive "test.Foo");
              ];
          }));
  assert_fallback_attribute
    ~name:"__iadd__"
    ~instantiated:(Some (Type.parametric "test.Foo" [Single Type.integer]))
    {|
      from typing import Generic, TypeVar
      T = TypeVar("T")
      class Foo(Generic[T]):
        def __add__(self, other: Foo[T]) -> Foo[T]:
          pass
    |}
    (Some
       (Parametric
          {
            name = "BoundMethod";
            parameters =
              [
                Single
                  (parse_callable
                     "typing.Callable(test.Foo.__add__)[[Named(self, test.Foo[int]), Named(other, \
                      test.Foo[int])], test.Foo[int]]");
                Single (Type.parametric "test.Foo" [Single Type.integer]);
              ];
          }));
  assert_fallback_attribute ~name:"__iadd__" {|
      class Foo:
        pass
    |} None;
  assert_fallback_attribute
    ~name:"__iadd__"
    {|
      class Foo:
        def __getattr__(self, attribute) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"foo"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def __getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"bar"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def __getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.string);
  assert_fallback_attribute
    ~name:"baz"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def __getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def __getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.none);
  assert_fallback_attribute
    ~name:"baz"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def __getattr__(self: Foo, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  (* Callables on the instance do not get picked up by the runtime. Who knew? *)
  assert_fallback_attribute
    ~name:"baz"
    {|
      class Foo:
        __getattr__: typing.Callable[[str], int]
    |}
    None;
  assert_fallback_attribute
    ~name:"baz"
    {|
      class Foo:
        __getattr__: BoundMethod[typing.Callable[[Foo, str], int], Foo]
    |}
    (Some Type.integer);
  ()


let () =
  "resolution"
  >::: [
         "new_and_refine" >:: test_new_and_refine;
         "parse_annotation" >:: test_parse_annotation;
         "parse_reference" >:: test_parse_reference;
         "partition_name" >:: test_partition_name;
         "resolve_literal" >:: test_resolve_literal;
         "resolve_exports" >:: test_resolve_exports;
         "resolve_mutable_literals" >:: test_resolve_mutable_literals;
         "resolve_mutable_literal_to_complex_type" >:: test_resolve_mutable_literal_to_complex_type;
         "resolve_mutable_literals_typed_dictionary"
         >:: test_resolve_mutable_literals_typed_dictionary;
         "resolve_mutable_literal_to_recursive_type"
         >:: test_resolve_mutable_literal_to_recursive_type;
         "distribute_union_over_parametric" >:: test_distribute_union_over_parametric;
         "get_typed_dictionary " >:: test_get_typed_dictionary;
         "function_definitions" >:: test_function_definitions;
         "source_is_unit_test" >:: test_source_is_unit_test;
         "fallback_attribute" >:: test_fallback_attribute;
       ]
  |> Test.run
