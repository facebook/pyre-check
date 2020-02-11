(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Ast
open Pyre
open Statement
open Test

let test_set_local context =
  let assert_local ~resolution ~name ~expected =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:(fun _ -> None))
      (Resolution.get_local resolution ~reference:!&name >>| Annotation.annotation)
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  assert_local ~resolution ~name:"local" ~expected:None;
  let resolution =
    Resolution.set_local
      resolution
      ~reference:!&"local"
      ~annotation:(Annotation.create Type.integer)
  in
  assert_local ~resolution ~name:"local" ~expected:(Some "int");
  let resolution =
    Resolution.set_local resolution ~reference:!&"local" ~annotation:(Annotation.create Type.float)
  in
  assert_local ~resolution ~name:"local" ~expected:(Some "float")


let test_set_local_with_attributes context =
  let assert_local_with_attributes ?(global_fallback = true) ~resolution ~name ~expected () =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:(fun _ -> None))
      ( Resolution.get_local_with_attributes
          ~global_fallback
          ~name:(Expression.create_name ~location:Location.any name)
          resolution
      >>| Annotation.annotation )
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  assert_local_with_attributes ~resolution ~name:"local" ~expected:None ();
  let resolution =
    Resolution.set_local_with_attributes
      resolution
      ~name:(Expression.create_name ~location:Location.any "local.a.x")
      ~annotation:(Annotation.create Type.integer)
  in
  assert_local_with_attributes ~resolution ~name:"local.a.x" ~expected:(Some "int") ();
  assert_local_with_attributes ~resolution ~name:"local.a.y" ~expected:None ();
  let resolution =
    Resolution.set_local_with_attributes
      resolution
      ~name:(Expression.create_name ~location:Location.any "local.a.x")
      ~annotation:(Annotation.create Type.float)
  in
  assert_local_with_attributes ~resolution ~name:"local.a.x" ~expected:(Some "float") ();
  ()


let test_parse_annotation context =
  let assert_parse_annotation ?(allow_untracked = false) ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:(fun _ -> None))
      ( parse_single_expression expression
      |> GlobalResolution.parse_annotation ~allow_untracked resolution )
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
  assert_resolve_literal "i" Type.Top;
  assert_resolve_literal "await i" Type.Top;
  assert_resolve_literal "await awaitable" Type.Top;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Top;
  assert_resolve_literal "j" Type.Top;
  assert_resolve_literal "foo()" Type.Top;
  assert_resolve_literal "C()" (Type.Primitive "C");
  assert_resolve_literal "G(7)" Type.Top;
  assert_resolve_literal "C" (Type.meta (Type.Primitive "C"));
  assert_resolve_literal "G" Type.Top;

  (* None *)
  assert_resolve_literal "None" Type.none;
  assert_resolve_literal "[None]" (Type.list Type.none);

  (* Dictionary *)
  assert_resolve_literal "{'a': 1}" (Type.dictionary ~key:Type.string ~value:Type.integer);
  assert_resolve_literal "{'a': i}" Type.Any;
  assert_resolve_literal "{**foo}" Type.Any;
  assert_resolve_literal "{'a': 1, **foo}" Type.Any;

  (* Boolean Operator *)
  assert_resolve_literal "1 or 2" Type.integer;
  assert_resolve_literal "True or 1" (Type.union [Type.bool; Type.integer]);
  assert_resolve_literal "True or i" Type.Any;

  (* List *)
  assert_resolve_literal "[1]" (Type.list Type.integer);
  assert_resolve_literal "[1, 'string']" (Type.list (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "[1, i]" Type.Any;

  (* Set *)
  assert_resolve_literal "{1}" (Type.set Type.integer);
  assert_resolve_literal "{1, 'string'}" (Type.set (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "{1, i}" Type.Any;

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
      GlobalResolution.resolve_exports resolution ~reference:(Reference.create name)
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
    let resolved = Resolution.resolve resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    assert_equal
      ~printer:Type.show
      (parse_annotation expected_output)
      (GlobalResolution.resolve_mutable_literals
         (Resolution.global_resolution resolution)
         ~resolve:(Resolution.resolve resolution)
         ~expression
         ~resolved
         ~expected)
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
  ()


let test_resolve_mutable_literal_to_complex_type context =
  let resolution = make_resolution ~context {|
      class C: ...
      class D(C): ...
    |} in
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
    let resolved = Resolution.resolve resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    assert_equal
      ~printer:Type.show
      (parse_annotation expected_output)
      (GlobalResolution.resolve_mutable_literals
         (Resolution.global_resolution resolution)
         ~resolve:(Resolution.resolve resolution)
         ~expression
         ~resolved
         ~expected)
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
    ~source:"{1: test.D()}"
    ~against:"typing.Union[typing.Dict[int, test.C], int, str]"
    "typing.Union[typing.Dict[int, test.C], int, str]";
  assert_resolve_mutable_literals
    ~source:"{1: test.D()}"
    ~against:"typing.Union[typing.Mapping[int, test.C], int, str]"
    "typing.Union[typing.Mapping[int, test.C], int, str]";
  ()


let test_resolve_mutable_literals_typed_dictionary context =
  let resolution = make_resolution ~context "" in
  let assert_resolve_mutable_literals ~source ~against_type expected_output_type =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    let resolved = Resolution.resolve resolution expression in
    let expression = Some expression in
    assert_equal
      ~printer:Type.show
      expected_output_type
      (GlobalResolution.resolve_mutable_literals
         (Resolution.global_resolution resolution)
         ~resolve:(Resolution.resolve resolution)
         ~expression
         ~resolved
         ~expected:against_type)
  in
  let movie_type =
    Type.TypedDictionary
      {
        name = "Movie";
        fields =
          [
            { name = "name"; annotation = Type.string }; { name = "year"; annotation = Type.integer };
          ];
        total = true;
      }
  in
  let nested_typed_dictionary_type =
    Type.TypedDictionary
      {
        name = "OuterTypedDict";
        fields = [{ name = "outer_foo"; annotation = movie_type }];
        total = true;
      }
  in
  let slightly_wrong_movie_type =
    Type.TypedDictionary.anonymous
      ~total:true
      [
        { name = "name"; annotation = Type.literal_integer 37 };
        { name = "year"; annotation = Type.integer };
      ]
  in
  let slightly_wrong_nested_type =
    Type.TypedDictionary.anonymous
      ~total:true
      [{ name = "outer_foo"; annotation = slightly_wrong_movie_type }]
  in
  assert_resolve_mutable_literals
    ~source:"{}"
    ~against_type:movie_type
    (Type.TypedDictionary.anonymous ~total:true []);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:movie_type
    movie_type;
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:movie_type
    slightly_wrong_movie_type;
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999, 'extra_key': 1 }"
    ~against_type:movie_type
    movie_type;
  assert_resolve_mutable_literals
    ~source:"{'hello': { 'name': 'The Matrix', 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:movie_type)
    (Type.dictionary ~key:Type.string ~value:movie_type);
  assert_resolve_mutable_literals
    ~source:"{'outer_foo': { 'name': 'The Matrix', 'year': 1999 }}"
    ~against_type:nested_typed_dictionary_type
    nested_typed_dictionary_type;
  assert_resolve_mutable_literals
    ~source:"{'hello': { 'name': 37, 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:movie_type)
    (Type.dictionary ~key:Type.string ~value:slightly_wrong_movie_type);
  assert_resolve_mutable_literals
    ~source:"{'outer_foo': { 'name': 37, 'year': 1999 }}"
    ~against_type:nested_typed_dictionary_type
    slightly_wrong_nested_type;
  assert_resolve_mutable_literals
    ~source:"{'outer_dict': {'outer_foo': { 'name': 37, 'year': 1999 }}}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:nested_typed_dictionary_type)
    (Type.dictionary ~key:Type.string ~value:slightly_wrong_nested_type);
  assert_resolve_mutable_literals
    ~source:"{'outer_dict': {'outer_foo': {}}}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:nested_typed_dictionary_type)
    (Type.dictionary
       ~key:Type.string
       ~value:
         (Type.TypedDictionary.anonymous
            ~total:true
            [{ name = "outer_foo"; annotation = Type.TypedDictionary.anonymous ~total:true [] }]));
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.union [movie_type; Type.integer])
    (Type.union [movie_type; Type.integer]);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.optional movie_type)
    (Type.optional movie_type);
  assert_resolve_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.optional movie_type)
    slightly_wrong_movie_type;
  ()


let test_function_definitions context =
  let assert_functions sources function_name expected =
    let project = ScratchProject.setup ~context sources in
    let resolution = ScratchProject.build_resolution project in
    let resolution = Resolution.global_resolution resolution in
    let functions =
      GlobalResolution.function_definitions resolution !&function_name
      >>| List.map ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
              Reference.show (Node.value name))
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


let test_class_definitions context =
  let assert_classes sources class_name expected =
    let project = ScratchProject.setup ~context sources in
    let resolution = ScratchProject.build_resolution project in
    let resolution = Resolution.global_resolution resolution in
    let classes =
      GlobalResolution.class_definitions resolution !&class_name
      >>| List.map ~f:(fun { Node.value = { Class.name; body; _ }; _ } ->
              Reference.show (Node.value name), List.length body)
      |> Option.value ~default:[]
    in
    let show_element (name, number_of_statements) =
      Format.sprintf "%s: %d" name number_of_statements
    in
    assert_equal
      ~printer:(fun elements -> List.map elements ~f:show_element |> String.concat ~sep:", ")
      expected
      classes
  in
  assert_classes ["foo.py", {|
    class Foo:
      pass
  |}] "foo.Foo" ["foo.Foo", 1];
  assert_classes
    ["foo.py", {|
    class Foo:
      class Bar:
        pass
  |}]
    "foo.Foo.Bar"
    ["foo.Foo.Bar", 1];
  assert_classes
    ["foo.py", {|
    if sun_is_out():
      class Foo:
        pass
  |}]
    "foo.Foo"
    ["foo.Foo", 1];
  assert_classes
    [
      ( "foo.py",
        {|
    if sun_is_out():
      class Foo:
        pass
    else:
      class Foo:
        a = 1
        b = 2
    |}
      );
    ]
    "foo.Foo"
    ["foo.Foo", 1; "foo.Foo", 2];

  (* Also handle the case where foo.bar is a class but foo.bar the module exists. *)
  assert_classes
    ["foo.py", {|
         class bar:
           a = 1
       |}; "foo/bar.py", "pass"]
    "foo.bar"
    ["foo.bar", 1]


(* We don't reverse order when returning the classes. *)

let test_source_is_unit_test context =
  let assert_is_unit_test ?(expected = true) source =
    let { ScratchProject.BuiltGlobalEnvironment.ast_environment; global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let source =
      AstEnvironment.ReadOnly.get_source
        (AstEnvironment.read_only ast_environment)
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
  |}


let () =
  "resolution"
  >::: [
         "set_local" >:: test_set_local;
         "set_local_with_attributes" >:: test_set_local_with_attributes;
         "parse_annotation" >:: test_parse_annotation;
         "parse_reference" >:: test_parse_reference;
         "partition_name" >:: test_partition_name;
         "resolve_literal" >:: test_resolve_literal;
         "resolve_exports" >:: test_resolve_exports;
         "resolve_mutable_literals" >:: test_resolve_mutable_literals;
         "resolve_mutable_literal_to_complex_type" >:: test_resolve_mutable_literal_to_complex_type;
         "resolve_mutable_literals_typed_dictionary"
         >:: test_resolve_mutable_literals_typed_dictionary;
         "function_definitions" >:: test_function_definitions;
         "class_definitions" >:: test_class_definitions;
         "source_is_unit_test" >:: test_source_is_unit_test;
       ]
  |> Test.run
