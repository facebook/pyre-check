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
open Statement
open Test
open WeakenMutableLiterals

let make_resolution ~context source =
  ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution


let test_weaken_mutable_literals context =
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
  let assert_weaken_mutable_literals ~source ~against expected_output =
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
  assert_weaken_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.List[test.C]"
    "typing.List[test.C]";
  assert_weaken_mutable_literals
    ~source:"[test.Q()]"
    ~against:"typing.List[test.C]"
    "typing.List[test.Q]";
  assert_weaken_mutable_literals
    ~source:"[y for y in [test.D()]]"
    ~against:"typing.List[test.C]"
    "typing.List[test.C]";
  assert_weaken_mutable_literals
    ~source:"[y for y in [test.Q()]]"
    ~against:"typing.List[test.C]"
    "typing.List[test.Q]";
  assert_weaken_mutable_literals
    ~source:"{ 's': test.D() }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.C]";
  assert_weaken_mutable_literals
    ~source:"{ 's': test.Q() }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.Q]";
  assert_weaken_mutable_literals
    ~source:"{ 's': y for y in [test.D()] }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.C]";
  assert_weaken_mutable_literals
    ~source:"{ 's': y for y in [test.Q()] }"
    ~against:"typing.Dict[str, test.C]"
    "typing.Dict[str, test.Q]";
  assert_weaken_mutable_literals
    ~source:"{ test.D() }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.C]";
  assert_weaken_mutable_literals
    ~source:"{ test.Q() }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.Q]";
  assert_weaken_mutable_literals
    ~source:"{ y for y in [test.D()] }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.C]";
  assert_weaken_mutable_literals
    ~source:"{ y for y in [test.Q()] }"
    ~against:"typing.Set[test.C]"
    "typing.Set[test.Q]";
  assert_weaken_mutable_literals
    ~source:"( test.D(), )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.C, ...]";
  assert_weaken_mutable_literals
    ~source:"( test.Q(), )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.Q]";
  assert_weaken_mutable_literals
    ~source:"( test.C(), test.D() )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.C, ...]";
  assert_weaken_mutable_literals
    ~source:"( test.C(), test.D() )"
    ~against:"typing.Tuple[test.D, ...]"
    "typing.Tuple[test.C, test.D]";
  assert_weaken_mutable_literals
    ~source:"( test.C(), test.Q() )"
    ~against:"typing.Tuple[test.C, ...]"
    "typing.Tuple[test.C, test.Q]";
  assert_weaken_mutable_literals
    ~source:"{}"
    ~against:"typing.Dict[str, int]"
    "typing.Dict[str, int]";
  assert_weaken_mutable_literals
    ~source:"{test.D(): 2}"
    ~against:"typing.Dict[test.C, int]"
    "typing.Dict[test.C, int]";
  assert_weaken_mutable_literals
    ~source:"{test.D(): 3}"
    ~against:"typing.Dict[test.C, typing.Any]"
    "typing.Dict[test.C, typing.Any]";
  assert_weaken_mutable_literals
    ~source:"{'foo': []}"
    ~against:"typing.Dict[int, typing.List[int]]"
    "typing.Dict[str, typing.List[int]]";
  assert_weaken_mutable_literals
    ~source:"{'foo': {}}"
    ~against:"typing.Dict[str, typing.Dict[str, int]]"
    "typing.Dict[str, typing.Dict[str, int]]";
  assert_weaken_mutable_literals
    ~source:"1"
    ~against:"typing.Union[int, str]"
    "typing.Union[int, str]";
  assert_weaken_mutable_literals
    ~source:"{test.C: 1, test.D: 2}"
    ~against:"typing.Dict[typing.Type[test.C], int]"
    "typing.Dict[typing.Type[test.C], int]";
  assert_weaken_mutable_literals
    ~source:"{'foo': (1, 2, 3, 4)}"
    ~against:"typing.Dict[str, typing.Iterable[int]]"
    "typing.Dict[str, typing.Iterable[int]]";

  assert_weaken_mutable_literals
    ~source:"{'x': {'s': test.D()}}"
    ~against:"typing.Dict[str, typing.Dict[str, test.C]]"
    "typing.Dict[str, typing.Dict[str, test.C]]";
  assert_weaken_mutable_literals
    ~source:"{'x': {'s': test.D()}}"
    ~against:"typing.Dict[str, typing.Dict[str, test.Q]]"
    "typing.Dict[str, typing.Dict[str, test.D]]";

  assert_weaken_mutable_literals
    ~source:"[[test.D()]]"
    ~against:"typing.List[typing.List[test.C]]"
    "typing.List[typing.List[test.C]]";
  assert_weaken_mutable_literals
    ~source:"[[test.D()]]"
    ~against:"typing.List[typing.List[test.Q]]"
    "typing.List[typing.List[test.D]]";

  assert_weaken_mutable_literals
    ~source:"{{test.D()}}"
    ~against:"typing.Set[typing.Set[test.C]]"
    "typing.Set[typing.Set[test.C]]";
  assert_weaken_mutable_literals
    ~source:"{{test.D()}}"
    ~against:"typing.Set[typing.Set[test.Q]]"
    "typing.Set[typing.Set[test.D]]";

  assert_weaken_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.C]]"
    "typing.Tuple[typing.List[test.C]]";
  assert_weaken_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.Q]]"
    "typing.Tuple[typing.List[test.D]]";
  (* Tuple length mismatch *)
  assert_weaken_mutable_literals
    ~source:"([test.D()], [test.D()])"
    ~against:"typing.Tuple[typing.List[test.C]]"
    "typing.Tuple[typing.List[test.D], typing.List[test.D]]";
  assert_weaken_mutable_literals
    ~source:"([test.D()], [test.D()])"
    ~against:"typing.Tuple[typing.List[test.C], typing.List[test.Q]]"
    "typing.Tuple[typing.List[test.C], typing.List[test.D]]";
  assert_weaken_mutable_literals
    ~source:"([test.C()], [test.D()])"
    ~against:"typing.Tuple[typing.List[test.C], typing.List[test.C]]"
    "typing.Tuple[typing.List[test.C], typing.List[test.C]]";
  assert_weaken_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.C], ...]"
    "typing.Tuple[typing.List[test.C], ...]";
  assert_weaken_mutable_literals
    ~source:"([test.D()],)"
    ~against:"typing.Tuple[typing.List[test.Q], ...]"
    "typing.Tuple[typing.List[test.D]]";
  assert_weaken_mutable_literals
    ~source:"([test.D()], [test.Q()])"
    ~against:"typing.Tuple[typing.List[test.C], ...]"
    "typing.Tuple[typing.List[test.D], typing.List[test.Q]]";
  assert_weaken_mutable_literals
    ~source:"([test.D()], [test.C()])"
    ~against:"typing.Tuple[typing.List[test.C], ...]"
    "typing.Tuple[typing.List[test.C], ...]";
  assert_weaken_mutable_literals
    ~source:"( test.D(), *(test.C(), test.C()))"
    ~against:"typing.Tuple[test.D, test.D, test.D]"
    "typing.Tuple[test.D, test.C, test.C]";
  assert_weaken_mutable_literals
    ~source:"( test.D(), *(test.C(), test.C()))"
    ~against:"typing.Tuple[test.D, ...]"
    "typing.Tuple[test.D, test.C, test.C]";

  assert_weaken_mutable_literals
    ~source:"{'foo': 3}"
    ~against:"typing.Dict[str, typing.Union[typing.Dict[str, int], int]]"
    "typing.Dict[str, typing.Union[typing.Dict[str, int], int]]";
  assert_weaken_mutable_literals
    ~source:"{**{1: True}, **{2: False}}"
    ~against:"typing.Dict[typing.Union[int, str], bool]"
    "typing.Dict[typing.Union[int, str], bool]";
  assert_weaken_mutable_literals
    ~source:"{**{1: True}, **{2: False}}"
    ~against:"typing.Dict[str, typing.Optional[bool]]"
    "typing.Dict[str, typing.Optional[bool]]";

  (* Handle variance for `Mapping`, etc. *)
  assert_weaken_mutable_literals
    ~source:"{test.D(): test.D()}"
    ~against:"typing.Mapping[test.C, test.C]"
    "typing.Mapping[test.C, test.C]";
  assert_weaken_mutable_literals
    ~source:"{test.D(): test.Q()}"
    ~against:"typing.Mapping[test.C, test.C]"
    "typing.Mapping[test.D, test.Q]";
  assert_weaken_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Sequence[test.C]"
    "typing.Sequence[test.C]";
  assert_weaken_mutable_literals
    ~source:"[test.Q()]"
    ~against:"typing.Sequence[test.C]"
    "typing.Sequence[test.Q]";
  assert_weaken_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Iterable[test.C]"
    "typing.Iterable[test.C]";
  assert_weaken_mutable_literals
    ~source:"[test.Q()]"
    ~against:"typing.Iterable[test.C]"
    "typing.Iterable[test.Q]";
  assert_weaken_mutable_literals
    ~source:"{test.D()}"
    ~against:"typing.AbstractSet[test.C]"
    "typing.AbstractSet[test.C]";
  assert_weaken_mutable_literals
    ~source:"{test.Q()}"
    ~against:"typing.AbstractSet[test.C]"
    "typing.AbstractSet[test.Q]";

  (* Literal literals. *)
  assert_weaken_mutable_literals
    ~source:"['a']"
    ~against:"typing.List[typing_extensions.Literal['a']]"
    "typing.List[typing_extensions.Literal['a']]";
  assert_weaken_mutable_literals
    ~source:"['a']"
    ~against:"typing.List[typing_extensions.Literal['b']]"
    "typing.List[typing_extensions.Literal['a']]";
  assert_weaken_mutable_literals
    ~source:"[1]"
    ~against:"typing.List[typing_extensions.Literal[1]]"
    "typing.List[typing_extensions.Literal[1]]";
  assert_weaken_mutable_literals
    ~source:"[1]"
    ~against:"typing.List[typing_extensions.Literal[2]]"
    "typing.List[typing_extensions.Literal[1]]";
  assert_weaken_mutable_literals
    ~source:"[True]"
    ~against:"typing.List[typing_extensions.Literal[True]]"
    "typing.List[typing_extensions.Literal[True]]";
  assert_weaken_mutable_literals
    ~source:"[True]"
    ~against:"typing.List[typing_extensions.Literal[False]]"
    "typing.List[typing_extensions.Literal[True]]";
  assert_weaken_mutable_literals
    ~source:"test.MyEnum.ONE"
    ~against:"typing_extensions.Literal[test.MyEnum.ONE]"
    "typing_extensions.Literal[test.MyEnum.ONE]";
  assert_weaken_mutable_literals
    ~source:"test.MyEnum.TWO"
    ~against:"typing_extensions.Literal[test.MyEnum.ONE]"
    "typing_extensions.Literal[test.MyEnum.TWO]";
  ()


let test_weaken_mutable_literals_to_readonly context =
  let resolution =
    make_resolution
      ~context
      {|
      from pyre_extensions import ReadOnly

      class Base: ...
      class Child(Base): ...
      class Unrelated: ...

      readonly_base: ReadOnly[Base]
      readonly_child: ReadOnly[Child]
      readonly_unrelated: ReadOnly[Unrelated]
    |}
  in
  let assert_weaken_mutable_literals ~source ~against expected_output =
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
  assert_weaken_mutable_literals
    ~source:"[test.readonly_child]"
    ~against:"pyre_extensions.ReadOnly[typing.List[test.Base]]"
    "pyre_extensions.ReadOnly[typing.List[test.Base]]";
  assert_weaken_mutable_literals
    ~source:"[test.readonly_unrelated]"
    ~against:"pyre_extensions.ReadOnly[typing.List[test.Base]]"
    "typing.List[pyre_extensions.ReadOnly[test.Unrelated]]";
  assert_weaken_mutable_literals
    ~source:"[y for y in [test.Child()]]"
    ~against:"pyre_extensions.ReadOnly[typing.List[test.Base]]"
    "pyre_extensions.ReadOnly[typing.List[test.Base]]";
  assert_weaken_mutable_literals
    ~source:"[[test.readonly_child]]"
    ~against:"pyre_extensions.ReadOnly[typing.List[typing.List[test.Base]]]"
    "pyre_extensions.ReadOnly[typing.List[typing.List[test.Base]]]";
  assert_weaken_mutable_literals
    ~source:"[[test.readonly_unrelated]]"
    ~against:"pyre_extensions.ReadOnly[typing.List[typing.List[test.Base]]]"
    "typing.List[typing.List[pyre_extensions.ReadOnly[test.Unrelated]]]";
  assert_weaken_mutable_literals
    ~source:"{test.readonly_child}"
    ~against:"pyre_extensions.ReadOnly[typing.Set[test.Base]]"
    "pyre_extensions.ReadOnly[typing.Set[test.Base]]";
  assert_weaken_mutable_literals
    ~source:"{test.readonly_unrelated}"
    ~against:"pyre_extensions.ReadOnly[typing.Set[test.Base]]"
    "typing.Set[pyre_extensions.ReadOnly[test.Unrelated]]";
  assert_weaken_mutable_literals
    ~source:"{{test.readonly_child}}"
    ~against:"pyre_extensions.ReadOnly[typing.Set[typing.Set[test.Base]]]"
    "pyre_extensions.ReadOnly[typing.Set[typing.Set[test.Base]]]";
  assert_weaken_mutable_literals
    ~source:"{{test.readonly_unrelated}}"
    ~against:"pyre_extensions.ReadOnly[typing.Set[typing.Set[test.Base]]]"
    "typing.Set[typing.Set[pyre_extensions.ReadOnly[test.Unrelated]]]";
  assert_weaken_mutable_literals
    ~source:"{y for y in [test.Child()]}"
    ~against:"pyre_extensions.ReadOnly[typing.Set[test.Base]]"
    "pyre_extensions.ReadOnly[typing.Set[test.Base]]";
  assert_weaken_mutable_literals
    ~source:"{ 's': test.readonly_child }"
    ~against:"pyre_extensions.ReadOnly[typing.Dict[str, test.Base]]"
    "pyre_extensions.ReadOnly[typing.Dict[str, test.Base]]";
  assert_weaken_mutable_literals
    ~source:"{ 's': test.readonly_unrelated }"
    ~against:"pyre_extensions.ReadOnly[typing.Dict[str, test.Base]]"
    "typing.Dict[str, pyre_extensions.ReadOnly[test.Unrelated]]";
  assert_weaken_mutable_literals
    ~source:"{ 's': y for y in [test.readonly_child] }"
    ~against:"pyre_extensions.ReadOnly[typing.Dict[str, test.Base]]"
    "pyre_extensions.ReadOnly[typing.Dict[str, test.Base]]";
  assert_weaken_mutable_literals
    ~source:"{ 's': y for y in [test.readonly_unrelated] }"
    ~against:"pyre_extensions.ReadOnly[typing.Dict[str, test.Base]]"
    "typing.Dict[str, pyre_extensions.ReadOnly[test.Unrelated]]";
  assert_weaken_mutable_literals
    ~source:"{ 'x': { 's': test.readonly_child }}"
    ~against:"pyre_extensions.ReadOnly[typing.Dict[str, typing.Dict[str, test.Base]]]"
    "pyre_extensions.ReadOnly[typing.Dict[str, typing.Dict[str, test.Base]]]";
  assert_weaken_mutable_literals
    ~source:"{ 'x': { 's': test.readonly_unrelated }}"
    ~against:"pyre_extensions.ReadOnly[typing.Dict[str, typing.Dict[str, test.Base]]]"
    "typing.Dict[str, typing.Dict[str, pyre_extensions.ReadOnly[test.Unrelated]]]";
  ()


let test_weaken_mutable_literal_against_union context =
  let resolution =
    make_resolution ~context {|
      class C: ...
      class D(C): ...
      class Q: ...
    |}
  in
  let assert_weaken_mutable_literals ~source ~against expected_output =
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
  assert_weaken_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Optional[typing.List[test.C]]"
    "typing.Optional[typing.List[test.C]]";
  assert_weaken_mutable_literals
    ~source:"[[test.D()]]"
    ~against:"typing.Optional[typing.List[typing.List[test.C]]]"
    "typing.Optional[typing.List[typing.List[test.C]]]";
  assert_weaken_mutable_literals
    ~source:{|["foo"]|}
    ~against:"typing.Optional[typing.List[typing.Union[typing.List[str], str]]]"
    "typing.Optional[typing.List[typing.Union[typing.List[str], str]]]";
  assert_weaken_mutable_literals
    ~source:"{test.D()}"
    ~against:"typing.Optional[typing.Set[test.C]]"
    "typing.Optional[typing.Set[test.C]]";
  assert_weaken_mutable_literals
    ~source:"{{test.D()}}"
    ~against:"typing.Optional[typing.Set[typing.Set[test.C]]]"
    "typing.Optional[typing.Set[typing.Set[test.C]]]";
  assert_weaken_mutable_literals
    ~source:"(test.D(),)"
    ~against:"typing.Optional[typing.Tuple[test.C, ...]]"
    "typing.Optional[typing.Tuple[test.C, ...]]";
  assert_weaken_mutable_literals
    ~source:"((test.D(),),)"
    ~against:"typing.Optional[typing.Tuple[typing.Tuple[test.C, ...], ...]]"
    "typing.Optional[typing.Tuple[typing.Tuple[test.C, ...], ...]]";
  assert_weaken_mutable_literals
    ~source:"{test.D(): 2}"
    ~against:"typing.Optional[typing.Dict[test.C, int]]"
    "typing.Optional[typing.Dict[test.C, int]]";
  assert_weaken_mutable_literals
    ~source:"{test.D(): 2}"
    ~against:"typing.Optional[typing.Mapping[test.C, int]]"
    "typing.Optional[typing.Mapping[test.C, int]]";

  (* Unions. *)
  assert_weaken_mutable_literals
    ~source:"[test.C()]"
    ~against:"typing.Union[typing.List[test.C], int, str]"
    "typing.Union[typing.List[test.C], int, str]";
  assert_weaken_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Union[typing.List[test.C], int, str]"
    "typing.Union[typing.List[test.C], int, str]";
  assert_weaken_mutable_literals
    ~source:"[test.D()]"
    ~against:"typing.Union[int, str]"
    "typing.List[test.D]";
  assert_weaken_mutable_literals
    ~source:"{test.D()}"
    ~against:"typing.Union[typing.Set[test.C], int, str]"
    "typing.Union[typing.Set[test.C], int, str]";
  assert_weaken_mutable_literals
    ~source:"(test.D(),)"
    ~against:"typing.Union[typing.Tuple[test.C, ...], int, str]"
    "typing.Union[typing.Tuple[test.C, ...], int, str]";
  assert_weaken_mutable_literals
    ~source:"{1: test.D()}"
    ~against:"typing.Union[typing.Dict[int, test.C], int, str]"
    "typing.Union[typing.Dict[int, test.C], int, str]";
  assert_weaken_mutable_literals
    ~source:"{1: test.D()}"
    ~against:"typing.Union[typing.Mapping[int, test.C], int, str]"
    "typing.Union[typing.Mapping[int, test.C], int, str]";

  (* Distribute the resolved Union type over the mutable container before weakening. For example,
     List[Union[List[C], List[Q]]] to List[List[Union[C, Q]]]. *)
  assert_weaken_mutable_literals
    ~source:"[[test.C()], [test.Q()]]"
    ~against:"typing.List[typing.List[typing.Union[test.C, test.Q]]]"
    "typing.List[typing.List[typing.Union[test.C, test.Q]]]";
  assert_weaken_mutable_literals
    ~source:"[{test.C()}, {test.Q()}]"
    ~against:"typing.List[typing.Set[typing.Union[test.C, test.Q]]]"
    "typing.List[typing.Set[typing.Union[test.C, test.Q]]]";
  assert_weaken_mutable_literals
    ~source:"[{'foo': test.C()}, {'bar': test.Q()}]"
    ~against:"typing.List[typing.Dict[str, typing.Union[test.C, test.Q]]]"
    "typing.List[typing.Dict[str, typing.Union[test.C, test.Q]]]";
  assert_weaken_mutable_literals
    ~source:"[{'foo': None}, {'foo': 'bar'}]"
    ~against:"typing.List[typing.Dict[str, typing.Optional[str]]]"
    "typing.List[typing.Dict[str,typing.Optional[str]]]";
  assert_weaken_mutable_literals
    ~source:"{'hello': {'foo': None}, 'world': {'foo': 'bar'}}"
    ~against:"typing.Dict[str, typing.Dict[str, typing.Optional[str]]]"
    "typing.Dict[str,typing.Dict[str, typing.Optional[str]]]";
  (* Weakening against an explicit List[Union[List[C], List[Q]]] still works. *)
  assert_weaken_mutable_literals
    ~source:"[[test.C()], [test.Q()]]"
    ~against:"typing.List[typing.Union[typing.List[test.C], typing.List[test.Q]]]"
    "typing.List[typing.Union[typing.List[test.C], typing.List[test.Q]]]";
  ()


let test_weaken_mutable_literals_typed_dictionary context =
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
  let assert_weaken_mutable_literals ~source ~against_type expected_weakened_type =
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
  assert_weaken_mutable_literals
    ~source:"{}"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (create_failed_typed_dictionary_type
       ~resolved_type:
         (resolve_expression_with_fresh_namespace resolution (parse_single_expression "{}"))
       [
         MissingRequiredField { field_name = "name"; class_name = "test.ClassBasedMovie" };
         MissingRequiredField { field_name = "year"; class_name = "test.ClassBasedMovie" };
       ]);
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (make_weakened_type (Type.Primitive "test.ClassBasedMovie"));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999, 'rating': 10 }"
    ~against_type:(Type.Primitive "test.Child")
    (make_weakened_type (Type.Primitive "test.Child"));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.RegularClass")
    (make_weakened_type
       (Type.dictionary ~key:Type.string ~value:(Type.union [Type.integer; Type.string])));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.Child")
    (create_failed_typed_dictionary_type
       ~resolved_type:(parse_annotation "typing.Dict[str, typing.Union[int, str]]")
       [MissingRequiredField { field_name = "rating"; class_name = "test.Child" }]);
  assert_weaken_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (name_type_mismatch ~resolved:"typing.Dict[str, int]");
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999, 'extra_key': 1 }"
    ~against_type:(Type.Primitive "test.ClassBasedMovie")
    (make_weakened_type
       ~typed_dictionary_errors:
         [
           UndefinedField { field_name = "extra_key"; class_name = "test.ClassBasedMovie" }
           |> Node.create_with_default_location;
         ]
       (Type.Primitive "test.ClassBasedMovie"));
  assert_weaken_mutable_literals
    ~source:"{'hello': { 'name': 'The Matrix', 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie"))
    (make_weakened_type
       (Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie")));
  assert_weaken_mutable_literals
    ~source:
      "{'outer_foo': { 'name': 'The Matrix', 'year': 1999 }, 'outer_bar': { 'name': 'The Matrix', \
       'year': 1999 }}"
    ~against_type:(Type.Primitive "test.OuterTypedDict")
    (make_weakened_type (Type.Primitive "test.OuterTypedDict"));
  assert_weaken_mutable_literals
    ~source:
      "{'hello': { 'name': 'The Matrix', 'year': 1999 }, 'world': { 'name': 37, 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie"))
    (name_type_mismatch
       ~resolved:
         "typing.Dict[str, typing.Union[typing.Dict[str, int], typing.Dict[str, typing.Union[int, \
          str]], test.ClassBasedMovie]]");
  assert_weaken_mutable_literals
    ~source:"{'hello': { 'name': 37, 'year': 1999 }}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.ClassBasedMovie"))
    (name_type_mismatch ~resolved:"typing.Dict[str, typing.Dict[str, int]]");
  assert_weaken_mutable_literals
    ~source:
      "{'outer_foo': { 'name': 37, 'year': 1999 }, 'outer_bar': { 'name': 'The Matrix', 'year': \
       'NaN' }}"
    ~against_type:(Type.Primitive "test.OuterTypedDict")
    (name_and_year_type_mismatch
       ~resolved:"typing.Dict[str, typing.Union[typing.Dict[str, int], typing.Dict[str, str]]]");
  assert_weaken_mutable_literals
    ~source:"{'outer_dict': {'outer_foo': { 'name': 37, 'year': 1999 }}}"
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.OuterTypedDict"))
    (name_type_mismatch ~resolved:"typing.Dict[str, typing.Dict[str, typing.Dict[str, int]]]");
  let source = "{'outer_dict': {'outer_foo': {}}}" in
  assert_weaken_mutable_literals
    ~source
    ~against_type:(Type.dictionary ~key:Type.string ~value:(Type.Primitive "test.OuterTypedDict"))
    (create_failed_typed_dictionary_type
       ~resolved_type:
         (resolve_expression_with_fresh_namespace resolution (parse_single_expression source))
       [
         MissingRequiredField { field_name = "name"; class_name = "test.ClassBasedMovie" };
         MissingRequiredField { field_name = "year"; class_name = "test.ClassBasedMovie" };
       ]);
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer])
    (make_weakened_type (Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer]));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.optional (Type.Primitive "test.ClassBasedMovie"))
    (make_weakened_type (Type.optional (Type.Primitive "test.ClassBasedMovie")));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.optional (Type.Primitive "test.ClassBasedMovie"))
    (name_type_mismatch ~resolved:"typing.Dict[str, int]");
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer])
    (make_weakened_type (Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer]));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 37, 'year': 1999 }"
    ~against_type:(Type.union [Type.Primitive "test.ClassBasedMovie"; Type.integer])
    (name_type_mismatch ~resolved:"typing.Dict[str, int]");

  (* Weaken to get the dictionary in a Union instead of giving an error for the TypedDict. *)
  assert_weaken_mutable_literals
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

  assert_weaken_mutable_literals
    ~source:"[{ 'name': 'The Matrix', 'year': 1999 }]"
    ~against_type:(Type.list (Type.Primitive "test.ClassBasedMovie"))
    (make_weakened_type (Type.list (Type.Primitive "test.ClassBasedMovie")));
  assert_weaken_mutable_literals
    ~source:"[{ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' }]"
    ~against_type:(Type.list (Type.Primitive "test.ClassBasedMovie"))
    (name_and_year_type_mismatch
       ~resolved:"typing.List[typing.Union[typing.Dict[str, int], typing.Dict[str, str]]]");
  assert_weaken_mutable_literals
    ~source:"({ 'name': 'The Matrix', 'year': 1999 },)"
    ~against_type:(Type.tuple [Type.Primitive "test.ClassBasedMovie"])
    (make_weakened_type (Type.tuple [Type.Primitive "test.ClassBasedMovie"]));
  assert_weaken_mutable_literals
    ~source:"({ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' })"
    ~against_type:
      (Type.tuple [Type.Primitive "test.ClassBasedMovie"; Type.Primitive "test.ClassBasedMovie"])
    (name_and_year_type_mismatch
       ~resolved:"typing.Tuple[typing.Dict[str, int], typing.Dict[str, str]];");
  assert_weaken_mutable_literals
    ~source:"({ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' })"
    ~against_type:
      (Type.Tuple
         (Type.OrderedTypes.create_unbounded_concatenation (Type.Primitive "test.ClassBasedMovie")))
    (name_and_year_type_mismatch
       ~resolved:"typing.Tuple[typing.Dict[str, int], typing.Dict[str, str]];");
  assert_weaken_mutable_literals
    ~source:"[{ 'name': 37, 'year': 1999 }, { 'name': 'The Matrix', 'year': 'NaN' }]"
    ~against_type:(Type.sequence (Type.Primitive "test.ClassBasedMovie"))
    (name_and_year_type_mismatch
       ~resolved:"typing.Sequence[typing.Union[typing.Dict[str, int], typing.Dict[str, str]]];");

  (* Non-total typed dictionary. *)
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (make_weakened_type (Type.Primitive "test.NonTotalMovie"));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix'}"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (make_weakened_type (Type.Primitive "test.NonTotalMovie"));
  assert_weaken_mutable_literals
    ~source:"{}"
    ~against_type:(Type.Primitive "test.NonTotalMovie")
    (make_weakened_type (Type.Primitive "test.NonTotalMovie"));
  assert_weaken_mutable_literals
    ~source:"{ 'name': 'The Matrix', 'year': 1999 }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (make_weakened_type (Type.Primitive "test.NameNotRequiredYearRequired"));
  assert_weaken_mutable_literals
    ~source:"{'year': 1999 }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (make_weakened_type (Type.Primitive "test.NameNotRequiredYearRequired"));
  (* Note that we don't add [year] to the resolved type for a mismatch because [year] is a required
     field. *)
  assert_weaken_mutable_literals
    ~source:"{'name': 'The Matrix' }"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (create_failed_typed_dictionary_type
       ~resolved_type:(parse_annotation "typing.Dict[str, str]")
       [
         MissingRequiredField
           { field_name = "year"; class_name = "test.NameNotRequiredYearRequired" };
       ]);
  assert_weaken_mutable_literals
    ~source:"{}"
    ~against_type:(Type.Primitive "test.NameNotRequiredYearRequired")
    (create_failed_typed_dictionary_type
       ~resolved_type:
         (resolve_expression_with_fresh_namespace resolution (parse_single_expression "{}"))
       [
         MissingRequiredField
           { field_name = "year"; class_name = "test.NameNotRequiredYearRequired" };
       ]);
  assert_weaken_mutable_literals
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
  assert_weaken_mutable_literals
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


let test_weaken_mutable_literal_to_recursive_type context =
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
  let assert_weaken_mutable_literals ~source ~against expected_output =
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
  assert_weaken_mutable_literals ~source:"1" ~against:"test.Tree" "test.Tree";
  assert_weaken_mutable_literals ~source:"[]" ~against:"test.Tree" "test.Tree";
  assert_weaken_mutable_literals ~source:"[1]" ~against:"test.Tree" "test.Tree";
  assert_weaken_mutable_literals ~source:"[1, [2]]" ~against:"test.Tree" "test.Tree";
  assert_weaken_mutable_literals ~source:"[1, *[2]]" ~against:"test.Tree" "test.Tree";
  assert_weaken_mutable_literals ~source:"[1, [2, [3, 4]]]" ~against:"test.Tree" "test.Tree";
  assert_weaken_mutable_literals
    ~source:{| [1, [2, "hello"]] |}
    ~against:"test.Tree"
    "typing.List[typing.Union[typing.List[typing.Union[int, str]], int]]";

  assert_weaken_mutable_literals ~source:"{1, {2, {3, 4}}}" ~against:"test.TreeSet" "test.TreeSet";
  assert_weaken_mutable_literals
    ~source:"{1, True}"
    ~against:"test.TreeSet"
    "typing.Set[typing.Union[int, bool]]";

  assert_weaken_mutable_literals ~source:"{}" ~against:"test.JSON" "test.JSON";
  assert_weaken_mutable_literals ~source:"[1]" ~against:"test.JSON" "test.JSON";
  assert_weaken_mutable_literals
    ~source:{| {"a": 1, **{"b": {"c": 3}}} |}
    ~against:"test.JSON"
    "test.JSON";
  assert_weaken_mutable_literals
    ~source:{| {"a": 1, "b": [2, "hello"], "c": {"d": {}}} |}
    ~against:"test.JSON"
    "test.JSON";
  assert_weaken_mutable_literals
    ~source:{| {"a": {"b": True}} |}
    ~against:"test.JSON"
    "typing.Dict[str, typing.Dict[str, bool]]";
  ()


let () =
  "resolution"
  >::: [
         "weaken_mutable_literals" >:: test_weaken_mutable_literals;
         "weaken_mutable_literal_to_complex_type" >:: test_weaken_mutable_literal_against_union;
         "weaken_mutable_literals_to_readonly" >:: test_weaken_mutable_literals_to_readonly;
         "weaken_mutable_literals_typed_dictionary"
         >:: test_weaken_mutable_literals_typed_dictionary;
         "weaken_mutable_literal_to_recursive_type"
         >:: test_weaken_mutable_literal_to_recursive_type;
         "distribute_union_over_parametric" >:: test_distribute_union_over_parametric;
       ]
  |> Test.run
