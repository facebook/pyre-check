(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let make_resolution ~context source =
  ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution


let global_resolution ~context =
  make_resolution
    ~context
    {|
      class FooNamedTuple(typing.NamedTuple):
        foo: str
        bar: int
        baz: typing.List[str]
        hello: typing.List[int]

      class NonTuple: ...
    |}
  |> Resolution.global_resolution


let test_field_annotations context =
  let global_resolution = global_resolution ~context in
  assert_equal
    (NamedTuple.field_annotations ~global_resolution (Type.Primitive "test.NonTuple"))
    None;
  assert_equal
    (NamedTuple.field_annotations ~global_resolution (Type.Primitive "test.FooNamedTuple"))
    (Some [Type.string; Type.integer; Type.list Type.string; Type.list Type.integer])


let test_is_named_tuple context =
  let global_resolution = global_resolution ~context in
  assert_false
    (NamedTuple.is_named_tuple ~global_resolution ~annotation:(Type.Primitive "test.NonTuple"));

  assert_true
    (NamedTuple.is_named_tuple ~global_resolution ~annotation:(Type.Primitive "test.FooNamedTuple"))


let () =
  "named_tuple"
  >::: ["field_annotations" >:: test_field_annotations; "is_named_tuple" >:: test_is_named_tuple]
  |> Test.run
