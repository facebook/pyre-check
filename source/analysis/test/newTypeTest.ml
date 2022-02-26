(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let test_transform_ast _ =
  let assert_expand ?(handle = "qualifier.py") source expected =
    let parse = parse ~handle in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_new_types (parse source))
  in
  assert_expand
    {|
      from typing import NewType
      T = NewType('T', int)
    |}
    {|
      from typing import NewType
      class T(int):
        def __init__(self, input: int) -> None:
          pass
    |};
  assert_expand
    {|
      import typing
      T = typing.NewType('T', int)
    |}
    {|
      import typing
      class T(int):
        def __init__(self, input: int) -> None:
          pass
    |};
  assert_expand
    {|
      import typing
      T = typing.NewType('T', typing.List[int])
    |}
    {|
      import typing
      class T(typing.List[int]):
        def __init__(self, input: typing.List[int]) -> None:
          pass
    |};
  assert_expand
    {|
      import typing
      T = typing.NewType('T', typing.Dict[str, typing.List[int]])
    |}
    {|
      import typing
      class T(typing.Dict[str, typing.List[int]]):
        def __init__(self, input: typing.Dict[str, typing.List[int]]) -> None:
          pass
    |};

  (* Don't recognize arbitrary NewType definitions. *)
  assert_expand
    {|
      class NewType: pass
      T = NewType('T', int)
    |}
    {|
      class NewType: pass
      T = NewType('T', int)
    |};
  assert_expand
    {|
      from derp import NewType
      T = NewType('T', int)
    |}
    {|
      from derp import NewType
      T = NewType('T', int)
    |};

  (* Don't transform non-toplevel statements. *)
  assert_expand
    {|
      def foo():
        T = typing.NewType('T', int)
    |}
    {|
      def foo():
        T = typing.NewType('T', int)
    |}


let () = "plugin_new_type" >::: ["transform_ast" >:: test_transform_ast] |> Test.run
