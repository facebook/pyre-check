(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Interprocedural
open Test

let test_resolve_ignoring_errors context =
  let assert_resolved ~source ~expression ~expected =
    let resolution =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.build_resolution
    in
    CallResolution.resolve_ignoring_errors ~resolution (Test.parse_single_expression expression)
    |> assert_equal ~printer:Type.show expected
  in
  assert_resolved
    ~source:{|
    class Data:
      def __init__(self, x: int) -> None: ...
  |}
    ~expression:"x.Data()"
    ~expected:(Type.Primitive "x.Data");
  assert_resolved
    ~source:
      {|
    from pyre_extensions import ReadOnly
    from typing_extensions import Self

    class Foo:
      def readonly(self: ReadOnly[Self]) -> ReadOnly[Self]:
        return self
  |}
    ~expression:"x.Foo().readonly()"
    ~expected:(Type.Primitive "x.Foo")


let () =
  "interproceduralCallResolution"
  >::: ["resolve_ignoring_errors" >:: test_resolve_ignoring_errors]
  |> Test.run
