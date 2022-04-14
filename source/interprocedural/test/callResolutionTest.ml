(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Interprocedural
open Test

let test_resolve_ignoring_optional context =
  let assert_resolved_without_optional ~source ~expression ~expected =
    let resolution =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.build_resolution
    in
    CallResolution.resolve_ignoring_optional ~resolution (Test.parse_single_expression expression)
    |> assert_equal ~printer:Type.show expected
  in
  assert_resolved_without_optional
    ~source:{|
    class Data:
      def __init__(self, x: int) -> None: ...
  |}
    ~expression:"x.Data()"
    ~expected:(Type.Primitive "x.Data")


let () =
  "interproceduralCallResolution"
  >::: ["resolve_ignoring_optional" >:: test_resolve_ignoring_optional]
  |> Test.run
