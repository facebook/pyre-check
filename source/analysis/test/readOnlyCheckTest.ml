(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Test
open ReadOnlyCheck
open ReadOnlyness

let test_forward_expression context =
  let global_resolution =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  let module Context = struct
    let qualifier = !&"test"

    let define =
      parse_single_define {|
      def foo() -> None: ...
    |}
      |> Node.create_with_default_location


    let error_map = Some (LocalErrorMap.empty ())

    let global_resolution = global_resolution
  end
  in
  let module State = State (Context) in
  let assert_resolved ?(resolution = Resolution.of_list []) expression expected_type =
    let { Resolved.resolved; _ } =
      parse_single_expression expression |> State.forward_expression ~resolution
    in
    assert_equal ~cmp:[%compare.equal: t] ~printer:show expected_type resolved
  in
  assert_resolved "..." ReadOnly;
  assert_resolved "False" ReadOnly;
  assert_resolved "True" ReadOnly;
  assert_resolved "1.2" ReadOnly;
  assert_resolved "42" ReadOnly;
  assert_resolved "'hello'" ReadOnly;
  assert_resolved "b'hello'" ReadOnly;
  assert_resolved "None" ReadOnly;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", ReadOnly]) "x" ReadOnly;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", Mutable]) "x" Mutable;
  assert_resolved ~resolution:(Resolution.of_list []) "x" Mutable;
  ()


let assert_readonly_errors ~context =
  let check ~environment ~source =
    source
    |> Preprocessing.defines ~include_toplevels:true
    |> List.concat_map
         ~f:
           (ReadOnlyCheck.readonly_errors_for_define
              ~type_environment:(TypeEnvironment.read_only environment)
              ~qualifier:!&"test")
  in
  assert_errors ~context ~check


let test_assignment context =
  let assert_readonly_errors = assert_readonly_errors ~context in
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation [3001]: z is declared to have readonlyness `ReadOnlyness.Mutable` but is \
       used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x = 42
        y = x
        z: ReadOnly[int] = y
    |}
    [];
  ()


let () =
  "readOnly"
  >::: ["forward_expression" >:: test_forward_expression; "assignment" >:: test_assignment]
  |> Test.run
