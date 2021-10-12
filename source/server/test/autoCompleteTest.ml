(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open OUnit2
open Server

let test_remove_dot _ =
  let assert_remove_dot ~position ~original ~expected =
    let actual = AutoComplete.remove_dot ~cursor_position:position original in
    assert_equal ~printer:ident expected actual
  in
  assert_remove_dot
    ~position:{ line = 3; column = 30 }
    ~original:{|
      a = 1 + 1
      function_call(variable1., variable2)
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      function_call(variable1, variable2)
      b = 2 + 2
    |};

  (* Edge case: DOT is the first character in a line *)
  assert_remove_dot
    ~position:{ line = 1; column = 1 }
    ~original:"., variable2)"
    ~expected:", variable2)";

  (* Edge case: DOT is the last character in a line *)
  assert_remove_dot
    ~position:{ line = 3; column = 16 }
    ~original:{|
      a = 1 + 1
      variable1.
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      variable1
      b = 2 + 2
    |};

  (* Edge case: DOT is in column 0, avoid erroring in this case. *)
  assert_remove_dot
    ~position:{ line = 3; column = 0 }
    ~original:{|
      a = 1 + 1
      .
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      .
      b = 2 + 2
    |};

  (* Edge case: pos and len are past end. *)
  assert_remove_dot
    ~position:{ line = 4; column = 20 }
    ~original:{|
      a = 1 + 1
      variable1.
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      variable1.
      b = 2 + 2
    |}


let test_find_module_reference _ =
  let assert_resolved_module ~position ~source ~expected =
    let actual = AutoComplete.find_module_reference ~cursor_position:position source in
    match actual with
    | Some actual -> assert_equal ~printer:Reference.show expected actual
    | None -> failwith "Module reference not found."
  in
  assert_resolved_module
    ~position:{ line = 3; column = 32 }
    ~source:{|
      a = 1 + 1
      function_call(module_name.)
      b = 2 + 2
    |}
    ~expected:(Reference.create "module_name");
  assert_resolved_module
    ~position:{ line = 3; column = 18 }
    ~source:{|
      a = 1 + 1
      module_name.
      b = 2 + 2
    |}
    ~expected:(Reference.create "module_name")


let () =
  "autoComplete"
  >::: ["remove_dot" >:: test_remove_dot; "find_module_reference" >:: test_find_module_reference]
  |> Test.run
