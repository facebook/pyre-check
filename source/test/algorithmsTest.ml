(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let test_fold_correctness _ =
  let fold1 = List.fold in
  let fold2 = Algorithms.fold_balanced in
  let input_strings = ["", []; "H", ["e"; "l"; "l"; "o"]] in
  let input_ints = [0, [1; 2; 3; 4; 5]; -1, [-1; -1; -1]] in
  let rec loop ~f ~cmp inputs =
    match inputs with
    | [] -> ()
    | (init, tail) :: rest ->
        let fold1_result = fold1 ~f ~init tail in
        let fold2_result = fold2 ~f ~init tail in
        assert_equal ~cmp fold1_result fold2_result;
        loop ~f ~cmp rest
  in
  loop ~f:( ^ ) ~cmp:String.equal input_strings;
  loop ~f:( + ) ~cmp:( = ) input_ints;
  (* Non associative operator *)
  loop ~f:( - ) ~cmp:Int.( <> ) input_ints;
  ()


let test_cartesian_product _ =
  let module ListOfIntList = struct
    type t = int list list [@@deriving show]
  end
  in
  let assert_equal ~input ~expected =
    assert_equal ~printer:ListOfIntList.show expected (Algorithms.cartesian_product input)
  in
  assert_equal ~input:[] ~expected:[];
  assert_equal ~input:[[1; 2; 3]] ~expected:[[1]; [2]; [3]];
  assert_equal ~input:[[1]; [2]] ~expected:[[1; 2]];
  assert_equal ~input:[[1; 2]; [3]] ~expected:[[1; 3]; [2; 3]];
  assert_equal ~input:[[1; 2]; [3; 4]] ~expected:[[1; 3]; [2; 3]; [1; 4]; [2; 4]];
  assert_equal
    ~input:[[1; 2]; [3; 4]; [5; 6; 7]]
    ~expected:
      [
        [1; 3; 5];
        [2; 3; 5];
        [1; 4; 5];
        [2; 4; 5];
        [1; 3; 6];
        [2; 3; 6];
        [1; 4; 6];
        [2; 4; 6];
        [1; 3; 7];
        [2; 3; 7];
        [1; 4; 7];
        [2; 4; 7];
      ];
  assert_equal ~input:[[3]; [1; 2]] ~expected:[[3; 1]; [3; 2]];
  assert_equal ~input:[[1; 2]; []; []] ~expected:[];
  assert_equal ~input:[[]; []; [1; 2]] ~expected:[];
  assert_equal ~input:[[]; [1; 2]; []; [3; 4]; []] ~expected:[]


let () =
  "file"
  >::: [
         "fold_correctness" >:: test_fold_correctness; "cartesian_product" >:: test_cartesian_product;
       ]
  |> Test.run
