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
  let fold2 = Algorithms.fold_divide_and_conquer in
  let input_strings = ["", []; "H", ["e"; "l"; "l"; "o"]] in
  let input_ints = [0, [1; 2; 3; 4; 5]; -1, [-1; -1]] in
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
  loop ~f:( - ) ~cmp:( != ) input_ints;
  ()


let () = "file" >::: ["fold_correctness" >:: test_fold_correctness] |> Test.run
