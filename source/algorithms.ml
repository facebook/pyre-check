(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let fold_divide_and_conquer list ~f ~init =
  let split_in_half list = List.split_n list (List.length list / 2) in
  let rec recursive (left, right) =
    match left, right with
    | [], [] -> init
    | [element], []
    | [], [element] ->
        element
    | [left], [right] -> f left right
    | left, right -> f (recursive (split_in_half left)) (recursive (split_in_half right))
  in
  match list with
  | [] -> init
  | list -> f init (recursive (split_in_half list))
