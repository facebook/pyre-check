(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* File containing useful general algorithm *)

open Core

let fold_balanced list ~f ~init =
  Option.value (List.reduce_balanced ~f (init :: list)) ~default:init


let rec cartesian_product = function
  | [] -> []
  | [list] -> List.map list ~f:(fun element -> [element])
  | head :: tail ->
      tail
      |> cartesian_product
      |> List.map ~f:(fun product -> List.map head ~f:(fun element -> element :: product))
      |> List.concat
