(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module List = Core.List

type 'a t = {
  length: Core.Int.t;
  list: 'a List.t;
}
[@@deriving sexp, hash, eq]

let empty = { length = 0; list = [] }

let cons element { length; list } = { length = length + 1; list = element :: list }

(* This is O(min(m, n)) where m and n are lengths of the input lists. No order is preserved. *)
let merge { length = left_length; list = left_list } { length = right_length; list = right_list } =
  let length = left_length + right_length in
  (* It is faster to rev_append to a larger list than to a smaller list, because the time complexity
     will be the length of the smaller list. *)
  if left_length <= right_length then
    { length; list = List.rev_append left_list right_list }
  else
    { length; list = List.rev_append right_list left_list }


let length { length; _ } = length

let of_list list = { length = List.length list; list }

let to_list { list; _ } = list
