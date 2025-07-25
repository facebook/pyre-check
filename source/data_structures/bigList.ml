(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* TODO(T170743593) new warning with ppx_conv_sexp.v0.16.X *)
[@@@warning "-name-out-of-scope"]

module List = Core.List

type 'a t = {
  length: Core.Int.t;
  list: 'a List.t;
}
[@@deriving sexp, hash, equal]

let empty = { length = 0; list = [] }

let cons element { length; list } = { length = length + 1; list = element :: list }

(*
 * n = first parameter list length, m = second parameter BigList length
 * This is O(n) of the input list, but avoids the expensive traversal of a BigList.
 * We assume the first parameter to be a small list, and the second to be large, n << m.
 *)
let append list { length; list = big_list } =
  { length = length + List.length list; list = List.append list big_list }


let length { length; _ } = length

let of_list list = { length = List.length list; list }

let to_list { list; _ } = list
