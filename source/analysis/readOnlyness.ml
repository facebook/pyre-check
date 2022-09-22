(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Mutable
  | ReadOnly
[@@deriving compare, sexp, show]

let name = "ReadOnlyness"

let bottom = Mutable

let less_or_equal ~left ~right =
  match left, right with
  | _, ReadOnly -> true
  | _ -> [%compare.equal: t] left right


let join left right =
  match left, right with
  | ReadOnly, _
  | _, ReadOnly ->
      ReadOnly
  | _ -> Mutable


let meet left right =
  match left, right with
  | Mutable, _
  | _, Mutable ->
      Mutable
  | _ -> ReadOnly
