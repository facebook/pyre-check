(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module Make
    (Left : AbstractDomain.S)
    (Right : AbstractDomain.S) =
struct
  type t = Left.t * Right.t
  [@@deriving sexp]

  let bottom = Left.bottom, Right.bottom
  let is_bottom (l, r) =
    Left.is_bottom l && Right.is_bottom r

  let join (l1, r1) (l2, r2) =
    Left.join l1 l2, Right.join r1 r2

  let widen ~iteration ~previous:(prev_l, prev_r) ~next:(next_l, next_r) =
    Left.widen ~iteration ~previous:prev_l ~next:next_l,
    Right.widen ~iteration ~previous:prev_r ~next:next_r

  let less_or_equal ~left:(l1, r1) ~right:(l2, r2) =
    Left.less_or_equal ~left:l1 ~right:l2 &&
    Right.less_or_equal ~left:r1 ~right:r2

  let show (left, right) =
    Format.sprintf "(%s, %s)" (Left.show left) (Right.show right)

  let make l r = (l, r)
end
