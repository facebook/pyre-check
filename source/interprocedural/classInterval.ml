(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
include Interval.Int

let lower_bound_exn interval = Option.value_exn (Interval.Int.lbound interval)

let upper_bound_exn interval = Option.value_exn (Interval.Int.ubound interval)

let meet left right = Interval.Int.intersect left right

let join left right = Interval.Int.convex_hull [left; right]

let equal left right =
  Interval.Int.is_subset left ~of_:right && Interval.Int.is_subset right ~of_:left


let is_empty = Interval.Int.is_empty

let bottom = empty

let top = create min_int max_int

let is_top interval =
  (not (is_empty interval)) && lbound_exn interval == min_int && ubound_exn interval == max_int


let less_or_equal ~left ~right = Interval.Int.is_subset left ~of_:right

let pp_interval formatter interval =
  if Interval.Int.is_empty interval then
    Format.fprintf formatter "<empty>"
  else
    Format.fprintf
      formatter
      "%d,%d"
      (Option.value_exn (Interval.Int.lbound interval))
      (Option.value_exn (Interval.Int.ubound interval))


let pp formatter interval = Format.fprintf formatter "@[[%a]@]" pp_interval interval

let show = Format.asprintf "%a" pp
