(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassInterval: represents non-strict subclasses of a class. Intervals are based on the DFS start
 * and finish discovery times when traversing the class hierarchy. For example, consider the
 * following classes.
 *
 * ```
 * class A: pass
 * class B(A): pass
 * class C(A): pass
 * class D(B, C): pass
 * ```
 *
 * Then, we may represent D's subclasses with [3,4], represent B's subclasses with [2,5] (which
 * subsumes subclass D's class interval [3,4]), represent C's subclasses with [3,4] and [6,7]
 * (which subsumes subclass D's class interval [3,4] and its own interval [6,7]), and represent A's
 * subclasses with [1,8] (which subsumes the class intervals of subclasses B, C, and D).
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
  if is_empty interval then
    Format.fprintf formatter "<empty>"
  else if is_top interval then
    Format.fprintf formatter "<top>"
  else
    Format.fprintf
      formatter
      "%d,%d"
      (Option.value_exn (Interval.Int.lbound interval))
      (Option.value_exn (Interval.Int.ubound interval))


let pp formatter interval = Format.fprintf formatter "@[[%a]@]" pp_interval interval

let show = Format.asprintf "%a" pp
