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

type t =
  | Empty
  | Interval of {
      lower_bound: int;
      upper_bound: int;
    }
[@@deriving compare]

let empty = Empty

let create lower_bound upper_bound =
  if lower_bound > upper_bound then Empty else Interval { lower_bound; upper_bound }


let lower_bound_exn = function
  | Empty -> failwith "lower_bound_exn: Empty"
  | Interval { lower_bound; _ } -> lower_bound


let upper_bound_exn = function
  | Empty -> failwith "upper_bound_exn: Empty"
  | Interval { upper_bound; _ } -> upper_bound


let meet left right =
  match left, right with
  | Interval left, Interval right ->
      let lower_bound = max left.lower_bound right.lower_bound in
      let upper_bound = min left.upper_bound right.upper_bound in
      create lower_bound upper_bound
  | _ -> Empty


let join left right =
  match left, right with
  | Empty, _ -> right
  | _, Empty -> left
  | Interval left, Interval right ->
      let lower_bound = min left.lower_bound right.lower_bound in
      let upper_bound = max left.upper_bound right.upper_bound in
      create lower_bound upper_bound


let equal left right =
  match left, right with
  | Empty, Empty -> true
  | Interval left, Interval right ->
      Int.equal left.lower_bound right.lower_bound && Int.equal left.upper_bound right.upper_bound
  | _ -> false


let is_empty = function
  | Empty -> true
  | _ -> false


let bottom = empty

let top = create Int.min_value Int.max_value

let is_top = function
  | Empty -> false
  | Interval { lower_bound; upper_bound } ->
      Int.equal lower_bound Int.min_value && Int.equal upper_bound Int.max_value


let less_or_equal ~left ~right =
  match left, right with
  | Empty, _ -> true
  | _, Empty -> false
  | Interval left, Interval right ->
      left.lower_bound >= right.lower_bound && left.upper_bound <= right.upper_bound


let pp_interval formatter = function
  | Empty -> Format.fprintf formatter "<empty>"
  | Interval { lower_bound; upper_bound } ->
      if Int.equal lower_bound Int.min_value && Int.equal upper_bound Int.max_value then
        Format.fprintf formatter "<top>"
      else
        Format.fprintf formatter "%d,%d" lower_bound upper_bound


let pp formatter interval = Format.fprintf formatter "@[[%a]@]" pp_interval interval

let show = Format.asprintf "%a" pp
