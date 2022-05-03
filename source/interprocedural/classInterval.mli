(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Intervals that represent non-strict subclasses of a class. Intervals are based on the DFS start
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
type t

val empty : t

val is_empty : t -> bool

val equal : t -> t -> bool

(* Create a class interval that is representable as a single range. Multiple disjoint ranges can be
   approximated with the join operation. *)
val create : int -> int -> t

val join : t -> t -> t

val meet : t -> t -> t

val show : t -> string

val pp : Format.formatter -> t -> unit

val pp_interval : Format.formatter -> t -> unit

val bottom : t

val less_or_equal : left:t -> right:t -> bool

val top : t

val is_top : t -> bool

val lower_bound_exn : t -> int

val upper_bound_exn : t -> int
