(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Intervals that represent non-strict subclasses of a class. *)
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
