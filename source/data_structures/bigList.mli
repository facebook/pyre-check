(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** A BigList is a list that keeps a "length" integer, so that BigList.length can be O(1) rather
    than O(n). *)
type 'a t [@@deriving sexp, hash, equal]

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

(*
 * n = first parameter list length, m = second parameter BigList length
 * This is O(n) of the input list, but avoids the expensive traversal of a BigList.
 * We assume the first parameter to be a small list, and the second to be large, n << m.
 *)
val append : 'a list -> 'a t -> 'a t

(* This is O(1). *)
val length : 'a t -> int

(* This is O(n) as it traverses input list to calculate length. *)
val of_list : 'a list -> 'a t

(* This is O(1). It simply discards its memory of the length. *)
val to_list : 'a t -> 'a list
