(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** A BigList is a list that keeps a "length" integer, so that BigList.length can be O(1) rather
    than O(n). *)
type 'a t [@@deriving sexp, hash, eq]

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

(* This is O(min(m, n)) where m and n are lengths of the input lists. No order is preserved. *)
val merge : 'a t -> 'a t -> 'a t

(* This is O(1). *)
val length : 'a t -> int

(* This is O(n) as it traverses input list to calculate length. *)
val of_list : 'a list -> 'a t

(* This is O(1). It simply discards its memory of the length. *)
val to_list : 'a t -> 'a list
