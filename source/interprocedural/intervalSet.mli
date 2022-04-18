(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A set of class intervals *)
type t

val of_list : ClassInterval.t list -> t

val to_list : t -> ClassInterval.t list

val show : t -> string

val show_list : ClassInterval.t list -> string

val equal : t -> t -> bool

val meet : t -> t -> t

val empty : t
