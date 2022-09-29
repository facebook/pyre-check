(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Mutable
  | ReadOnly
[@@deriving compare, sexp, hash]

(* Explicitly list `pp` because the `ELEMENT` module type has its own show, meaning that the derived
   `show` would seem unused. *)
val pp : Format.formatter -> t -> unit

include Abstract.SimpleDomain.ELEMENT with type t := t

val of_type : Type.t -> t
