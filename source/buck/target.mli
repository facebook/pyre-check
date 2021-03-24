(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving sexp, compare, hash]

val of_string : string -> t

val show : t -> string

val pp : Format.formatter -> t -> unit
