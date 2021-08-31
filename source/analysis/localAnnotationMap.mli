(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : unit -> t

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

val show : t -> string

val set
  :  ?precondition:Resolution.annotation_store ->
  ?postcondition:Resolution.annotation_store ->
  key:int ->
  t ->
  unit

module ReadOnly : sig
  type t

  val get_precondition : t -> int -> Resolution.annotation_store option

  val get_postcondition : t -> int -> Resolution.annotation_store option
end

val read_only : t -> ReadOnly.t
