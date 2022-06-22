(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  :  ?precondition:Refinement.Store.t ->
  ?postcondition:Refinement.Store.t ->
  statement_key:int ->
  t ->
  unit

module ReadOnly : sig
  type t [@@deriving equal]

  val get_precondition : t -> statement_key:int -> Refinement.Store.t option

  val get_postcondition : t -> statement_key:int -> Refinement.Store.t option
end

val read_only : t -> ReadOnly.t
