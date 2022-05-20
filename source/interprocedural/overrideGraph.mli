(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

(* See `.ml` for documentation of modules and functions. *)

module Heap : sig
  type t

  val empty : t

  val of_alist_exn : (Reference.t * Reference.t list) list -> t

  val fold : t -> init:'a -> f:(member:Reference.t -> subtypes:Reference.t list -> 'a -> 'a) -> 'a

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val from_source : environment:Analysis.TypeEnvironment.ReadOnly.t -> source:Source.t -> t

  val skip_overrides : to_skip:Reference.Set.t -> t -> t

  type cap_overrides_result = {
    overrides: t;
    skipped_overrides: Reference.t list;
  }

  val cap_overrides : maximum_overrides:int option -> t -> cap_overrides_result

  type serializable

  val to_serializable : t -> serializable

  val of_serializable : serializable -> t
end

module SharedMemory : sig
  val get_overriding_types : member:Reference.t -> Reference.t list option

  val overrides_exist : Reference.t -> bool

  val expand_override_targets : Target.t list -> Target.t list

  val from_heap : Heap.t -> unit

  val cleanup : Heap.t -> unit
end

val record_overrides_for_qualifiers
  :  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  skip_overrides:Reference.Set.t ->
  maximum_overrides:int option ->
  qualifiers:Reference.t list ->
  Heap.cap_overrides_result
