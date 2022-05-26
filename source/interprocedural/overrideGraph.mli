(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

(** Override graph in the ocaml heap, storing a mapping from a method to classes overriding it. *)
module Heap : sig
  type t

  val empty : t

  val of_alist_exn : (Target.t * Reference.t list) list -> t

  val fold : t -> init:'a -> f:(member:Target.t -> subtypes:Reference.t list -> 'a -> 'a) -> 'a

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val from_source
    :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
    include_unit_tests:bool ->
    source:Source.t ->
    t

  val skip_overrides : to_skip:Reference.Set.t -> t -> t

  type cap_overrides_result = {
    overrides: t;
    skipped_overrides: Target.t list;
  }

  val cap_overrides : maximum_overrides:int option -> t -> cap_overrides_result
  (** If a method has too many overrides, ignore them. *)

  type serializable
  (** This can be used to cache the whole graph in shared memory. *)

  val to_serializable : t -> serializable

  val of_serializable : serializable -> t
end

(** Override graph in the shared memory, a mapping from a method to classes directly overriding it. *)
module SharedMemory : sig
  val get_overriding_types : member:Target.t -> Reference.t list option

  val overrides_exist : Target.t -> bool

  val expand_override_targets : Target.t list -> Target.t list

  val from_heap : Heap.t -> unit
  (** Records a heap override graph in shared memory. *)

  val cleanup : Heap.t -> unit
  (** Remove an override graph from shared memory. This must be called before storing another
      override graph. *)
end

val record_overrides_for_qualifiers
  :  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  include_unit_tests:bool ->
  skip_overrides:Reference.Set.t ->
  maximum_overrides:int option ->
  qualifiers:Reference.t list ->
  Heap.cap_overrides_result
(** Compute the override graph, which maps overide_targets (parent methods which are overridden) to
    all concrete methods overriding them, and save it to shared memory. *)
