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

  val from_qualifier
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    skip_overrides_targets:Reference.SerializableSet.t ->
    Reference.t ->
    t

  type cap_overrides_result = {
    overrides: t;
    skipped_overrides: Target.t list;
  }

  (** If a method has too many overrides, ignore them. *)
  val cap_overrides
    :  analyze_all_overrides_targets:Target.Set.t ->
    maximum_overrides:int option ->
    t ->
    cap_overrides_result
end

(** Override graph in the shared memory, a mapping from a method to classes directly overriding it. *)
module SharedMemory : sig
  type t

  val create : unit -> t

  (** Record a heap override graph in shared memory and return the handle to the storage location. *)
  val from_heap : Heap.t -> t

  val to_heap : t -> Heap.t

  (** Remove an override graph from shared memory. This must be called before storing another
      override graph. *)
  val cleanup : t -> unit

  val save_to_cache : t -> unit

  val load_from_cache : unit -> (t, SaveLoadSharedMemory.Usage.t) result

  module ReadOnly : sig
    type t

    val get_overriding_types : t -> member:Target.t -> Reference.t list option

    val overrides_exist : t -> Target.t -> bool

    val expand_override_targets : t -> Target.t list -> Target.t list
  end

  val read_only : t -> ReadOnly.t
end

type skipped_overrides = Target.t list

type whole_program_overrides = {
  override_graph_heap: Heap.t;
  override_graph_shared_memory: SharedMemory.t;
  skipped_overrides: skipped_overrides;
}

(** Compute the override graph, which maps overide_targets (parent methods which are overridden) to
    all concrete methods overriding them, and save it to shared memory. *)
val build_whole_program_overrides
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  skip_overrides_targets:Reference.SerializableSet.t ->
  maximum_overrides:int option ->
  analyze_all_overrides_targets:Target.Set.t ->
  qualifiers:Reference.t list ->
  whole_program_overrides
