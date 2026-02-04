(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

module Heap : sig
  type t [@@deriving show, equal]

  val of_alist_exn : (Reference.t * string) list -> t

  val empty : t

  val from_qualifier
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
    Ast.Reference.t ->
    t

  val from_qualifiers
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
    qualifiers:Reference.t list ->
    t
end

module SharedMemory : sig
  type t

  val create : unit -> t

  val from_heap : Heap.t -> t

  val from_qualifiers
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
    qualifiers:Reference.t list ->
    t

  module ReadOnly : sig
    type t

    val get : t -> cache:bool -> Reference.t -> StringLiteral.t option

    val mem : t -> Reference.t -> bool
  end

  val read_only : t -> ReadOnly.t

  val save_to_cache : t -> unit

  val load_from_cache : unit -> (t, SaveLoadSharedMemory.Usage.t) result

  val cleanup : t -> unit
end
