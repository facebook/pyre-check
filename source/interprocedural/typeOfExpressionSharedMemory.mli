(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create : callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t -> unit -> t

(* Compute the type of the given expression, or retrieve its type from the cache. `callable` is the
   callable whose source code contains the given expression. *)
val compute_or_retrieve_type
  :  t ->
  pyre_in_context:Analysis.PyrePysaEnvironment.InContext.t ->
  callable:Target.t ->
  Ast.Expression.t ->
  Type.t
