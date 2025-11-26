(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create
  :  pyre_api:PyrePysaApi.ReadOnly.t ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  unit ->
  t

(* Compute the type of the given expression, or retrieve its type from the cache. `callable` is the
   callable whose source code contains the given expression. This must only be used when using the
   Pyre1 API. *)
val compute_or_retrieve_pyre_type
  :  t ->
  pyre_in_context:PyrePysaApi.InContext.t ->
  Ast.Expression.t ->
  Type.t

(* Compute the type of the given expression, or retrieve its type from the cache. `callable` is the
   callable whose source code contains the given expression. *)
val compute_or_retrieve_pysa_type
  :  t ->
  pyre_in_context:PyrePysaApi.InContext.t ->
  Ast.Expression.t ->
  Analysis.PyrePysaEnvironment.PysaType.t
