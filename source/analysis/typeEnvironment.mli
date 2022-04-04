(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

module ReadOnly : sig
  type t

  val create
    :  ?get_errors:(Reference.t -> Error.t list) ->
    ?get_local_annotations:(Reference.t -> LocalAnnotationMap.ReadOnly.t option) ->
    AnnotatedGlobalEnvironment.ReadOnly.t ->
    t

  val global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

  val global_resolution : t -> GlobalResolution.t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val get_errors : t -> Reference.t -> Error.t list

  val get_local_annotations : t -> Reference.t -> LocalAnnotationMap.ReadOnly.t option
end

type t

val create : AnnotatedGlobalEnvironment.t -> t

val global_environment : t -> AnnotatedGlobalEnvironment.t

val ast_environment : t -> AstEnvironment.t

val module_tracker : t -> ModuleTracker.t

val get_errors : t -> Reference.t -> Error.t list

val get_local_annotations : t -> Reference.t -> LocalAnnotationMap.ReadOnly.t option

val set_errors : t -> Reference.t -> Error.t list -> unit

val set_local_annotations : t -> Reference.t -> LocalAnnotationMap.ReadOnly.t -> unit

val invalidate : t -> Reference.t list -> unit

val read_only : t -> ReadOnly.t
