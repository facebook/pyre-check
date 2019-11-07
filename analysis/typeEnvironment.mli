(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
module Error = AnalysisError

module ReadOnly : sig
  type t

  val create
    :  ?get_errors:(Reference.t -> Error.t list) ->
    AnnotatedGlobalEnvironment.ReadOnly.t ->
    t

  val global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

  val global_resolution : t -> GlobalResolution.t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val get_errors : t -> Reference.t -> Error.t list
end

type t

val create : AnnotatedGlobalEnvironment.ReadOnly.t -> t

val global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

val global_resolution : t -> GlobalResolution.t

val ast_environment : t -> AstEnvironment.ReadOnly.t

val get_errors : t -> Reference.t -> Error.t list

val set_errors : t -> Reference.t -> Error.t list -> unit

val invalidate : t -> Reference.t list -> unit

val read_only : t -> ReadOnly.t
