(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type response = {
  state: State.t;
  response: Protocol.response option;
}

(* Exposed for testing. *)
module AnnotationEdit : sig
  type t = {
    new_text: string;
    range: LanguageServer.Types.Range.t;
    title: string;
  }

  val range : t -> LanguageServer.Types.Range.t

  val new_text : t -> string

  val create : file:File.t -> error:Analysis.AnalysisError.t option -> t option
end

val process_client_shutdown_request
  :  state:State.t ->
  id:LanguageServer.Types.RequestId.t ->
  response

(* TODO (T82533515): `environment` should really be typed as `TypeEnvironment.ReadOnly.t`. *)
val process_type_query_request
  :  environment:Analysis.TypeEnvironment.t ->
  configuration:Configuration.Analysis.t ->
  Query.Request.t ->
  Query.Response.t

val process_display_type_errors_request
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  PyrePath.t list ->
  response

val process_type_check_request
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  PyrePath.t list ->
  response

val process_get_definition_request
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  request:Protocol.DefinitionRequest.t ->
  response

val process
  :  state:State.t ->
  configuration:Configuration.Server.t ->
  request:Protocol.Request.t ->
  response
