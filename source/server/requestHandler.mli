(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val instantiate_error
  :  build_system:BuildSystem.t ->
  configuration:Configuration.Analysis.t ->
  ast_environment:Analysis.AstEnvironment.ReadOnly.t ->
  Analysis.AnalysisError.t ->
  Analysis.AnalysisError.Instantiated.t

val create_info_response : ServerProperties.t -> Response.t

val process_request
  :  properties:ServerProperties.t ->
  state:ServerState.t ->
  Request.t ->
  (ServerState.t * Response.t) Lwt.t
