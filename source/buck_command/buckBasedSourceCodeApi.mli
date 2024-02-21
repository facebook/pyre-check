(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create_for_testing
  :  ?get_source_code_api:(unit -> Analysis.SourceCodeApi.t) ->
  ?get_type_check_qualifiers:(unit -> Ast.Reference.t list) ->
  unit ->
  t

val create
  :  controls:Analysis.EnvironmentControls.t ->
  loader:FileLoader.t ->
  listing:Sourcedb.Listing.t ->
  unit ->
  t

val get_source_code_api : t -> Analysis.SourceCodeApi.t

val get_source_code_incremental_api : t -> Analysis.SourceCodeIncrementalApi.Base.t

val get_type_check_qualifiers : t -> Ast.Reference.t list
