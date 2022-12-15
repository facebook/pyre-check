(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

val check_define
  :  type_resolution_for_statement:
       (local_annotations:LocalAnnotationMap.ReadOnly.t option ->
       parent:Ast.Reference.t option ->
       statement_key:int ->
       unit ->
       Resolution.t) ->
  global_resolution:GlobalResolution.t ->
  local_annotations:LocalAnnotationMap.ReadOnly.t option ->
  qualifier:Reference.t ->
  Statement.Define.t Node.t ->
  Error.t list

val check_module_TESTING_ONLY
  :  type_resolution_for_statement:
       (local_annotations:LocalAnnotationMap.ReadOnly.t option ->
       parent:Ast.Reference.t option ->
       statement_key:int ->
       unit ->
       Resolution.t) ->
  global_resolution:GlobalResolution.t ->
  local_annotations_for_define:(Ast.Reference.t -> LocalAnnotationMap.ReadOnly.t option) ->
  Source.t ->
  Error.t list
