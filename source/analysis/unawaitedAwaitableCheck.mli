(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

val check_define
  :  resolution:Resolution.t ->
  local_annotations:LocalAnnotationMap.ReadOnly.t option ->
  qualifier:Reference.t ->
  Statement.Define.t Node.t ->
  Error.t list

val check_module_TESTING_ONLY
  :  resolution:Resolution.t ->
  local_annotations_for_define:(Ast.Reference.t -> LocalAnnotationMap.ReadOnly.t option) ->
  Source.t ->
  Error.t list
