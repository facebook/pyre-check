(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val apply_query_rule
  :  resolution:Analysis.GlobalResolution.t ->
  rule:Taint.Model.ModelQuery.rule ->
  callable:Interprocedural.Callable.real_target ->
  (Taint.Model.annotation_kind * Taint.Model.taint_annotation) list
