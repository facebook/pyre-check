(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val apply_callable_query_rule
  :  verbose:bool ->
  resolution:Analysis.GlobalResolution.t ->
  rule:Taint.Model.ModelQuery.rule ->
  callable:Interprocedural.Callable.real_target ->
  (Taint.Model.annotation_kind * Taint.Model.taint_annotation) list

val apply_attribute_query_rule
  :  verbose:bool ->
  resolution:Analysis.GlobalResolution.t ->
  rule:Taint.Model.ModelQuery.rule ->
  attribute:Ast.Reference.t ->
  Taint.Model.taint_annotation list

val apply_all_rules
  :  resolution:Analysis.Resolution.t ->
  scheduler:Scheduler.t ->
  configuration:Taint.TaintConfiguration.t ->
  rule_filter:int list option ->
  rules:Taint.Model.ModelQuery.rule list ->
  callables:Interprocedural.Callable.real_target list ->
  stubs:Interprocedural.Callable.HashSet.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  models:Taint.Result.call_model Interprocedural.Callable.Map.t ->
  Taint.Result.call_model Interprocedural.Callable.Map.t
