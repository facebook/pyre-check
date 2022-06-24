(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ModelQueryRegistryMap : sig
  type t = Taint.Registry.t Core.String.Map.t

  val empty : t

  val set : t -> model_query_name:string -> models:Taint.Registry.t -> t

  val get : t -> string -> Taint.Registry.t option

  val merge : model_join:(Taint.Model.t -> Taint.Model.t -> Taint.Model.t) -> t -> t -> t

  val to_alist : t -> (string * Taint.Registry.t) list

  val mapi : t -> f:(model_query_name:string -> models:Taint.Registry.t -> Taint.Registry.t) -> t

  val get_model_query_names : t -> string list

  val get_models : t -> Taint.Registry.t list

  val get_registry
    :  model_join:(Taint.Model.t -> Taint.Model.t -> Taint.Model.t) ->
    t ->
    Taint.Registry.t
end

module DumpModelQueryResults : sig
  val dump_to_string : models_and_names:ModelQueryRegistryMap.t -> string

  val dump_to_file : models_and_names:ModelQueryRegistryMap.t -> path:PyrePath.t -> unit

  val dump_to_file_and_string
    :  models_and_names:ModelQueryRegistryMap.t ->
    path:PyrePath.t ->
    string
end

val apply_callable_query_rule
  :  verbose:bool ->
  resolution:Analysis.GlobalResolution.t ->
  rule:Taint.ModelParser.Internal.ModelQuery.rule ->
  callable:Interprocedural.Target.t ->
  (Taint.ModelParser.Internal.annotation_kind * Taint.ModelParser.Internal.taint_annotation) list
  Core.String.Map.t

val apply_attribute_query_rule
  :  verbose:bool ->
  resolution:Analysis.GlobalResolution.t ->
  rule:Taint.ModelParser.Internal.ModelQuery.rule ->
  name:Ast.Reference.t ->
  annotation:Ast.Expression.t option ->
  Taint.ModelParser.Internal.taint_annotation list Core.String.Map.t

val apply_all_rules
  :  resolution:Analysis.Resolution.t ->
  scheduler:Scheduler.t ->
  configuration:Taint.TaintConfiguration.t ->
  rule_filter:int list option ->
  rules:Taint.ModelParser.Internal.ModelQuery.rule list ->
  callables:Interprocedural.Target.t list ->
  stubs:Interprocedural.Target.HashSet.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  ModelQueryRegistryMap.t * Taint.ModelVerificationError.t list

val generate_models_from_queries
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  callables:Interprocedural.Target.t list ->
  stubs:Interprocedural.Target.t Base.Hash_set.t ->
  taint_configuration:Taint.TaintConfiguration.t ->
  Taint.ModelParser.Internal.ModelQuery.rule list ->
  ModelQueryRegistryMap.t * Taint.ModelVerificationError.t list
