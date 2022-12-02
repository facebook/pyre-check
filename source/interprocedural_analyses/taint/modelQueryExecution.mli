(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module VariableMetadata : sig
  type t = {
    name: Ast.Reference.t;
    type_annotation: Ast.Expression.Expression.t option;
  }
  [@@deriving show, compare]
end

module ModelQueryRegistryMap : sig
  type t = Registry.t Core.String.Map.t

  val empty : t

  val set : t -> model_query_name:string -> models:Registry.t -> t

  val get : t -> string -> Registry.t option

  val merge : model_join:(Model.t -> Model.t -> Model.t) -> t -> t -> t

  val to_alist : t -> (string * Registry.t) list

  val mapi : t -> f:(model_query_name:string -> models:Registry.t -> Registry.t) -> t

  val get_model_query_names : t -> string list

  val get_models : t -> Registry.t list

  val get_registry : model_join:(Model.t -> Model.t -> Model.t) -> t -> Registry.t
end

module GlobalVariableQueries : sig
  val get_globals_and_annotations
    :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
    VariableMetadata.t list

  val apply_global_query
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    variable_metadata:VariableMetadata.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.TaintAnnotation.t list Core.String.Map.t
end

module DumpModelQueryResults : sig
  val dump_to_string : models_and_names:ModelQueryRegistryMap.t -> string

  val dump_to_file : models_and_names:ModelQueryRegistryMap.t -> path:PyrePath.t -> unit

  val dump_to_file_and_string
    :  models_and_names:ModelQueryRegistryMap.t ->
    path:PyrePath.t ->
    string
end

val apply_callable_query
  :  verbose:bool ->
  resolution:Analysis.GlobalResolution.t ->
  class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
  callable:Interprocedural.Target.t ->
  ModelParseResult.ModelQuery.t ->
  ModelParseResult.ModelAnnotation.t list Core.String.Map.t

val apply_attribute_query
  :  verbose:bool ->
  resolution:Analysis.GlobalResolution.t ->
  class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
  variable_metadata:VariableMetadata.t ->
  ModelParseResult.ModelQuery.t ->
  ModelParseResult.TaintAnnotation.t list Core.String.Map.t

val apply_all_queries
  :  resolution:Analysis.Resolution.t ->
  scheduler:Scheduler.t ->
  taint_configuration:TaintConfiguration.SharedMemory.t ->
  class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  queries:ModelParseResult.ModelQuery.t list ->
  callables:Interprocedural.Target.t list ->
  stubs:Interprocedural.Target.HashSet.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  ModelQueryRegistryMap.t * ModelVerificationError.t list

val generate_models_from_queries
  :  taint_configuration:TaintConfiguration.SharedMemory.t ->
  class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  callables:Interprocedural.Target.t list ->
  stubs:Interprocedural.Target.t Base.Hash_set.t ->
  ModelParseResult.ModelQuery.t list ->
  ModelQueryRegistryMap.t * ModelVerificationError.t list
