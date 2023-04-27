(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ModelQueryRegistryMap : sig
  type t

  val empty : t

  val add : t -> model_query_identifier:string -> registry:Registry.t -> t

  val get : t -> string -> Registry.t option

  val merge : model_join:(Model.t -> Model.t -> Model.t) -> t -> t -> t

  val to_alist : t -> (string * Registry.t) list

  val mapi : t -> f:(model_query_identifier:string -> models:Registry.t -> Registry.t) -> t

  val get_model_query_identifiers : t -> string list

  val get_models : t -> Registry.t list

  val get_registry : model_join:(Model.t -> Model.t -> Model.t) -> t -> Registry.t
end

module DumpModelQueryResults : sig
  val dump_to_string : model_query_results:ModelQueryRegistryMap.t -> string

  val dump_to_file : model_query_results:ModelQueryRegistryMap.t -> path:PyrePath.t -> unit

  val dump_to_file_and_string
    :  model_query_results:ModelQueryRegistryMap.t ->
    path:PyrePath.t ->
    string
end

module PartitionCacheQueries : sig
  type t = {
    write_to_cache: ModelParseResult.ModelQuery.t list;
    read_from_cache: ModelParseResult.ModelQuery.t list;
    others: ModelParseResult.ModelQuery.t list;
  }
  [@@deriving show, equal]

  val partition : ModelParseResult.ModelQuery.t list -> t
end

module ReadWriteCache : sig
  type t [@@deriving show, equal]

  val empty : t

  val write : t -> kind:string -> name:string -> target:Interprocedural.Target.t -> t

  val read : t -> kind:string -> name:string -> Interprocedural.Target.Set.t
end

module CandidateTargetsFromCache : sig
  type t =
    | Top
    | Set of Interprocedural.Target.Set.t
  [@@deriving show, equal]

  val from_constraint : ReadWriteCache.t -> ModelParseResult.ModelQuery.Constraint.t -> t
end

module CallableQueryExecutor : sig
  val generate_annotations_from_query_on_target
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    target:Interprocedural.Target.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.ModelAnnotation.t list

  val generate_cache_from_queries_on_targets
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    targets:Interprocedural.Target.t list ->
    ModelParseResult.ModelQuery.t list ->
    ReadWriteCache.t
end

module AttributeQueryExecutor : sig
  val generate_annotations_from_query_on_target
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    target:Interprocedural.Target.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.TaintAnnotation.t list

  val generate_cache_from_queries_on_targets
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    targets:Interprocedural.Target.t list ->
    ModelParseResult.ModelQuery.t list ->
    ReadWriteCache.t

  val get_attributes : resolution:Analysis.GlobalResolution.t -> Interprocedural.Target.t list

  val get_type_annotation
    :  resolution:Analysis.GlobalResolution.t ->
    string ->
    string ->
    Ast.Expression.t option
end

module GlobalVariableQueryExecutor : sig
  val generate_annotations_from_query_on_target
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    target:Interprocedural.Target.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.TaintAnnotation.t list

  val generate_cache_from_queries_on_targets
    :  verbose:bool ->
    resolution:Analysis.GlobalResolution.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    targets:Interprocedural.Target.t list ->
    ModelParseResult.ModelQuery.t list ->
    ReadWriteCache.t

  val get_globals : resolution:Analysis.GlobalResolution.t -> Interprocedural.Target.t list

  val get_type_annotation
    :  resolution:Analysis.GlobalResolution.t ->
    Ast.Reference.t ->
    Ast.Expression.t option
end

val generate_models_from_queries
  :  resolution:Analysis.GlobalResolution.t ->
  scheduler:Scheduler.t ->
  class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.Heap.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  verbose:bool ->
  callables_and_stubs:Interprocedural.Target.t list ->
  stubs:Interprocedural.Target.t Base.Hash_set.t ->
  ModelParseResult.ModelQuery.t list ->
  ModelQueryRegistryMap.t * ModelVerificationError.t list
