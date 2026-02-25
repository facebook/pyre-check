(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module PyrePysaApi = Interprocedural.PyrePysaApi

module ExecutionResult : sig
  type t

  val get_models : t -> TaintFixpoint.SharedModels.t

  val get_errors : t -> ModelVerificationError.t list

  val create_empty : unit -> t

  val dump_to_string : t -> string

  val dump_to_file : t -> path:PyrePath.t -> unit

  val dump_to_file_and_string : t -> path:PyrePath.t -> string
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

  val write : t -> kind:string -> name:string -> target:Target.t -> t

  val read : t -> kind:string -> name:string -> Target.Set.t
end

module CandidateTargetsFromCache : sig
  type t =
    | Top
    | Set of Target.Set.t
  [@@deriving show, equal]

  val from_constraint : ReadWriteCache.t -> ModelParseResult.ModelQuery.Constraint.t -> t
end

module CallableQueryExecutor : sig
  val generate_annotations_from_query_on_target
    :  verbose:bool ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    modelable:ModelParseResult.Modelable.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.ModelAnnotation.t list

  val generate_cache_from_queries_on_targets
    :  verbose:bool ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    targets:Target.t list ->
    ModelParseResult.ModelQuery.t list ->
    ReadWriteCache.t

  val make_modelable
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    Target.t ->
    ModelParseResult.Modelable.t
end

module AttributeQueryExecutor : sig
  val generate_annotations_from_query_on_target
    :  verbose:bool ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    modelable:ModelParseResult.Modelable.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.TaintAnnotation.t list

  val generate_cache_from_queries_on_targets
    :  verbose:bool ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    targets:Target.t list ->
    ModelParseResult.ModelQuery.t list ->
    ReadWriteCache.t

  val get_attributes : scheduler:Scheduler.t -> pyre_api:PyrePysaApi.ReadOnly.t -> Target.t list

  val make_modelable
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    Target.t ->
    ModelParseResult.Modelable.t
end

module GlobalVariableQueryExecutor : sig
  val generate_annotations_from_query_on_target
    :  verbose:bool ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    modelable:ModelParseResult.Modelable.t ->
    ModelParseResult.ModelQuery.t ->
    ModelParseResult.TaintAnnotation.t list

  val generate_cache_from_queries_on_targets
    :  verbose:bool ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.SharedMemory.t ->
    targets:Target.t list ->
    ModelParseResult.ModelQuery.t list ->
    ReadWriteCache.t

  val get_globals : scheduler:Scheduler.t -> pyre_api:PyrePysaApi.ReadOnly.t -> Target.t list

  val make_modelable
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
    Target.t ->
    ModelParseResult.Modelable.t
end

val generate_models_from_queries
  :  pyre_api:PyrePysaApi.ReadOnly.t ->
  scheduler:Scheduler.t ->
  scheduler_policies:Configuration.SchedulerPolicies.t ->
  callables_to_definitions_map:Interprocedural.CallablesSharedMemory.ReadOnly.t ->
  class_hierarchy_graph:Interprocedural.ClassHierarchyGraph.Heap.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  verbose:bool ->
  error_on_unexpected_models:bool ->
  error_on_empty_result:bool ->
  definitions_and_stubs:Target.t list ->
  ModelParseResult.ModelQuery.t list ->
  ExecutionResult.t
