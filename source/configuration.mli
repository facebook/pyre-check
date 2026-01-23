(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Buck : sig
  type t = {
    mode: string option;
    isolation_prefix: string option;
    bxl_builder: string option;
    targets: string list;
    (* This is the buck root of the source directory, i.e. output of `buck root`. *)
    source_root: PyrePath.t;
    (* This is the root of directory where built artifacts will be placed. *)
    artifact_root: PyrePath.t;
    targets_fallback_sources: SearchPath.t list option;
    kill_buck_after_build: bool;
    number_of_threads: int option;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module ChangeIndicator : sig
  type t = {
    root: PyrePath.t;
    relative: string;
  }
  [@@deriving sexp, compare, hash, yojson]

  val to_path : t -> PyrePath.t
end

module UnwatchedFiles : sig
  type t = {
    root: PyrePath.t;
    checksum_path: string;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module UnwatchedDependency : sig
  type t = {
    change_indicator: ChangeIndicator.t;
    files: UnwatchedFiles.t;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module SourcePaths : sig
  type t =
    | Simple of SearchPath.t list
    | WithUnwatchedDependency of {
        sources: SearchPath.t list;
        unwatched_dependency: UnwatchedDependency.t;
      }
    | Buck of Buck.t
  [@@deriving sexp, compare, hash, yojson]

  val to_search_paths : t -> SearchPath.t list
end

module RemoteLogging : sig
  type t = {
    logger: string;
    identifier: string;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module PythonVersion : sig
  type t = {
    major: int;
    minor: int;
    micro: int;
  }
  [@@deriving sexp, compare, hash, yojson, equal]

  val create : ?major:int -> ?minor:int -> ?micro:int -> unit -> t
end

module SharedMemory : sig
  type t = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving sexp, compare, hash, yojson]

  val default : t
end

module Extension : sig
  type t = {
    suffix: string;
    include_suffix_in_module_qualifier: bool;
  }
  [@@deriving show, sexp, compare, hash, yojson]

  val create_extension : string -> t

  val suffix : t -> string
end

module Analysis : sig
  type shared_memory = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving show]

  val default_enable_readonly_analysis : bool

  val default_enable_strict_override_check : bool

  val default_enable_strict_any_check : bool

  val default_enable_unawaited_awaitable_analysis : bool

  val default_include_suppressed_errors : bool

  type t = {
    parallel: bool;
    analyze_external_sources: bool;
    filter_directories: PyrePath.t list option;
    ignore_all_errors: PyrePath.t list option;
    number_of_workers: int;
    long_lived_workers: bool;
    local_root: PyrePath.t;
    debug: bool;
    project_root: PyrePath.t;
    source_paths: SearchPath.t list;
    search_paths: SearchPath.t list;
    taint_model_paths: PyrePath.t list;
    strict: bool;
    show_error_traces: bool;
    excludes: Str.regexp list;
    extensions: Extension.t list;
    store_type_check_resolution: bool;
    store_type_errors: bool;
    track_dependencies: bool;
    log_directory: PyrePath.t;
    python_version: PythonVersion.t;
    system_platform: string option;
    shared_memory: shared_memory;
    enable_type_comments: bool;
    enable_readonly_analysis: bool;
    enable_strict_override_check: bool;
    enable_strict_any_check: bool;
    enable_unawaited_awaitable_analysis: bool;
    include_suppressed_errors: bool;
  }

  val create
    :  ?parallel:bool ->
    ?analyze_external_sources:bool ->
    ?filter_directories:PyrePath.t list ->
    ?ignore_all_errors:PyrePath.t list ->
    ?number_of_workers:int ->
    ?long_lived_workers:bool ->
    ?local_root:PyrePath.t ->
    ?project_root:PyrePath.t ->
    ?search_paths:SearchPath.t list ->
    ?taint_model_paths:PyrePath.t list ->
    ?strict:bool ->
    ?debug:bool ->
    ?show_error_traces:bool ->
    ?excludes:string list ->
    ?extensions:Extension.t list ->
    ?store_type_check_resolution:bool ->
    ?store_type_errors:bool ->
    ?track_dependencies:bool ->
    ?log_directory:string ->
    ?python_version:PythonVersion.t ->
    ?system_platform:string option ->
    ?shared_memory_heap_size:int ->
    ?shared_memory_dependency_table_power_from_configuration:int ->
    ?shared_memory_hash_table_power:int ->
    ?enable_type_comments:bool ->
    ?enable_readonly_analysis:bool ->
    ?enable_strict_override_check:bool ->
    ?enable_strict_any_check:bool ->
    ?enable_unawaited_awaitable_analysis:bool ->
    ?include_suppressed_errors:bool ->
    source_paths:SearchPath.t list ->
    unit ->
    t

  val log_directory : t -> PyrePath.t

  val search_paths : t -> SearchPath.t list

  val extension_suffixes : t -> string list

  val find_extension : t -> ArtifactPath.t -> Extension.t option

  val validate_paths : t -> unit
end

(* Represents a scheduler policy, which can be converted into a `Scheduler.Policy` that can be used
   by `Scheduler.map_reduce`. *)
module SchedulerPolicy : sig
  type t =
    | FixedChunkSize of {
        minimum_chunk_size: int option;
        minimum_chunks_per_worker: int;
        preferred_chunk_size: int;
      }
    | FixedChunkCount of {
        minimum_chunks_per_worker: int option;
        minimum_chunk_size: int;
        preferred_chunks_per_worker: int;
      }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module ScheduleIdentifier : sig
  type t =
    | CollectDefinitions
    | TypeCheck
    | ComputeChangedPaths
    | SaveChangedPaths
    | TaintFetchCallables
    | MethodKinds
    | ClassHierarchyGraph
    | CallableModelQueries
    | AttributeModelQueries
    | GlobalModelQueries
    | InferClassModels
    | GlobalConstants
    | CallGraph
    | HigherOrderCallGraph
    | OverrideGraph
    | TaintFixpoint
    | TaintCollectErrors
    | TaintFileCoverage
    | TaintKindCoverage
    | DecoratorResolution
    | CallableToDecoratorsMap
    | CallablesSharedMemory
    | PyreflyParseSources
    | PyreflyParseModuleInfo
    | PyreflyCollectDefinitions
    | PyreflyParseClassParents
    | PyreflyParseTypeOfExpressions
    | PyreflyParseCallGraphs
  [@@deriving sexp, compare, hash]

  val of_string : string -> t option

  val to_string : t -> string
end

module SchedulerPolicies : sig
  type t [@@deriving sexp, compare, hash]

  val empty : t

  val of_alist_exn : (ScheduleIdentifier.t * SchedulerPolicy.t) list -> t

  val of_yojson : Yojson.Safe.t -> (t, string) result

  val get : t -> ScheduleIdentifier.t -> SchedulerPolicy.t option
end

module TaintOutputFormat : sig
  type t =
    | Json
    | ShardedJson
  [@@deriving sexp, compare, hash]

  val of_string : string -> (t, string) Result.t
end

module MissingFlowKind : sig
  type t =
    (* Find missing flows through obscure models. *)
    | Obscure
    (* Find missing flows due to missing type information. *)
    | Type
  [@@deriving sexp, compare, equal, hash]

  val of_string : string -> (t, string) Result.t
end

module StaticAnalysis : sig
  module SavedState : sig
    type t = {
      watchman_root: string option;
      project_name: string option;
      preset: string option;
      cache_critical_files: string list;
    }
    [@@deriving sexp, compare, hash, yojson]

    val empty : t
  end

  type t = {
    repository_root: PyrePath.t option;
    (* A directory to write files in. *)
    save_results_to: PyrePath.t option;
    output_format: TaintOutputFormat.t;
    pyrefly_results: PyrePath.t option;
    dump_call_graph: PyrePath.t option;
    verify_models: bool;
    verify_dsl: bool;
    verify_taint_config_only: bool;
    (* Analysis configuration *)
    configuration: Analysis.t;
    rule_filter: int list option;
    source_filter: string list option;
    sink_filter: string list option;
    transform_filter: string list option;
    find_missing_flows: MissingFlowKind.t option;
    dump_model_query_results: PyrePath.t option;
    use_cache: bool;
    build_cache_only: bool;
    infer_self_tito: bool;
    infer_argument_tito: bool;
    maximum_model_source_tree_width: int option;
    maximum_model_sink_tree_width: int option;
    maximum_model_tito_tree_width: int option;
    maximum_tree_depth_after_widening: int option;
    maximum_return_access_path_width: int option;
    maximum_return_access_path_depth_after_widening: int option;
    maximum_tito_collapse_depth: int option;
    maximum_tito_positions: int option;
    maximum_overrides_to_analyze: int option;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
    maximum_capture_trace_length: int option;
    check_invariants: bool;
    limit_entrypoints: bool;
    compact_ocaml_heap: bool;
    saved_state: SavedState.t;
    compute_coverage: bool;
    scheduler_policies: SchedulerPolicies.t;
    higher_order_call_graph_max_iterations: int;
    maximum_target_depth: int;
    maximum_parameterized_targets_at_call_site: int option;
  }

  val default_maximum_target_depth : int

  val default_maximum_parameterized_targets_at_call_site : int

  val default_higher_order_call_graph_max_iterations : int

  val default_higher_order_call_graph_max_iterations_in_tests : int

  val create
    :  Analysis.t ->
    maximum_target_depth:int ->
    ?repository_root:PyrePath.t ->
    ?save_results_to:PyrePath.t ->
    ?output_format:TaintOutputFormat.t ->
    ?pyrefly_results:PyrePath.t ->
    ?dump_call_graph:PyrePath.t ->
    ?verify_models:bool ->
    ?verify_dsl:bool ->
    ?verify_taint_config_only:bool ->
    ?rule_filter:int list ->
    ?source_filter:string list ->
    ?sink_filter:string list ->
    ?transform_filter:string list ->
    ?find_missing_flows:MissingFlowKind.t ->
    ?dump_model_query_results:PyrePath.t ->
    ?use_cache:bool ->
    ?build_cache_only:bool ->
    ?infer_self_tito:bool ->
    ?infer_argument_tito:bool ->
    ?maximum_model_source_tree_width:int ->
    ?maximum_model_sink_tree_width:int ->
    ?maximum_model_tito_tree_width:int ->
    ?maximum_tree_depth_after_widening:int ->
    ?maximum_return_access_path_width:int ->
    ?maximum_return_access_path_depth_after_widening:int ->
    ?maximum_tito_collapse_depth:int ->
    ?maximum_tito_positions:int ->
    ?maximum_overrides_to_analyze:int ->
    ?maximum_trace_length:int ->
    ?maximum_tito_depth:int ->
    ?maximum_capture_trace_length:int ->
    ?check_invariants:bool ->
    ?limit_entrypoints:bool ->
    ?compact_ocaml_heap:bool ->
    ?saved_state:SavedState.t ->
    ?compute_coverage:bool ->
    ?scheduler_policies:SchedulerPolicies.t ->
    ?higher_order_call_graph_max_iterations:int ->
    ?maximum_parameterized_targets_at_call_site:int option ->
    unit ->
    t
end
