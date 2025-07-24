(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T170743593) new warning with ppx_conv_sexp.v0.16.X *)
[@@@warning "-name-out-of-scope"]

(* This module supports the loading of Pyre configaration from global .pyre_configuration files with
   local overrides defined within .pyre_configuration.local files. Some additional utility functions
   are provided *)

open Core

let default_python_major_version = 3

let default_python_minor_version = 10

let default_python_micro_version = 10

let default_shared_memory_heap_size = 8 * 1024 * 1024 * 1024 (* 8 GiB *)

let default_shared_memory_dependency_table_power = 27

let default_shared_memory_hash_table_power = 26

module Buck = struct
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
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open JsonParsing.YojsonUtils in
    let open Yojson.Safe.Util in
    try
      let mode = optional_string_member "mode" json in
      let isolation_prefix = optional_string_member "isolation_prefix" json in
      let bxl_builder = optional_string_member "bxl_builder" json in
      let targets = string_list_member "targets" json ~default:[] in
      let source_root = path_member "source_root" json in
      let artifact_root = path_member "artifact_root" json in
      let targets_fallback_sources =
        optional_list_member
          "targets_fallback_sources"
          ~f:(fun json -> to_string json |> SearchPath.create)
          json
      in
      let kill_buck_after_build = bool_member "kill_buck_after_build" ~default:false json in
      let number_of_threads = optional_int_member "number_of_threads" json in
      Result.Ok
        {
          mode;
          isolation_prefix;
          bxl_builder;
          targets;
          source_root;
          artifact_root;
          targets_fallback_sources;
          kill_buck_after_build;
          number_of_threads;
        }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)


  let to_yojson
      {
        mode;
        isolation_prefix;
        bxl_builder;
        targets;
        source_root;
        artifact_root;
        targets_fallback_sources;
        kill_buck_after_build;
        number_of_threads;
      }
    =
    let result =
      [
        "targets", `List (List.map targets ~f:(fun target -> `String target));
        "source_root", `String (PyrePath.absolute source_root);
        "artifact_root", `String (PyrePath.absolute artifact_root);
      ]
    in
    let result =
      match mode with
      | None -> result
      | Some mode -> ("mode", `String mode) :: result
    in
    let result =
      match isolation_prefix with
      | None -> result
      | Some isolation_prefix -> ("isolation_prefix", `String isolation_prefix) :: result
    in
    let result =
      match bxl_builder with
      | None -> result
      | Some bxl_builder -> ("bxl_builder", `String bxl_builder) :: result
    in
    let result =
      match number_of_threads with
      | None -> result
      | Some number_of_threads -> ("number_of_threads", `Int number_of_threads) :: result
    in
    let result =
      match targets_fallback_sources with
      | None -> result
      | Some sources ->
          ( "targets_fallback_sources",
            [%to_yojson: string list] (List.map sources ~f:SearchPath.show) )
          :: ("kill_buck_after_build", `Bool kill_buck_after_build)
          :: result
    in
    `Assoc result
end

module ChangeIndicator = struct
  type t = {
    root: PyrePath.t;
    relative: string;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open JsonParsing.YojsonUtils in
    try
      let root = path_member "root" json in
      let relative = string_member "relative" json in
      Result.Ok { root; relative }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)


  let to_yojson { root; relative } =
    `Assoc ["root", `String (PyrePath.absolute root); "relative", `String relative]


  let to_path { root; relative } = PyrePath.create_relative ~root ~relative
end

module UnwatchedFiles = struct
  type t = {
    root: PyrePath.t;
    checksum_path: string;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open JsonParsing.YojsonUtils in
    try
      let root = path_member "root" json in
      let checksum_path = string_member "checksum_path" json in
      Result.Ok { root; checksum_path }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)


  let to_yojson { root; checksum_path } =
    `Assoc ["root", `String (PyrePath.absolute root); "checksum_path", `String checksum_path]
end

module UnwatchedDependency = struct
  type t = {
    change_indicator: ChangeIndicator.t;
    files: UnwatchedFiles.t;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module SourcePaths = struct
  type t =
    | Simple of SearchPath.t list
    | WithUnwatchedDependency of {
        sources: SearchPath.t list;
        unwatched_dependency: UnwatchedDependency.t;
      }
    | Buck of Buck.t
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let parsing_failed () =
      let message = Format.sprintf "Malformed source path JSON: %s" (Yojson.Safe.to_string json) in
      Result.Error message
    in
    let parse_search_path_jsons search_path_jsons =
      try
        Result.Ok (List.map search_path_jsons ~f:(fun json -> to_string json |> SearchPath.create))
      with
      | Type_error _ -> parsing_failed ()
    in
    let open Result in
    match json with
    | `List search_path_jsons ->
        (* Recognize this as a shortcut for simple source paths. *)
        parse_search_path_jsons search_path_jsons
        >>= fun search_paths -> Result.Ok (Simple search_paths)
    | `Assoc _ -> (
        match member "kind" json with
        | `String "simple" -> (
            match member "paths" json with
            | `List search_path_jsons ->
                parse_search_path_jsons search_path_jsons
                >>= fun search_paths -> Result.Ok (Simple search_paths)
            | _ -> parsing_failed ())
        | `String "buck" -> (
            match Buck.of_yojson json with
            | Result.Ok buck -> Result.Ok (Buck buck)
            | Result.Error error -> Result.Error error)
        | `String "with_unwatched_dependency" -> (
            match member "paths" json, member "unwatched_dependency" json with
            | `List search_path_jsons, (`Assoc _ as unwatched_dependency_json) ->
                parse_search_path_jsons search_path_jsons
                >>= fun sources ->
                UnwatchedDependency.of_yojson unwatched_dependency_json
                >>= fun unwatched_dependency ->
                Result.Ok (WithUnwatchedDependency { sources; unwatched_dependency })
            | _, _ -> parsing_failed ())
        | _ -> parsing_failed ())
    | _ -> parsing_failed ()


  let to_yojson = function
    | Simple search_paths ->
        `Assoc
          [
            "kind", `String "simple";
            "paths", [%to_yojson: string list] (List.map search_paths ~f:SearchPath.show);
          ]
    | WithUnwatchedDependency { sources; unwatched_dependency } ->
        `Assoc
          [
            "kind", `String "with_unwatched_dependency";
            "paths", [%to_yojson: string list] (List.map sources ~f:SearchPath.show);
            "unwatched_dependency", UnwatchedDependency.to_yojson unwatched_dependency;
          ]
    | Buck buck -> Buck.to_yojson buck |> Yojson.Safe.Util.combine (`Assoc ["kind", `String "buck"])


  let to_search_paths = function
    | Simple sources
    | WithUnwatchedDependency { sources; _ } ->
        sources
    | Buck { Buck.artifact_root; targets_fallback_sources; _ } ->
        SearchPath.Root artifact_root :: Option.value ~default:[] targets_fallback_sources
end

module RemoteLogging = struct
  type t = {
    logger: string;
    identifier: (string[@default ""]);
  }
  [@@deriving sexp, compare, hash, yojson]
end

module PythonVersion = struct
  type t = {
    major: int;
    minor: int;
    micro: int;
  }
  [@@deriving sexp, compare, hash, yojson, equal]

  let create
      ?(major = default_python_major_version)
      ?(minor = default_python_minor_version)
      ?(micro = default_python_micro_version)
      ()
    =
    { major; minor; micro }
end

module SharedMemory = struct
  type t = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving sexp, compare, hash, yojson]

  let default =
    {
      heap_size = default_shared_memory_heap_size;
      dependency_table_power = default_shared_memory_dependency_table_power;
      hash_table_power = default_shared_memory_hash_table_power;
    }


  let of_yojson json =
    let open JsonParsing.YojsonUtils in
    Ok
      {
        heap_size = int_member "heap_size" ~default:default.heap_size json;
        dependency_table_power =
          int_member "dependency_table_power" ~default:default.dependency_table_power json;
        hash_table_power = int_member "hash_table_power" ~default:default.hash_table_power json;
      }
end

module Extension = struct
  type t = {
    suffix: string;
    include_suffix_in_module_qualifier: bool;
  }
  [@@deriving show, sexp, compare, hash]

  let to_yojson { suffix; include_suffix_in_module_qualifier } =
    `Assoc
      [
        "suffix", `String suffix;
        "include_suffix_in_module_qualifier", `Bool include_suffix_in_module_qualifier;
      ]


  let of_yojson = function
    | `Assoc
        [
          ("suffix", `String suffix);
          ("include_suffix_in_module_qualifier", `Bool include_suffix_in_module_qualifier);
        ] ->
        Result.Ok { suffix; include_suffix_in_module_qualifier }
    | _ as json ->
        let message =
          Format.sprintf "Malformed critical file JSON: %s" (Yojson.Safe.to_string json)
        in
        Result.Error message


  let create_extension serialized =
    match String.split serialized ~on:'$' with
    | [suffix] -> { suffix; include_suffix_in_module_qualifier = false }
    | [suffix; options] when String.equal options "include_suffix_in_module_qualifier" ->
        { suffix; include_suffix_in_module_qualifier = true }
    | _ -> failwith (Format.asprintf "Unable to parse extension from %s" serialized)


  let suffix { suffix; _ } = suffix
end

module Analysis = struct
  type shared_memory = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving show]

  let default_enable_readonly_analysis = false

  let default_enable_strict_override_check = false

  let default_enable_strict_any_check = false

  let default_enable_unawaited_awaitable_analysis = false

  let default_include_suppressed_errors = false

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
    excludes: Str.regexp list; [@opaque]
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

  let create
      ?(parallel = true)
      ?(analyze_external_sources = false)
      ?filter_directories
      ?ignore_all_errors
      ?(number_of_workers = 4)
      ?(long_lived_workers = false)
      ?(local_root = PyrePath.current_working_directory ())
      ?(project_root = PyrePath.create_absolute "/")
      ?(search_paths = [])
      ?(taint_model_paths = [])
      ?(strict = false)
      ?(debug = false)
      ?(show_error_traces = false)
      ?(excludes = [])
      ?(extensions = [])
      ?(store_type_check_resolution = true)
      ?(store_type_errors = true)
      ?(track_dependencies = false)
      ?log_directory
      ?(python_version = PythonVersion.create ())
      ?(system_platform = None)
      ?(shared_memory_heap_size = default_shared_memory_heap_size)
      ?(shared_memory_dependency_table_power_from_configuration =
        default_shared_memory_dependency_table_power)
      ?(shared_memory_hash_table_power = default_shared_memory_hash_table_power)
      ?(enable_type_comments = true)
      ?(enable_readonly_analysis = default_enable_readonly_analysis)
      ?(enable_strict_override_check = default_enable_strict_override_check)
      ?(enable_strict_any_check = default_enable_strict_any_check)
      ?(enable_unawaited_awaitable_analysis = default_enable_unawaited_awaitable_analysis)
      ?(include_suppressed_errors = default_include_suppressed_errors)
      ~source_paths
      ()
    =
    (* The shared memory dependency table power varies by project, so it lives in the
       .pyre_configuration. But the decision of whether or not to track dependencies at all is a
       case-by-case choice in Ocaml-side entrypoints. As a result, we override the value coming from
       configuration *)
    let shared_memory_dependency_table_power =
      if track_dependencies then
        shared_memory_dependency_table_power_from_configuration
      else
        1
    in
    {
      parallel;
      analyze_external_sources;
      filter_directories;
      ignore_all_errors;
      number_of_workers;
      long_lived_workers;
      local_root;
      debug;
      project_root;
      source_paths;
      search_paths;
      taint_model_paths;
      strict;
      show_error_traces;
      excludes =
        List.map excludes ~f:(fun exclude_regex ->
            Str.global_substitute
              (Str.regexp_string "${SOURCE_DIRECTORY}")
              (fun _ -> PyrePath.absolute local_root)
              exclude_regex
            |> Str.regexp);
      extensions;
      store_type_check_resolution;
      store_type_errors;
      track_dependencies;
      log_directory =
        (match log_directory with
        | Some directory -> PyrePath.create_absolute directory
        | None -> PyrePath.append local_root ~element:".pyre");
      python_version;
      system_platform;
      shared_memory =
        {
          heap_size = shared_memory_heap_size;
          dependency_table_power = shared_memory_dependency_table_power;
          hash_table_power = shared_memory_hash_table_power;
        };
      enable_type_comments;
      enable_readonly_analysis;
      enable_strict_override_check;
      enable_strict_any_check;
      enable_unawaited_awaitable_analysis;
      include_suppressed_errors;
    }


  let log_directory { log_directory; _ } = log_directory

  let search_paths { source_paths; search_paths; _ } =
    (* Have an ordering of search_path > source_path with the parser. search_path precedes
     * local_root due to the possibility of having a subdirectory of the root in the search path. *)
    search_paths @ source_paths


  let extension_suffixes { extensions; _ } = List.map ~f:Extension.suffix extensions

  let find_extension { extensions; _ } path =
    List.find extensions ~f:(fun extension ->
        String.is_suffix
          ~suffix:(Extension.suffix extension)
          (ArtifactPath.raw path |> PyrePath.absolute))


  let validate_paths { project_root; source_paths; search_paths; _ } =
    let check_directory_exists directory =
      if not (PyrePath.directory_exists directory) then
        raise (Invalid_argument (Format.asprintf "`%a` is not a directory" PyrePath.pp directory))
    in
    let check_path_exists path =
      if not (PyrePath.file_exists path) then
        raise (Invalid_argument (Format.asprintf "`%a` is not a valid path" PyrePath.pp path))
    in
    let check_search_path_exists search_path =
      match search_path with
      | SearchPath.Root _
      | SearchPath.Subdirectory _ ->
          check_directory_exists (SearchPath.to_path search_path)
      | SearchPath.Submodule _ -> check_path_exists (SearchPath.to_path search_path)
    in
    source_paths |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
    check_directory_exists project_root;
    search_paths |> List.iter ~f:check_search_path_exists;
    ()
end

(* Represents a scheduler policy, which can be converted into a `Scheduler.Policy` that can be used
   by `Scheduler.map_reduce`. *)
module SchedulerPolicy = struct
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

  let of_yojson json =
    let open JsonParsing.YojsonUtils in
    try
      let positive_int_member key json =
        let value = int_member key json in
        if value < 1 then
          raise
            (Yojson.Safe.Util.Type_error
               (Format.sprintf "Expected positive integer, got %d" value, json))
        else
          value
      in
      let optional_positive_int_member key json =
        let value = optional_int_member key json in
        match value with
        | Some value when value < 1 ->
            raise
              (Yojson.Safe.Util.Type_error
                 (Format.sprintf "Expected positive integer, got %d" value, json))
        | _ -> value
      in
      let kind = string_member "kind" json in
      match kind with
      | "fixed_chunk_size" ->
          let minimum_chunk_size = optional_positive_int_member "minimum_chunk_size" json in
          let minimum_chunks_per_worker = positive_int_member "minimum_chunks_per_worker" json in
          let preferred_chunk_size = positive_int_member "preferred_chunk_size" json in
          Result.Ok
            (FixedChunkSize { minimum_chunk_size; minimum_chunks_per_worker; preferred_chunk_size })
      | "fixed_chunk_count" ->
          let minimum_chunks_per_worker = optional_int_member "minimum_chunks_per_worker" json in
          let minimum_chunk_size = positive_int_member "minimum_chunk_size" json in
          let preferred_chunks_per_worker =
            positive_int_member "preferred_chunks_per_worker" json
          in
          Result.Ok
            (FixedChunkCount
               { minimum_chunks_per_worker; minimum_chunk_size; preferred_chunks_per_worker })
      | _ ->
          Result.Error
            (Format.sprintf
               "Invalid scheduler policy: get `%s`, expected: fixed_chunk_size or fixed_chunk_count"
               kind)
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)
end

module ScheduleIdentifier = struct
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
    | ParsePyreflyModuleInfo
    | PyreflyCollectDefinitions
  [@@deriving sexp, compare, hash]

  let of_string = function
    | "collect_definitions" -> Some CollectDefinitions
    | "type_check" -> Some TypeCheck
    | "compute_changed_paths" -> Some ComputeChangedPaths
    | "save_changed_paths" -> Some SaveChangedPaths
    | "taint_fetch_callables" -> Some TaintFetchCallables
    | "class_hierarchy_graph" -> Some ClassHierarchyGraph
    | "callable_model_queries" -> Some CallableModelQueries
    | "attribute_model_queries" -> Some AttributeModelQueries
    | "global_model_queries" -> Some GlobalModelQueries
    | "infer_class_models" -> Some InferClassModels
    | "global_constants" -> Some GlobalConstants
    | "call_graph" -> Some CallGraph
    | "override_graph" -> Some OverrideGraph
    | "taint_fixpoint" -> Some TaintFixpoint
    | "taint_collect_errors" -> Some TaintCollectErrors
    | "taint_file_coverage" -> Some TaintFileCoverage
    | "taint_kind_coverage" -> Some TaintKindCoverage
    | "pyrefly_parse_sources" -> Some PyreflyParseSources
    | "parse_pyrefly_module_info" -> Some ParsePyreflyModuleInfo
    | "pyrefly_collect_definitions" -> Some PyreflyCollectDefinitions
    | _ -> None


  let to_string = function
    | CollectDefinitions -> "collect_definitions"
    | TypeCheck -> "type_check"
    | ComputeChangedPaths -> "compute_changed_paths"
    | SaveChangedPaths -> "save_changed_paths"
    | TaintFetchCallables -> "taint_fetch_callables"
    | MethodKinds -> "method_kinds"
    | ClassHierarchyGraph -> "class_hierarchy_graph"
    | CallableModelQueries -> "callable_model_queries"
    | AttributeModelQueries -> "attribute_model_queries"
    | GlobalModelQueries -> "global_model_queries"
    | InferClassModels -> "infer_class_models"
    | GlobalConstants -> "global_constants"
    | CallGraph -> "call_graph"
    | HigherOrderCallGraph -> "higher_order_call_graph"
    | OverrideGraph -> "override_graph"
    | TaintFixpoint -> "taint_fixpoint"
    | TaintCollectErrors -> "taint_collect_errors"
    | TaintFileCoverage -> "taint_file_coverage"
    | TaintKindCoverage -> "taint_kind_coverage"
    | DecoratorResolution -> "decorator_resolution"
    | CallableToDecoratorsMap -> "callable_to_decorators_map"
    | CallablesSharedMemory -> "callables_to_definitions_map"
    | PyreflyParseSources -> "pyrefly_parse_sources"
    | ParsePyreflyModuleInfo -> "parse_pyrefly_module_info"
    | PyreflyCollectDefinitions -> "pyrefly_collect_definitions"
end

module SchedulerPolicies = struct
  module T = Map.Make (ScheduleIdentifier)

  type t = SchedulerPolicy.t T.t [@@deriving compare]

  let empty = T.empty

  let of_alist_exn = T.of_alist_exn

  let sexp_of_t = T.sexp_of_t SchedulerPolicy.sexp_of_t

  let t_of_sexp = T.t_of_sexp SchedulerPolicy.t_of_sexp

  let hash_fold_t state map =
    map |> Map.to_alist |> [%hash_fold: (ScheduleIdentifier.t * SchedulerPolicy.t) list] state


  let hash = Hash.run hash_fold_t

  let get = Map.find

  let of_yojson json =
    let open Core.Result in
    try
      let parse_item (key, value) =
        match ScheduleIdentifier.of_string key with
        | None -> Result.Error (Format.sprintf "Unknown schedule identifier: `%s`" key)
        | Some identifier ->
            SchedulerPolicy.of_yojson value >>| fun scheduler_policy -> identifier, scheduler_policy
      in
      json
      |> Yojson.Safe.Util.to_assoc
      |> List.map ~f:parse_item
      |> Core.Result.all
      >>| T.of_alist_exn
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)
end

module TaintOutputFormat = struct
  type t =
    | Json
    | ShardedJson
  [@@deriving sexp, compare, hash]

  let of_string = function
    | "json" -> Ok Json
    | "sharded-json" -> Ok ShardedJson
    | output_format -> Error (Format.sprintf "Invalid taint output format `%s`" output_format)
end

module MissingFlowKind = struct
  type t =
    (* Find missing flows through obscure models. *)
    | Obscure
    (* Find missing flows due to missing type information. *)
    | Type
  [@@deriving sexp, compare, equal, hash]

  let of_string = function
    | "obscure" -> Ok Obscure
    | "type" -> Ok Type
    | missing_flow -> Error (Format.sprintf "Invalid missing flow kind `%s`" missing_flow)
end

module StaticAnalysis = struct
  module SavedState = struct
    type t = {
      watchman_root: string option;
      project_name: string option;
      (* The Pysa preset that was used to build the saved state. *)
      preset: string option;
      cache_critical_files: string list;
    }
    [@@deriving sexp, compare, hash, yojson]

    let empty =
      { watchman_root = None; project_name = None; preset = None; cache_critical_files = [] }
  end

  type t = {
    repository_root: PyrePath.t option;
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

  (* Maximum depth for parameterized targets, i.e `foo[x: bar[y: baz]]` has depth 2. We limit the
     depth to prevent infinite recursion. *)
  let default_maximum_target_depth = 10

  (* Maximum number of parameterized targets that can be created at a call site. *)
  let default_maximum_parameterized_targets_at_call_site = 10

  (* Default number of iterations of the call graph fixpoint. *)
  let default_higher_order_call_graph_max_iterations = 50

  let default_higher_order_call_graph_max_iterations_in_tests = 10

  let create
      configuration
      ~maximum_target_depth
      ?repository_root
      ?save_results_to
      ?(output_format = TaintOutputFormat.Json)
      ?pyrefly_results
      ?dump_call_graph
      ?(verify_models = true)
      ?(verify_dsl = true)
      ?(verify_taint_config_only = false)
      ?rule_filter
      ?source_filter
      ?sink_filter
      ?transform_filter
      ?find_missing_flows
      ?dump_model_query_results
      ?(use_cache = false)
      ?(build_cache_only = false)
      ?(infer_self_tito = true)
      ?(infer_argument_tito = false)
      ?maximum_model_source_tree_width
      ?maximum_model_sink_tree_width
      ?maximum_model_tito_tree_width
      ?maximum_tree_depth_after_widening
      ?maximum_return_access_path_width
      ?maximum_return_access_path_depth_after_widening
      ?maximum_tito_collapse_depth
      ?maximum_tito_positions
      ?maximum_overrides_to_analyze
      ?maximum_trace_length
      ?maximum_tito_depth
      ?(check_invariants = false)
      ?(limit_entrypoints = false)
      ?(compact_ocaml_heap = false)
      ?(saved_state = SavedState.empty)
      ?(compute_coverage = false)
      ?(scheduler_policies = SchedulerPolicies.empty)
      ?(higher_order_call_graph_max_iterations = default_higher_order_call_graph_max_iterations)
      ?(maximum_parameterized_targets_at_call_site = None)
      ()
    =
    {
      repository_root;
      save_results_to;
      output_format;
      pyrefly_results;
      dump_call_graph;
      verify_models;
      verify_dsl;
      verify_taint_config_only;
      configuration;
      rule_filter;
      source_filter;
      sink_filter;
      transform_filter;
      find_missing_flows;
      dump_model_query_results;
      use_cache;
      build_cache_only;
      infer_self_tito;
      infer_argument_tito;
      maximum_model_source_tree_width;
      maximum_model_sink_tree_width;
      maximum_model_tito_tree_width;
      maximum_tree_depth_after_widening;
      maximum_return_access_path_width;
      maximum_return_access_path_depth_after_widening;
      maximum_tito_collapse_depth;
      maximum_tito_positions;
      maximum_overrides_to_analyze;
      maximum_trace_length;
      maximum_tito_depth;
      check_invariants;
      limit_entrypoints;
      compact_ocaml_heap;
      saved_state;
      compute_coverage;
      scheduler_policies;
      higher_order_call_graph_max_iterations;
      maximum_target_depth;
      maximum_parameterized_targets_at_call_site;
    }
end
