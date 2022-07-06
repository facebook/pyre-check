(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    use_buck2: bool;
    targets: string list;
    (* This is the buck root of the source directory, i.e. output of `buck root`. *)
    source_root: PyrePath.t;
    (* This is the root of directory where built artifacts will be placed. *)
    artifact_root: PyrePath.t;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open JsonParsing in
    try
      let mode = optional_string_member "mode" json in
      let isolation_prefix = optional_string_member "isolation_prefix" json in
      let use_buck2 = bool_member ~default:false "use_buck2" json in
      let targets = string_list_member "targets" json ~default:[] in
      let source_root = path_member "source_root" json in
      let artifact_root = path_member "artifact_root" json in
      Result.Ok { mode; isolation_prefix; use_buck2; targets; source_root; artifact_root }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let to_yojson { mode; isolation_prefix; use_buck2; targets; source_root; artifact_root } =
    let result =
      [
        "use_buck2", `Bool use_buck2;
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
    `Assoc result
end

module ChangeIndicator = struct
  type t = {
    root: PyrePath.t;
    relative: string;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open JsonParsing in
    try
      let root = path_member "root" json in
      let relative = string_member "relative" json in
      Result.Ok { root; relative }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


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
    let open JsonParsing in
    try
      let root = path_member "root" json in
      let checksum_path = string_member "checksum_path" json in
      Result.Ok { root; checksum_path }
    with
    | Yojson.Safe.Util.Type_error (message, _)
    | Yojson.Safe.Util.Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


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
    | Buck { artifact_root; _ } -> [SearchPath.Root artifact_root]
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
  [@@deriving sexp, compare, hash, yojson]

  let default =
    {
      major = default_python_major_version;
      minor = default_python_minor_version;
      micro = default_python_micro_version;
    }
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
    let open JsonParsing in
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
  type incremental_style =
    | Shallow
    | FineGrained
  [@@deriving show]

  type shared_memory = {
    heap_size: int;
    dependency_table_power: int;
    hash_table_power: int;
  }
  [@@deriving show]

  type constraint_solving_style =
    | FunctionCallLevel
    | ExpressionLevel
  [@@deriving show]

  let default_constraint_solving_style = FunctionCallLevel

  type t = {
    parallel: bool;
    analyze_external_sources: bool;
    filter_directories: PyrePath.t list option;
    ignore_all_errors: PyrePath.t list option;
    number_of_workers: int;
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
    incremental_style: incremental_style;
    log_directory: PyrePath.t;
    python_major_version: int;
    python_minor_version: int;
    python_micro_version: int;
    shared_memory: shared_memory;
    enable_type_comments: bool;
    constraint_solving_style: constraint_solving_style;
  }
  [@@deriving show]

  let create
      ?(parallel = true)
      ?(analyze_external_sources = false)
      ?filter_directories
      ?ignore_all_errors
      ?(number_of_workers = 4)
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
      ?(incremental_style = Shallow)
      ?log_directory
      ?(python_major_version = default_python_major_version)
      ?(python_minor_version = default_python_minor_version)
      ?(python_micro_version = default_python_micro_version)
      ?(shared_memory_heap_size = default_shared_memory_heap_size)
      ?(shared_memory_dependency_table_power = default_shared_memory_dependency_table_power)
      ?(shared_memory_hash_table_power = default_shared_memory_hash_table_power)
      ?(enable_type_comments = true)
      ?(constraint_solving_style = default_constraint_solving_style)
      ~source_paths
      ()
    =
    {
      parallel;
      analyze_external_sources;
      filter_directories;
      ignore_all_errors;
      number_of_workers;
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
      incremental_style;
      log_directory =
        (match log_directory with
        | Some directory -> PyrePath.create_absolute directory
        | None -> PyrePath.append local_root ~element:".pyre");
      python_major_version;
      python_minor_version;
      python_micro_version;
      shared_memory =
        {
          heap_size = shared_memory_heap_size;
          dependency_table_power = shared_memory_dependency_table_power;
          hash_table_power = shared_memory_hash_table_power;
        };
      enable_type_comments;
      constraint_solving_style;
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
      if not (PyrePath.is_directory directory) then
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

module StaticAnalysis = struct
  type t = {
    repository_root: PyrePath.t option;
    result_json_path: PyrePath.t option;
    dump_call_graph: PyrePath.t option;
    verify_models: bool;
    verify_dsl: bool;
    (* Analysis configuration *)
    configuration: Analysis.t;
    rule_filter: int list option;
    find_missing_flows: string option;
    dump_model_query_results: PyrePath.t option;
    use_cache: bool;
    inline_decorators: bool;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
  }

  let create
      configuration
      ?repository_root
      ?result_json_path
      ?dump_call_graph
      ?(verify_models = true)
      ?(verify_dsl = true)
      ?rule_filter
      ?find_missing_flows
      ?dump_model_query_results
      ?(use_cache = false)
      ?(inline_decorators = true)
      ?maximum_trace_length
      ?maximum_tito_depth
      ()
    =
    {
      repository_root;
      result_json_path;
      dump_call_graph;
      verify_models;
      verify_dsl;
      configuration;
      rule_filter;
      find_missing_flows;
      dump_model_query_results;
      use_cache;
      inline_decorators;
      maximum_trace_length;
      maximum_tito_depth;
    }
end
