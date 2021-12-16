(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Buck : sig
  type t = {
    mode: string option;
    isolation_prefix: string option;
    targets: string list;
    (* This is the buck root of the source directory, i.e. output of `buck root`. *)
    source_root: PyrePath.t;
    (* This is the root of directory where built artifacts will be placed. *)
    artifact_root: PyrePath.t;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module SourcePaths : sig
  type t =
    | Simple of SearchPath.t list
    | Buck of Buck.t
  [@@deriving sexp, compare, hash, yojson]
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
  [@@deriving sexp, compare, hash, yojson]

  val default : t
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

  val default_constraint_solving_style : constraint_solving_style

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
    excludes: Str.regexp list;
    extensions: Extension.t list;
    store_type_check_resolution: bool;
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

  val create
    :  ?parallel:bool ->
    ?analyze_external_sources:bool ->
    ?filter_directories:PyrePath.t list ->
    ?ignore_all_errors:PyrePath.t list ->
    ?number_of_workers:int ->
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
    ?incremental_style:incremental_style ->
    ?log_directory:string ->
    ?python_major_version:int ->
    ?python_minor_version:int ->
    ?python_micro_version:int ->
    ?shared_memory_heap_size:int ->
    ?shared_memory_dependency_table_power:int ->
    ?shared_memory_hash_table_power:int ->
    ?enable_type_comments:bool ->
    ?constraint_solving_style:constraint_solving_style ->
    source_paths:SearchPath.t list ->
    unit ->
    t

  val log_directory : t -> PyrePath.t

  val search_paths : t -> SearchPath.t list

  val extension_suffixes : t -> string list

  val find_extension : t -> PyrePath.t -> Extension.t option
end

module Server : sig
  type load_parameters = {
    shared_memory_path: PyrePath.t;
    changed_files_path: PyrePath.t option;
  }

  type load =
    | LoadFromFiles of load_parameters
    | LoadFromProject of {
        project_name: string;
        metadata: string option;
      }

  type saved_state_action =
    | Save of string
    | Load of load

  type socket_path = {
    path: PyrePath.t;
    (* actual path to socket (OCaml has limits on socket path length) *)
    link: PyrePath.t; (* symbolic link to path in pyre directory *)
  }

  type t = {
    (* Server-specific configuration options *)
    socket: socket_path;
    json_socket: socket_path;
    adapter_socket: socket_path;
    lock_path: PyrePath.t;
    pid_path: PyrePath.t;
    log_path: PyrePath.t;
    daemonize: bool;
    saved_state_action: saved_state_action option;
    (* Analysis configuration *)
    configuration: Analysis.t;
  }

  val set_global : t -> unit

  val get_global : unit -> t option
end

module StaticAnalysis : sig
  type t = {
    (* A directory to write files in. *)
    result_json_path: PyrePath.t option;
    dump_call_graph: PyrePath.t option;
    verify_models: bool;
    (* Analysis configuration *)
    configuration: Analysis.t;
    rule_filter: int list option;
    find_missing_flows: string option;
    dump_model_query_results: PyrePath.t option;
    use_cache: bool;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
  }
end
