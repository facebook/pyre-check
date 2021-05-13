(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre

val default_python_major_version : int

val default_python_minor_version : int

val default_python_micro_version : int

module Features : sig
  type t = {
    click_to_fix: bool;
    go_to_definition: bool;
    hover: bool;
  }
  [@@deriving yojson, show]

  val create : string option -> t

  val default : t
end

module Extension : sig
  type t = {
    suffix: string;
    include_suffix_in_module_qualifier: bool;
  }
  [@@deriving show, eq, sexp, compare, hash, yojson]

  val create_extension : string -> t

  val suffix : t -> string
end

module Analysis : sig
  type incremental_style =
    | Shallow
    | FineGrained
  [@@deriving show]

  type t = {
    infer: bool;
    configuration_file_hash: string option;
    parallel: bool;
    analyze_external_sources: bool;
    filter_directories: Path.t list option;
    ignore_all_errors: Path.t list option;
    number_of_workers: int;
    local_root: Path.t;
    debug: bool;
    project_root: Path.t;
    source_path: SearchPath.t list;
    search_path: SearchPath.t list;
    taint_model_paths: Path.t list;
    expected_version: string option;
    strict: bool;
    show_error_traces: bool;
    excludes: Str.regexp list;
    extensions: Extension.t list;
    store_type_check_resolution: bool;
    incremental_style: incremental_style;
    include_hints: bool;
    perform_autocompletion: bool;
    features: Features.t;
    ignore_infer: Path.t list;
    log_directory: Path.t;
    python_major_version: int;
    python_minor_version: int;
    python_micro_version: int;
  }
  [@@deriving show, eq]

  val create
    :  ?infer:bool ->
    ?configuration_file_hash:string ->
    ?parallel:bool ->
    ?analyze_external_sources:bool ->
    ?filter_directories:Path.t list ->
    ?ignore_all_errors:Path.t list ->
    ?number_of_workers:int ->
    ?local_root:Path.t ->
    ?project_root:Path.t ->
    ?search_path:SearchPath.t list ->
    ?taint_model_paths:Path.t list ->
    ?expected_version:string ->
    ?strict:bool ->
    ?debug:bool ->
    ?show_error_traces:bool ->
    ?excludes:string list ->
    ?extensions:Extension.t list ->
    ?store_type_check_resolution:bool ->
    ?incremental_style:incremental_style ->
    ?include_hints:bool ->
    ?perform_autocompletion:bool ->
    ?features:Features.t ->
    ?ignore_infer:Path.t list ->
    ?log_directory:string ->
    ?python_major_version:int ->
    ?python_minor_version:int ->
    ?python_micro_version:int ->
    source_path:SearchPath.t list ->
    unit ->
    t

  val log_directory : t -> Path.t

  val search_path : t -> SearchPath.t list

  val extension_suffixes : t -> string list

  val find_extension : t -> Path.t -> Extension.t option

  val features : t -> Features.t
end

module Server : sig
  type load_parameters = {
    shared_memory_path: Path.t;
    changed_files_path: Path.t option;
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
    path: Path.t;
    (* actual path to socket (OCaml has limits on socket path length) *)
    link: Path.t; (* symbolic link to path in pyre directory *)
  }

  type t = {
    (* Server-specific configuration options *)
    socket: socket_path;
    json_socket: socket_path;
    adapter_socket: socket_path;
    lock_path: Path.t;
    pid_path: Path.t;
    log_path: Path.t;
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
    result_json_path: Path.t option;
    dump_call_graph: bool;
    verify_models: bool;
    (* Analysis configuration *)
    configuration: Analysis.t;
    rule_filter: int list option;
    find_missing_flows: string option;
    dump_model_query_results: bool;
    use_cache: bool;
    maximum_trace_length: int option;
  }

  val to_json : t -> Yojson.Safe.json
end
