(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

module Analysis : sig
  type incremental_style =
    | Shallow
    | Transitive
    | FineGrained
  [@@deriving show]

  type t = {
    start_time: float;
    infer: bool;
    additional_checks: string list;
    configuration_file_hash: string option;
    parallel: bool;
    filter_directories: Path.t list option;
    ignore_all_errors: Path.t list option;
    ignore_dependencies: bool;
    number_of_workers: int;
    local_root: Path.t;
    sections: string list;
    debug: bool;
    project_root: Path.t;
    search_path: SearchPath.t list;
    taint_models_directories: Path.t list;
    verbose: bool;
    expected_version: string option;
    strict: bool;
    declare: bool;
    show_error_traces: bool;
    log_identifier: string;
    logger: string option;
    profiling_output: string option;
    excludes: Str.regexp list;
    extensions: string list;
    store_type_check_resolution: bool;
    incremental_style: incremental_style;
    include_hints: bool;
  }
  [@@deriving show, eq]

  val create
    :  ?start_time:float ->
    ?infer:bool ->
    ?additional_checks:string list ->
    ?configuration_file_hash:string ->
    ?parallel:bool ->
    ?filter_directories:Path.t list ->
    ?ignore_all_errors:Path.t list ->
    ?ignore_dependencies:bool ->
    ?number_of_workers:int ->
    ?local_root:Path.t ->
    ?sections:string list ->
    ?project_root:Path.t ->
    ?search_path:SearchPath.t list ->
    ?taint_models_directories:Path.t list ->
    ?verbose:bool ->
    ?expected_version:string ->
    ?strict:bool ->
    ?declare:bool ->
    ?debug:bool ->
    ?show_error_traces:bool ->
    ?log_identifier:string ->
    ?logger:string ->
    ?profiling_output:string ->
    ?excludes:string list ->
    ?extensions:string list ->
    ?store_type_check_resolution:bool ->
    ?incremental_style:incremental_style ->
    ?include_hints:bool ->
    unit ->
    t

  val set_global : t -> unit

  val get_global : unit -> t option

  val pyre_root : t -> Path.t

  val search_path : t -> SearchPath.t list
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
    (* Analysis configuration *)
    configuration: Analysis.t;
  }
end
