(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre

module Analysis: sig
  type t = {
    start_time: float;
    infer: bool;
    recursive_infer: bool;
    run_additional_checks: bool;
    parallel: bool;
    filter_directories: (Path.t list) option;
    number_of_workers: int;
    local_root: Path.t;
    sections: string list;
    debug: bool;
    project_root: Path.t;
    search_path: Path.SearchPath.t list;
    typeshed: Path.t option;
    verbose: bool;
    expected_version: string option;
    strict: bool;
    declare: bool;
    show_error_traces: bool;
    log_identifier: string;
    logger: string option;
    excludes: Str.regexp list;
  }
  [@@deriving show, eq]

  val create
    :  ?start_time: float
    -> ?infer: bool
    -> ?recursive_infer: bool
    -> ?run_additional_checks: bool
    -> ?parallel: bool
    -> ?filter_directories: Path.t list
    -> ?number_of_workers: int
    -> ?local_root: Path.t
    -> ?sections: string list
    -> ?project_root: Path.t
    -> ?search_path: Path.SearchPath.t list
    -> ?typeshed: Path.t
    -> ?verbose: bool
    -> ?expected_version: string
    -> ?strict: bool
    -> ?declare: bool
    -> ?debug: bool
    -> ?show_error_traces: bool
    -> ?log_identifier: string
    -> ?logger: string
    -> ?excludes: string list
    -> unit
    -> t

  val set_global: t -> unit
  val get_global: unit -> t option

  val localize: t -> local_debug: bool -> local_strict: bool -> declare: bool -> t

  val pyre_root: t -> Path.t

  val search_path: t -> Path.SearchPath.t list
end

module Server: sig
  type load_parameters = {
    shared_memory_path: Path.t;
    changed_files_path: Path.t;
  }

  type load =
    | LoadFromFiles of load_parameters
    | LoadFromProject of string

  type saved_state_action =
    | Save of string
    | Load of load

  type t = {
    (* Server-specific configuration options *)
    socket_path: Path.t;
    socket_link: Path.t;
    lock_path: Path.t;
    pid_path: Path.t;
    log_path: Path.t;
    daemonize: bool;
    use_watchman: bool;
    watchman_creation_timeout: float;
    saved_state_action: saved_state_action option;
    (* Analysis configuration *)
    configuration: Analysis.t;
  }

  val set_global: t -> unit
  val get_global: unit -> t option
end

module StaticAnalysis: sig
  type t = {
    (* A directory to write files in. *)
    result_json_path: Path.t option;
    dump_call_graph: bool;
    (* Analysis configuration *)
    configuration: Analysis.t;
  }
end
