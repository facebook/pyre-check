(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ServerConfiguration : sig
  type t = {
    (* Source file discovery *)
    source_paths: Configuration.SourcePaths.t;
    search_paths: SearchPath.t list;
    excludes: string list;
    checked_directory_allowlist: PyrePath.t list;
    checked_directory_blocklist: PyrePath.t list;
    extensions: Configuration.Extension.t list;
    (* Auxiliary paths *)
    log_path: PyrePath.t;
    global_root: PyrePath.t;
    local_root: PyrePath.t option;
    watchman_root: PyrePath.t option;
    taint_model_paths: PyrePath.t list;
    (* Type checking controls *)
    debug: bool;
    strict: bool;
    python_version: Configuration.PythonVersion.t;
    show_error_traces: bool;
    store_type_check_resolution: bool;
    critical_files: Newserver.CriticalFile.t list;
    saved_state_action: Newserver.SavedStateAction.t option;
    (* Parallelism controls *)
    parallel: bool;
    number_of_workers: int;
    (* Memory controls *)
    shared_memory: Configuration.SharedMemory.t;
    (* Logging controls *)
    additional_logging_sections: string list;
    remote_logging: Configuration.RemoteLogging.t option;
    profiling_output: string option;
    memory_profiling_output: string option;
  }
  [@@deriving sexp, compare, hash, yojson]
end

val command : Command.t
