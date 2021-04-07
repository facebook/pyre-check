(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module Buck : sig
  type t = {
    mode: string option;
    isolation_prefix: string option;
    targets: string list;
    (* This is the buck root of the source directory, i.e. output of `buck root`. *)
    source_root: Path.t;
    (* This is the root of directory where built artifacts will be placed. *)
    artifact_root: Path.t;
  }
  [@@deriving sexp, compare, hash, yojson]
end

module SourcePaths : sig
  type t =
    | Simple of SearchPath.t list
    | Buck of Buck.t
  [@@deriving sexp, compare, hash, yojson]
end

module CriticalFile : sig
  type t =
    | BaseName of string
    | Extension of string
    | FullPath of Path.t
  [@@deriving sexp, compare, hash, yojson]

  val matches : path:Path.t -> t -> bool

  val matches_any : path:Path.t -> t list -> bool

  val find : within:Path.t list -> t list -> Path.t option
end

module SavedStateAction : sig
  type t =
    | LoadFromFile of {
        shared_memory_path: Path.t;
        changed_files_path: Path.t option;
      }
    | LoadFromProject of {
        project_name: string;
        project_metadata: string option;
      }
    | SaveToFile of { shared_memory_path: Path.t }
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

type t = {
  (* Source file discovery *)
  source_paths: SourcePaths.t;
  search_paths: SearchPath.t list;
  excludes: string list;
  checked_directory_allowlist: Path.t list;
  checked_directory_blocklist: Path.t list;
  extensions: Configuration.Extension.t list;
  (* Auxiliary paths *)
  log_path: Path.t;
  global_root: Path.t;
  local_root: Path.t option;
  watchman_root: Path.t option;
  taint_model_paths: Path.t list;
  (* Type checking controls *)
  debug: bool;
  strict: bool;
  python_version: PythonVersion.t;
  show_error_traces: bool;
  store_type_check_resolution: bool;
  critical_files: CriticalFile.t list;
  saved_state_action: SavedStateAction.t option;
  (* Parallelism controls *)
  parallel: bool;
  number_of_workers: int;
  (* Logging controls *)
  additional_logging_sections: string list;
  remote_logging: RemoteLogging.t option;
  profiling_output: string option;
  memory_profiling_output: string option;
}
[@@deriving sexp, compare, hash]

val of_yojson : Yojson.Safe.t -> (t, string) Result.t

val to_yojson : t -> Yojson.Safe.t

(* NOTE: This is neither a stateless nor an exception-free operation. *)
val analysis_configuration_of : t -> Configuration.Analysis.t
