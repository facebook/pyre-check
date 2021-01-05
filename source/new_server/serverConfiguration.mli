(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module CriticalFile : sig
  type t =
    | BaseName of string
    | FullPath of Path.t
  [@@deriving sexp, compare, hash, yojson]

  val base_name_of : t -> string

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
  [@@deriving sexp, compare, hash, yojson]
end

type t = {
  (* Source file discovery *)
  source_paths: SearchPath.t list;
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
  show_error_traces: bool;
  store_type_check_resolution: bool;
  critical_files: CriticalFile.t list;
  saved_state_action: SavedStateAction.t option;
  (* Parallelism controls *)
  parallel: bool;
  number_of_workers: int;
}
[@@deriving sexp, compare, hash]

val of_yojson : Yojson.Safe.t -> (t, string) Result.t

val to_yojson : t -> Yojson.Safe.t

(* NOTE: This is neither a stateless nor an exception-free operation. *)
val analysis_configuration_of : t -> Configuration.Analysis.t
