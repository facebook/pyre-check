(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t = {
  (* Source file discovery *)
  source_paths: Path.t list;
  search_paths: SearchPath.t list;
  excludes: string list;
  checked_directory_allowlist: Path.t list;
  checked_directory_blocklist: Path.t list;
  extensions: string list;
  (* Auxiliary paths *)
  log_path: Path.t;
  global_root: Path.t;
  local_root: Path.t option;
  taint_model_paths: Path.t list;
  (* Type checking controls *)
  debug: bool;
  strict: bool;
  show_error_traces: bool;
  store_type_check_resolution: bool;
  critical_files: string list;
  (* Parallelism controls *)
  parallel: bool;
  number_of_workers: int;
}
[@@deriving sexp, compare, hash]

val of_yojson : Yojson.Safe.t -> (t, string) Result.t

val to_yojson : t -> Yojson.Safe.t

(* NOTE: This is neither a stateless nor an exception-free operation. *)
val analysis_configuration_of : t -> Configuration.Analysis.t
