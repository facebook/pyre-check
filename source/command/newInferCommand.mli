(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module InferMode : sig
  type t =
    | Local
    | Interprocedural
  [@@deriving sexp, compare, hash, yojson]
end

module InferConfiguration : sig
  type t = {
    source_paths: Configuration.SourcePaths.t;
    search_paths: SearchPath.t list;
    excludes: string list;
    checked_directory_allowlist: PyrePath.t list;
    checked_directory_blocklist: PyrePath.t list;
    ignore_infer: PyrePath.t list;
    extensions: Configuration.Extension.t list;
    log_path: PyrePath.t;
    global_root: PyrePath.t;
    local_root: PyrePath.t option;
    infer_mode: InferMode.t;
    debug: bool;
    python_version: Configuration.PythonVersion.t;
    parallel: bool;
    number_of_workers: int;
    shared_memory: Configuration.SharedMemory.t;
    remote_logging: Configuration.RemoteLogging.t option;
    profiling_output: string option;
    memory_profiling_output: string option;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Command.t
