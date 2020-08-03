(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Core
open Pyre

module Setting : sig
  type t = {
    watchman_root: Path.t;
    watchman_filter: Watchman.Filter.t;
    watchman_connection: Watchman.Raw.Connection.t;
    project_name: string;
    project_metadata: string option;
    critical_files: ServerConfiguration.CriticalFiles.t;
    target: Path.t;
  }
end

module Queried : sig
  type t = {
    bucket: string;
    path: string;
    target: Path.t;
    changed_files: Path.t list;
    (* For logging purpose only *)
    commit_id: string option;
  }
  [@@deriving sexp, compare]
end

module Fetched : sig
  type t = {
    path: Path.t;
    changed_files: Path.t list;
  }
  [@@deriving sexp, compare]
end

val query_exn : Setting.t -> Queried.t Lwt.t

val query : Setting.t -> (Queried.t, string) Result.t Lwt.t

val fetch_exn : Queried.t -> Fetched.t Lwt.t

val fetch : Queried.t -> (Fetched.t, string) Result.t Lwt.t

val query_and_fetch_exn : Setting.t -> Fetched.t Lwt.t

val query_and_fetch : Setting.t -> (Fetched.t, string) Result.t Lwt.t
