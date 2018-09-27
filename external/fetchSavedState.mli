(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Pyre

type fetched_saved_state = {
  saved_state_path: Path.t;
  changed_files: Path.t list;
}

(* Exposed for testing. *)
val saved_state_query: watchman_root: Path.t -> project_name: string -> Yojson.Safe.json

val load
  :  watchman_root: Path.t
  -> project_name: string
  -> version: string
  -> target_path: Path.t
  -> fetched_saved_state option
