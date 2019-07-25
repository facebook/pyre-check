(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

type fetched_saved_state = {
  saved_state_path: Path.t;
  changed_files: Path.t list option;
}

let saved_state_query ~watchman_root:_ ~project_name:_ = `Assoc []

let load
    ~watchman_root:_
    ~project_name:_
    ~project_metadata:_
    ~configuration_file_hash:_
    ~version:_
    ~target_path:_
  =
  None
