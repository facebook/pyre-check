(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = {
  source_paths: Configuration.SourcePaths.t;
  socket_path: PyrePath.t;
  watchman_root: PyrePath.t option;
  critical_files: CriticalFile.t list;
  saved_state_action: SavedStateAction.t option;
  skip_initial_type_check: bool;
}
[@@deriving sexp, compare]
