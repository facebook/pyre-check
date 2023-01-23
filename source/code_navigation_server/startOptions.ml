(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base

type t = {
  environment_controls: Analysis.EnvironmentControls.t;
  source_paths: Configuration.SourcePaths.t;
  socket_path: PyrePath.t;
  watchman: Server.StartOptions.Watchman.t option;
  build_system_initializer: BuildSystem.Initializer.t;
  critical_files: Server.CriticalFile.t list;
}
