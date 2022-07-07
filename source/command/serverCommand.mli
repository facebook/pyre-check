(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ServerConfiguration : sig
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    socket_path: PyrePath.t;
    strict: bool;
    show_error_traces: bool;
    additional_logging_sections: string list;
    watchman_root: PyrePath.t option;
    taint_model_paths: PyrePath.t list;
    store_type_check_resolution: bool;
    critical_files: Server.CriticalFile.t list;
    saved_state_action: Server.SavedStateAction.t option;
    skip_initial_type_check: bool;
  }
  [@@deriving sexp, compare, hash, of_yojson]
end

val command : Command.t
