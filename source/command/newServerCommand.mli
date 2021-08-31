(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ServerConfiguration : sig
  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    strict: bool;
    show_error_traces: bool;
    additional_logging_sections: string list;
    watchman_root: PyrePath.t option;
    taint_model_paths: PyrePath.t list;
    store_type_check_resolution: bool;
    critical_files: Newserver.CriticalFile.t list;
    saved_state_action: Newserver.SavedStateAction.t option;
  }
  [@@deriving sexp, compare, hash, of_yojson]
end

val command : Command.t
