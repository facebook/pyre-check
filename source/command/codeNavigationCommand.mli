(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CodeNavigationConfiguration : sig
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    socket_path: PyrePath.t;
    additional_logging_sections: string list;
    watchman_root: PyrePath.t option;
    critical_files: Server.CriticalFile.t list;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Core.Command.t
