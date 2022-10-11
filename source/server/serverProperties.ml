(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This structure holds all the global states of a given server that are immutable throughout the
   server's lifetime. *)
type t = {
  socket_path: PyrePath.t;
  configuration: Configuration.Analysis.t;
  critical_files: CriticalFile.t list;
}

let create ~socket_path ~configuration ~critical_files () =
  { socket_path; critical_files; configuration }
