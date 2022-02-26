(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This structure holds all the global states of a given server that are immutable throughout the
   server's lifetime. *)
type t = {
  start_time: Timer.t;
  socket_path: PyrePath.t;
  configuration: Configuration.Analysis.t;
  critical_files: CriticalFile.t list;
}

let create ?start_time ~socket_path ~configuration ~critical_files () =
  {
    start_time = Option.value start_time ~default:(Timer.start ());
    socket_path;
    critical_files;
    configuration;
  }
