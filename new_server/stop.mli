(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* If a server has been set up and waiting with `Start.start_server_and_wait`, This function will
   stop the waiting and terminate the process. *)
(* Note that this function only works with servers that are started via
   `Start.start_server_and_wait`. If we just do `Start.start_server` without installing an Lwt
   signal handler for SIGINT, then this function is not going to have any effect. *)
val stop_waiting_server : unit -> 'a Lwt.t
