(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

(* Perform logging for a server stop event *)
val log_stopped_server : reason:string -> start_time:Timer.t -> unit -> unit

(* Convenient wrapper around `log_stopped_server` followed by `stop_waiting_server`. *)
val log_and_stop_waiting_server : reason:string -> properties:ServerProperties.t -> unit -> 'a Lwt.t
