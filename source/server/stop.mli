(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Representing the reason why a server needs to stop *)
module Reason : sig
  type t =
    | ExplicitRequest
    | CriticalFileUpdate of PyrePath.t
    | UncaughtException of exn

  val origin_of_exception : exn -> string

  val name_of : t -> string

  val message_of : t -> string
end

(* Reason for the last server stop needs to be stored in a global state, as there is no direct way
   of passing the information across server stop otherwise: standard Unix signal APIs do not allow
   attaching additional information to SIGINT. *)
val get_last_server_stop_reason : unit -> Reason.t option

(* If a server has been set up and waiting with `Start.start_server_and_wait`, This function will
   stop the waiting and terminate the process.

   Note that this function only works with servers that are started via
   `Start.start_server_and_wait`. If we just do `Start.start_server` without installing an Lwt
   signal handler for SIGINT, then this function is not going to have any effect.

   Each server stop must be justified by a reason. After the SIGINT gets caught elsewhere in the
   program, it can retrieve the reason why the signal was sent via `get_last_server_stop_reason`. *)
val stop_waiting_server : Reason.t -> 'a Lwt.t

(* Perform logging for a server stop event *)
val log_stopped_server : reason:string -> start_time:Timer.t -> unit -> unit
