(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

(* Start the server from a given configuration. *)
(* Then invoke `f None` if the server fails to start and `f (Some state)` if the server starts
   successfully, with `state` being the initial server state. *)
(* The server will be automatically shut down after the promise returned by `f` fulfills. *)
val start_server : f:(ServerState.t ref option -> 'a Lwt.t) -> ServerConfiguration.t -> 'a Lwt.t

(* Start the server and blocks forever until exceptional events occur. Returns immediately when the
   server fails to start. *)
val start_server_and_wait : ServerConfiguration.t -> unit Lwt.t
