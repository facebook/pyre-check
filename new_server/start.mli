(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

(* Start the server from a given configuration. Then invoke `on_started state` if the server starts
   successfully. *)
(* If the server fails to start, or if an exception is raised from `on_started`, invoke
   `on_exception raised_exception`. *)
(* The server will be automatically shut down after the promise returned by either `on_started` or
   `on_exception` fulfills. *)
val start_server
  :  ?watchman:Watchman.Raw.t ->
  on_started:(ServerState.t ref -> unit Lwt.t) ->
  on_exception:(exn -> unit Lwt.t) ->
  ServerConfiguration.t ->
  unit Lwt.t

(* Start the server and blocks forever until exceptional events occur. Returns immediately when the
   server fails to start. *)
val start_server_and_wait : ServerConfiguration.t -> unit Lwt.t
