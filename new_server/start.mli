(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Start the server from a given configuration. Then invoke `on_started` if the server starts and
   its state fully initialized. *)
(* If `on_server_socket_ready` is provided, it will be invoked right after the server socket gets
   established and just before the server state initialization takes place. The argument passed to
   `on_server_socket_ready` is the path to the socket file. *)
(* If the server fails to start, or if an exception is raised from `on_started`, invoke
   `on_exception raised_exception`. *)
(* The server will be automatically shut down after the promise returned by either `on_started` or
   `on_exception` fulfills. *)
val start_server
  :  ?watchman:Watchman.Raw.t ->
  ?on_server_socket_ready:(Pyre.Path.t -> unit Lwt.t) ->
  on_started:(ServerState.t ref -> unit Lwt.t) ->
  on_exception:(exn -> unit Lwt.t) ->
  ServerConfiguration.t ->
  unit Lwt.t

(* Start the server and blocks forever until exceptional events occur. Returns immediately when the
   server fails to start. *)
val start_server_and_wait : ServerConfiguration.t -> unit Lwt.t
