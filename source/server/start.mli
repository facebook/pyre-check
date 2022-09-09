(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception ServerStopped

exception ServerInterrupted of Core.Signal.t

(* Return a promise which will invoke [on_caught] when given unix signals are received. *)
val wait_for_signal : on_caught:(Core.Signal.t -> 'a Lwt.t) -> Core.Signal.t list -> 'a Lwt.t

val get_optional_watchman_subscriber
  :  critical_files:CriticalFile.t list ->
  extensions:Configuration.Extension.t list ->
  source_paths:Configuration.SourcePaths.t ->
  StartOptions.Watchman.t option ->
  Watchman.Subscriber.t option Lwt.t

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
  :  ?on_server_socket_ready:(PyrePath.t -> unit Lwt.t) ->
  on_started:(ServerProperties.t -> ServerState.t ExclusiveLock.Lazy.t -> 'a Lwt.t) ->
  on_exception:(exn -> 'a Lwt.t) ->
  StartOptions.t ->
  'a Lwt.t
