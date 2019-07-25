(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre
open Network

type version_mismatch = {
  server_version: string;
  expected_version: string;
}
[@@deriving show]

exception ServerNotRunning

val socket_path : ?create:bool -> ?name:string -> Configuration.Analysis.t -> Path.t

val create_configuration
  :  ?daemonize:bool ->
  ?log_path:PyrePath.t ->
  ?saved_state_action:Configuration.Server.saved_state_action ->
  Configuration.Analysis.t ->
  Configuration.Server.t

exception ConnectionFailure

exception VersionMismatch of version_mismatch

val start
  :  ?old_state:State.t ->
  connections:State.connections ->
  configuration:Configuration.Server.t ->
  unit ->
  State.t

val stop : reason:string -> configuration:Configuration.Server.t -> 'a

val connect : retries:int -> configuration:Configuration.Analysis.t -> Socket.t
