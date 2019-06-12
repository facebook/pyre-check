(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module type Connections = sig
  val write_to_persistent_client
    :  connections:State.connections ->
    socket:Network.Socket.t ->
    response:Protocol.response ->
    unit

  (* Mutates the state to keep track of the socket. *)
  val add_persistent_client : connections:State.connections -> socket:Network.Socket.t -> unit

  val remove_persistent_client : connections:State.connections -> socket:Network.Socket.t -> unit

  val broadcast_response : connections:State.connections -> response:Protocol.response -> unit

  val add_file_notifier : connections:State.connections -> socket:Network.Socket.t -> unit

  val remove_file_notifier : connections:State.connections -> socket:Network.Socket.t -> unit
end

module Make (Socket : sig
  val write : Unix.File_descr.t -> Protocol.response -> unit

  val close : Unix.File_descr.t -> unit
end) : Connections

module Unix : Connections
