(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module Make (Socket : sig
  val write : Unix.File_descr.t -> Protocol.response -> unit

  val close : Unix.File_descr.t -> unit
end) : sig
  val write_to_persistent_client
    :  state:State.t ->
    socket:Network.Socket.t ->
    response:Protocol.response ->
    unit

  val broadcast_response : state:State.t -> response:Protocol.response -> unit
end
