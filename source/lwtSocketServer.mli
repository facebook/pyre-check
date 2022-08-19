(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module provides high-level APIs for async server creation and shutdown. *)

module SocketAddress : sig
  type t
  (** Type of prepared socket. *)

  val create_from_address : Unix.sockaddr -> t
  (** Create a socket address from a [Unix.sockaddr] directly. *)

  val create_from_path : PyrePath.t -> t
  (** Create a Unix-domain socket with the given path as the location of the socket on the file
      system. This function may attempt to remove defunct socket at the given location, if there is
      any. *)
end

type t
(** Type of a server *)

val establish
  :  handle_connection:
       (Unix.sockaddr -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel -> unit Lwt.t) ->
  SocketAddress.t ->
  t Lwt.t
(** A thin wrapper around {!Lwt_io.establish_server_with_client_address}, accepting
    {!SocketAddress.t} instead of {!Unix.sockaddr} directly. *)

val shutdown : t -> unit Lwt.t
(** A thin wrapper around {!Lwt_io.shutdown_server}. *)

val with_server
  :  f:(unit -> 'a Lwt.t) ->
  handle_connection:
    (Unix.sockaddr -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel -> unit Lwt.t) ->
  SocketAddress.t ->
  'a Lwt.t
(** [with_server ~f ~handle_connection address] executes [establish ~handle_connection address] to
    establish a server first, then invoke [f], and finally call {!shutdown} to shutdown the
    established server before returning the output value of [f]. Shutdown is guaranteed to occur
    regardless of whether [f] gets resolved or rejected. *)
