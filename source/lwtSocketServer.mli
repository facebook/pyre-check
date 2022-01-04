(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module provides high-level APIs for async server creation and shutdown.

    These APIs are very similar to what {!Lwt_io.establish_server_with_client_address} and
    {!Lwt_io.shutdown_server} do. But instead of doing socket [bind]+[listen]+[accept] all at once,
    we splits [bind]+[listen] from [accept] as a separate step here. This allows the Pyre server to
    bind the socket first, initialize its internal state (which can be quite expensive), and finally
    starting to take client requests. We do not want the state to be initialized before calling
    [bind] because we want to fail fast if another Pyre server is already running. And we do not
    want the state to be initialized after calling [accept] because it will lead to awkward
    programming interfaces where the request handlers have to worry about whether the server state
    has been initialized or not. *)

(** A `[PreparedSocket]` is a socket on which [bind] and [listen] are invoked, but not [accept]. See
    {!Lwt_unix.bind}, {!Lwt_unix.listen}, and {!Lwt_unix.accept}. *)
module PreparedSocket : sig
  type t
  (** Type of prepared socket. *)

  val create_from_address : Unix.sockaddr -> t Lwt.t
  (** Create a prepared socket from a given socket address. *)

  val create_from_path : PyrePath.t -> t Lwt.t
  (** Create a prepared Unix-domain socket with the given path as the location of the socket on the
      file system. This function may attempt to remove defunct socket at the given location, if
      there is any. *)
end

type t
(** Type of a server *)

val establish
  :  f:(Unix.sockaddr -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel -> unit Lwt.t) ->
  PreparedSocket.t ->
  t Lwt.t
(** [establish ~f prepared_socket] creates a server which accepts incoming connections from
    [prepared_socket]. When a client makes a new connection, the server will create two I/O channel
    from the connection and invoke [f client_address (input_channel, output_channel)].

    The server does not block waiting for f to complete: it concurrently tries to accept more client
    connections while f is handling the client.

    When the promise returned by [f] completes, both the connection and the associated I/O channels
    will be automatically closed. [f] should never attempt to close the I/O channels by itself.

    If [f] raises an exception (or the promise it returns fails with an exception), [establish] can
    do nothing with that exception, except pass it to [Lwt.async_exception_hook].

    The returned promise (a [t Lwt.t]) resolves right before the first internal call to [accept]. *)

val shutdown : t -> unit Lwt.t
(** Closes the given server's listening socket. The returned promise resolves when the [close]
    system call completes. This function does not affect the sockets of connections that have
    already been accepted, i.e. passed to [f] by [establish]. *)
