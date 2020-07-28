(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

exception ConnectionError of string

module Raw : sig
  module Response : sig
    type t =
      | Ok of Yojson.Safe.t
      | EndOfStream
      | Error of string
  end

  module Connection : sig
    type t

    val send : t -> Yojson.Safe.t -> unit Lwt.t

    val receive : t -> unit -> Response.t Lwt.t
  end

  type t

  (* Spawning a watchman background service by shelling out to `watchman get-sockname`. Raise
     `ConnectionError` if `watchman get-sockname` fails to return 0. *)
  val create_exn : unit -> t Lwt.t

  (* Same as `create_exn` but returns `Result.t` rather than throwing. *)
  val create : unit -> (t, string) Core.Result.t Lwt.t

  (* This API is useful for testing. It allows its client to competely mock out watchman behavior by
     specifying what to return when `Connection.send` and `Connection.receive` is invoked. Note that
     returning `Some json` in `receive` corresponds to returning `Response.Ok json` in
     `Connection.receive`, and returning `None` in `receive` corresponds to returning
     `Response.EndOfStream` in `Connection.receive`. *)
  val create_for_testing
    :  send:(Yojson.Safe.t -> unit Lwt.t) ->
    receive:(unit -> Yojson.Safe.t option Lwt.t) ->
    unit ->
    t

  (* Establish a socket connection with the watchman background service. *)
  val open_connection : t -> Connection.t Lwt.t

  (* Shutdown the given socket connection with the watchman background service. *)
  val shutdown_connection : Connection.t -> unit Lwt.t

  (* Establish a socket connection with the watchman background service, and invoke `f` if the
     connection can be successfully established. The connection will be automatically shutdown after
     the promise returned by `f` either gets fulfilled or rejected. *)
  val with_connection : f:(Connection.t -> 'a Lwt.t) -> t -> 'a Lwt.t
end
