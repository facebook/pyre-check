(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception ConnectionError of string

exception SubscriptionError of string

exception QueryError of string

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

module Filter : sig
  (* Only track changes in files that satisfy any of the following conditions:
   * - Suffix of the file is included in `suffixes`.
   * - File name of the file is included in `base_names`. *)
  type t = {
    base_names: string list;
    whole_names: string list;
    suffixes: string list;
  }
  [@@deriving sexp, compare, hash]

  val from_server_configurations
    :  critical_files:CriticalFile.t list ->
    extensions:Configuration.Extension.t list ->
    source_paths:Configuration.SourcePaths.t ->
    unit ->
    t

  val watchman_expression_of : t -> Yojson.Safe.t
end

module Subscriber : sig
  module Setting : sig
    type t = {
      (* The underlying low-level abstraction layer. *)
      raw: Raw.t;
      (* The watchman root. *)
      root: PyrePath.t;
      filter: Filter.t;
    }
  end

  type t

  (* Establish a socket connection with the watchman background service, and set up a subscription
     that watches the set of files as specified in the setting. The returned promise resolves
     immediately after the subscription is set up.
   * May raise `ConnectionError` if the underlying connection logic raises, or `SubscriptionError`
     in one of the following case:
   * - Watchman sends an invalid JSON.
   * - Watchman rejects the subscription request.
   * - Watchman's response to the subscription request does not contain an initial clock. *)
  val subscribe : Setting.t -> t Lwt.t

  (* Return the setting used to create the subscriber. *)
  val setting_of : t -> Setting.t

  (* Listen to the watchman server and invoke `f` on the "files" field of every received message.
     The returned promise would wait forever as long as the underlying socket connection is alive,
     except when an excpetion is raised.
   * May raise `ConnectionError` if the underlying connection logic raises, or `SubscriptionError`
     in one of the following case:
   * - Watchman sends an invalid JSON.
   * - Watchman sends an `is_fresh_instance` update.
   * - Watchman sends an update with no "files" field in it. *)
  val listen : f:(PyrePath.t list -> unit Lwt.t) -> t -> unit Lwt.t

  (* A convenient wrapper that invokes `subscribe` first and then `listen` *)
  val with_subscription : f:(PyrePath.t list -> unit Lwt.t) -> Setting.t -> unit Lwt.t
end

module SinceQuery : sig
  module Since : sig
    module SavedState : sig
      type t = {
        storage: string;
        project_name: string;
        project_metadata: string option;
      }
      [@@deriving sexp, compare, hash]
    end

    type t =
      | Clock of string
      | SourceControlAware of {
          mergebase_with: string;
          saved_state: SavedState.t option;
        }
    [@@deriving sexp, compare, hash]
  end

  module Response : sig
    module SavedState : sig
      type t = {
        bucket: string;
        path: string;
        commit_id: string option;
      }
      [@@deriving sexp, compare, hash]
    end

    type t = {
      relative_paths: string list;
      saved_state: SavedState.t option;
    }
    [@@deriving sexp, compare, hash]

    val of_watchman_response_exn : Yojson.Safe.t -> t

    val of_watchman_response : Yojson.Safe.t -> t option
  end

  type t = {
    root: PyrePath.t;
    filter: Filter.t;
    since: Since.t;
  }
  [@@deriving sexp, compare, hash]

  val watchman_request_of : t -> Yojson.Safe.t

  (* Send the given "since" query to the watchman background service and return the response. *)
  val query_exn : connection:Raw.Connection.t -> t -> Response.t Lwt.t

  (* Same as `query_exn` but returns `Result.t` rather than throwing. *)
  val query : connection:Raw.Connection.t -> t -> (Response.t, string) Core.Result.t Lwt.t
end
