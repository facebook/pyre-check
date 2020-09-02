(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Newserver

module Client : sig
  type t

  val current_server_state : t -> ServerState.t

  val send_raw_request : t -> string -> string Lwt.t

  val send_request : t -> Request.t -> (Response.t, string) Core.Result.t Lwt.t

  val assert_response : request:Request.t -> expected:Response.t -> t -> unit Lwt.t
end

module ScratchProject : sig
  type t = {
    context: OUnit2.test_ctxt;
    server_configuration: ServerConfiguration.t;
    watchman: Watchman.Raw.t option;
  }

  val setup
    :  context:OUnit2.test_ctxt ->
    ?external_sources:(string * string) list ->
    ?include_typeshed_stubs:bool ->
    ?include_helper_builtins:bool ->
    ?watchman:Watchman.Raw.t ->
    (* A list of test sources specified in the form of (relative_path, content) *)
    (string * string) list ->
    t

  val test_server_with : f:(Client.t -> unit Lwt.t) -> t -> unit Lwt.t
end
