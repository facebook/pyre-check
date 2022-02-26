(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Server

module Client : sig
  type t

  val get_server_properties : t -> ServerProperties.t

  val current_server_state : t -> ServerState.t

  val send_raw_request : t -> string -> string Lwt.t

  val send_request : t -> Request.t -> string Lwt.t

  val assert_response_equal : expected:Response.t -> actual:string -> t -> unit

  val assert_response : request:Request.t -> expected:Response.t -> t -> unit Lwt.t

  val subscribe
    :  subscription:Subscription.Request.t ->
    expected_response:Response.t ->
    t ->
    unit Lwt.t

  val assert_subscription_response : expected:Subscription.Response.t -> t -> unit Lwt.t

  val close : t -> unit Lwt.t
end

module ScratchProject : sig
  type t

  val configuration_of : t -> Configuration.Analysis.t

  val start_options_of : t -> StartOptions.t

  val setup
    :  context:OUnit2.test_ctxt ->
    ?external_sources:(string * string) list ->
    ?include_typeshed_stubs:bool ->
    ?include_helper_builtins:bool ->
    ?custom_source_root:PyrePath.t ->
    ?watchman:Watchman.Raw.t ->
    ?build_system_initializer:BuildSystem.Initializer.t ->
    (* A list of test sources specified in the form of (relative_path, content) *)
    (string * string) list ->
    t

  val test_server_with
    :  ?expected_exit_status:Start.ExitStatus.t ->
    ?on_server_socket_ready:(PyrePath.t -> unit Lwt.t) ->
    f:(Client.t -> unit Lwt.t) ->
    t ->
    unit Lwt.t
end
