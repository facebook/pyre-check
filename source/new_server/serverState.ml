(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Analysis

module Subscriptions = struct
  type t = Subscription.t String.Table.t

  let create () = String.Table.create ()

  let add ~name ~subscription subscriptions = Hashtbl.set subscriptions ~key:name ~data:subscription

  let get ~name subscriptions = Hashtbl.find subscriptions name

  let remove ~name subscriptions = Hashtbl.remove subscriptions name

  let all subscriptions = Hashtbl.data subscriptions
end

type t = {
  start_time: Timer.t;
  socket_path: Path.t;
  server_configuration: ServerConfiguration.t;
  configuration: Configuration.Analysis.t;
  build_system: BuildSystem.t;
  type_environment: TypeEnvironment.t;
  error_table: AnalysisError.t list Reference.Table.t;
  subscriptions: Subscriptions.t;
}

let create
    ?start_time
    ?error_table
    ?subscriptions
    ~socket_path
    ~server_configuration
    ~build_system
    ~type_environment
    ()
  =
  {
    start_time = Option.value start_time ~default:(Timer.start ());
    socket_path;
    server_configuration;
    configuration = ServerConfiguration.analysis_configuration_of server_configuration;
    build_system;
    type_environment;
    error_table = Option.value error_table ~default:(Reference.Table.create ());
    subscriptions = Option.value subscriptions ~default:(Subscriptions.create ());
  }
