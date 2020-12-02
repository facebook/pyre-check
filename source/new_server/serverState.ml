(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

type t = {
  socket_path: Path.t;
  server_configuration: ServerConfiguration.t;
  configuration: Configuration.Analysis.t;
  type_environment: Analysis.TypeEnvironment.t;
  error_table: Analysis.AnalysisError.t list Ast.Reference.Table.t;
  subscriptions: Subscription.t String.Table.t;
}

let add_subscription ~name ~subscription { subscriptions; _ } =
  Hashtbl.set subscriptions ~key:name ~data:subscription


let remove_subscription ~name { subscriptions; _ } = Hashtbl.remove subscriptions name
