(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre

module Subscriptions : sig
  type t

  val create : unit -> t

  val add : name:string -> subscription:Subscription.t -> t -> unit

  val get : name:string -> t -> Subscription.t option

  val remove : name:string -> t -> unit

  val all : t -> Subscription.t list
end

type t = private {
  start_time: Timer.t;
  socket_path: Path.t;
  configuration: Configuration.Analysis.t;
  critical_files: CriticalFile.t list;
  build_system: BuildSystem.t;
  type_environment: Analysis.TypeEnvironment.t;
  error_table: Analysis.AnalysisError.t list Ast.Reference.Table.t;
  subscriptions: Subscriptions.t;
}

val create
  :  ?start_time:Timer.t ->
  ?error_table:Analysis.AnalysisError.t list Ast.Reference.Table.t ->
  ?subscriptions:Subscriptions.t ->
  socket_path:Path.t ->
  configuration:Configuration.Analysis.t ->
  critical_files:CriticalFile.t list ->
  build_system:BuildSystem.t ->
  type_environment:Analysis.TypeEnvironment.t ->
  unit ->
  t
