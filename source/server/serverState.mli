(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Subscriptions : sig
  type t

  val create : unit -> t

  val add : subscription:Subscription.t -> t -> unit

  val get : name:string -> t -> Subscription.t option

  val remove : name:string -> t -> unit

  val all : t -> Subscription.t list
end

module BuildFailure : sig
  type t

  val create : unit -> t

  val update : events:SourcePath.Event.t list -> error_message:string -> t -> unit

  val get_last_error_message : t -> string option

  val get_deferred_events : t -> SourcePath.Event.t list

  val clear : t -> unit
end

type t = private {
  scheduler: Scheduler.t;
  build_system: BuildSystem.t;
  overlaid_environment: Analysis.OverlaidEnvironment.t;
  query_cache: Query.Cache.t;
  subscriptions: Subscriptions.t;
  build_failure: BuildFailure.t;
}

val create
  :  ?subscriptions:Subscriptions.t ->
  ?build_failure:BuildFailure.t ->
  scheduler:Scheduler.t ->
  build_system:BuildSystem.t ->
  overlaid_environment:Analysis.OverlaidEnvironment.t ->
  unit ->
  t

val load_stored_configuration : unit -> Configuration.Analysis.t

val load
  :  environment_controls:Analysis.EnvironmentControls.t ->
  scheduler:Scheduler.t ->
  build_system:BuildSystem.t ->
  unit ->
  t

val store : path:PyrePath.t -> configuration:Configuration.Analysis.t -> t -> unit
