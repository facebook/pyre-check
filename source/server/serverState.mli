(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Subscriptions : sig
  type t

  val create : unit -> t

  val add : name:string -> subscription:Subscription.t -> t -> unit

  val get : name:string -> t -> Subscription.t option

  val remove : name:string -> t -> unit

  val all : t -> Subscription.t list
end

type t = private {
  build_system: BuildSystem.t;
  overlaid_environment: Analysis.OverlaidEnvironment.t;
  subscriptions: Subscriptions.t;
}

val create
  :  ?subscriptions:Subscriptions.t ->
  build_system:BuildSystem.t ->
  overlaid_environment:Analysis.OverlaidEnvironment.t ->
  unit ->
  t

val load_stored_configuration : unit -> Configuration.Analysis.t

val load : configuration:Configuration.Analysis.t -> build_system:BuildSystem.t -> unit -> t

val store : path:PyrePath.t -> configuration:Configuration.Analysis.t -> t -> unit
