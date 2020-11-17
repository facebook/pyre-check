(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Cache : sig
  val load : configuration:Configuration.Analysis.t -> Analysis.TypeEnvironment.t option

  val save
    :  configuration:Configuration.Analysis.t ->
    environment:Analysis.TypeEnvironment.t ->
    unit
end

val command : Command.t
