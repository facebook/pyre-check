(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val register_configuration : Configuration.Analysis.t -> unit

val get_configuration : unit -> Configuration.Analysis.t
