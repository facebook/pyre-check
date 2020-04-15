(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type t

val create : Configuration.t -> t

val get_triggered_sink : t -> partial_sink:Sinks.partial_sink -> source:Sources.t -> Sinks.t option
