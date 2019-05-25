(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

exception InvalidQuery of string

val help : unit -> string

val parse_query : configuration:Configuration.Analysis.t -> string -> Protocol.Request.t
