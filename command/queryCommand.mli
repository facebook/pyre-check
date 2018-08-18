(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Server

val parse_query: root: Pyre.Path.t -> string -> Protocol.Request.t option

val command: Command.t
