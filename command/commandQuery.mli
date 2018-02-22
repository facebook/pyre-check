(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

val parse_query: string -> ServerProtocol.type_query_request option

val command: Command.t
