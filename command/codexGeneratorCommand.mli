(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

val to_json: configuration: Configuration.Analysis.t -> File.Handle.t list -> Yojson.Safe.json

val run: bool -> string -> unit -> unit

val command: Core.Command.t
