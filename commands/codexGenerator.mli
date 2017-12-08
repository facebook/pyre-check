(** Copyright 2016-present Facebook. All rights reserved. **)

val to_json: root: string -> File.Handle.t list -> Yojson.Safe.json

val run: bool -> string -> unit -> unit

val command: Core.Command.t
