(** Copyright 2016-present Facebook. All rights reserved. **)

val parse: root: Pyre.Path.t -> Yojson.Safe.json -> Protocol.Request.t option
