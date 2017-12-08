(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

val process_request
  :  Unix.File_descr.t
  -> State.t
  -> ServerConfiguration.t
  -> Protocol.Request.t
  -> State.t * (Protocol.response option)
