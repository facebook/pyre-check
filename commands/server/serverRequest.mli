(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

exception InvalidRequest
val process_request
  :  Unix.File_descr.t
  -> State.t
  -> ServerConfiguration.t
  -> Protocol.Request.t
  -> State.t * (Protocol.response option)
