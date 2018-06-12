(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


exception InvalidRequest
val process_request
  :  Unix.File_descr.t
  -> ServerState.t
  -> ServerConfiguration.t
  -> ServerProtocol.Request.t
  -> ServerState.t * (ServerProtocol.response option)
