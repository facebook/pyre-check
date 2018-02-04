(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

val parse: root: Pyre.Path.t -> check_on_save:bool -> Yojson.Safe.json -> Protocol.Request.t option
