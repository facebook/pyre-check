(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre
open Path.AppendOperator

let log_path configuration =
  (Configuration.pyre_root configuration
   ^| "persistent")
  ^| "client.log"
