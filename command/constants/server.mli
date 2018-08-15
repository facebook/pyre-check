(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre

val root: Configuration.t -> Path.t

val log_path: Configuration.t -> Path.t
