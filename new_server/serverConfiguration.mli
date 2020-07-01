(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t = { log_path: Path.t } [@@deriving sexp, compare]

val of_yojson : Yojson.Safe.t -> (t, string) Result.t
