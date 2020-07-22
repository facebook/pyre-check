(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Core

type t =
  | GetInfo
  | DisplayTypeError of string list
  | Stop
[@@deriving sexp, compare, hash, yojson { strict = false }]
