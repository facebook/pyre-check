(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type t =
  | Ok
  | Error of string
  | Info of {
      version: string;
      pid: int;
      socket: string;
      configuration: ServerConfiguration.t;
    }
  | TypeErrors of Analysis.AnalysisError.Instantiated.t list
[@@deriving sexp, compare, hash, yojson]
