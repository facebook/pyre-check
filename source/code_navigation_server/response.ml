(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module ErrorKind = struct
  type t =
    | InvalidRequest of string
    | ModuleNotTracked of { module_: Request.Module.t [@key "module"] }
    | OverlayNotFound of { overlay_id: string }
  [@@deriving sexp, compare, yojson { strict = false }]
end

type t =
  | Ok
  | Error of ErrorKind.t
  | TypeErrors of Analysis.AnalysisError.Instantiated.t list
[@@deriving sexp, compare, yojson { strict = false }]

let to_string response = to_yojson response |> Yojson.Safe.to_string
