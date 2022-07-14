(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module OverlayCodeUpdate = struct
  type t =
    | NewCode of string
    | ResetCode
  [@@deriving sexp, compare, hash, yojson]
end

type t =
  | DisplayTypeError of string list
  | IncrementalUpdate of string list
  | Query of string
  | OverlayUpdate of {
      overlay_id: string;
      source_path: string;
      code_update: OverlayCodeUpdate.t;
    }
[@@deriving sexp, compare, hash, yojson { strict = false }]

(* For some of the requests, use their legacy names for backward compatibility. *)
let name_of = function
  | DisplayTypeError _ -> "DisplayTypeErrors"
  | IncrementalUpdate _ -> "IncrementalCheck"
  | Query _ -> "Query"
  | OverlayUpdate _ -> "OverlayUpdate"
