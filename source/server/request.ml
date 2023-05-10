(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

module OverlayCodeUpdate = struct
  type t =
    | NewCode of string
    | ResetCode
  [@@deriving sexp, equal, hash, yojson]
end

type t =
  | DisplayTypeError of string list
  | GetOverlayTypeErrors of {
      overlay_id: string;
      path: string;
    }
  | IncrementalUpdate of string list
  | Query of string
  | QueryWithOverlay of {
      query_text: string;
      overlay_id: string option;
    }
  | OverlayUpdate of {
      overlay_id: string;
      source_path: string;
      code_update: OverlayCodeUpdate.t;
    }
[@@deriving sexp, equal, hash, yojson { strict = false }]

(* For some of the requests, use their legacy names for backward compatibility. *)
let name_of = function
  | DisplayTypeError _ -> "DisplayTypeErrors"
  | GetOverlayTypeErrors _ -> "GetOverlayTypeErrors"
  | IncrementalUpdate _ -> "IncrementalCheck"
  | Query _ -> "Query"
  | QueryWithOverlay _ -> "QueryWithOverlay"
  | OverlayUpdate _ -> "OverlayUpdate"
