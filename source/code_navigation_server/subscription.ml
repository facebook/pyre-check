(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module Request = struct
  type t = Subscribe [@@deriving sexp, compare, yojson { strict = false }]
end

module Response = struct
  type t =
    | Ok
    | Idle
    | BusyChecking of { overlay_id: string option }
    | Stop of { message: string }
  [@@deriving sexp, compare, yojson { strict = false }]

  let to_string response = to_yojson response |> Yojson.Safe.to_string
end
