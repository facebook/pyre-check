(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = Stop [@@deriving sexp, compare, yojson { strict = false }]

let of_string message =
  try
    Yojson.Safe.from_string message
    |> of_yojson
    |> Result.map_error ~f:(fun _ -> "Unrecognized request JSON")
  with
  | Yojson.Json_error message ->
      let message = Format.sprintf "Cannot parse JSON. %s" message in
      Result.Error message
