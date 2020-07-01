(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t = { log_path: Path.t } [@@deriving sexp, compare]

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    let log_path =
      json |> member "log_path" |> to_string |> Path.create_absolute ~follow_symbolic_links:false
    in
    Result.Ok { log_path }
  with
  | Type_error (message, _)
  | Undefined (message, _) ->
      Result.Error message
  | other_exception -> Result.Error (Exn.to_string other_exception)
