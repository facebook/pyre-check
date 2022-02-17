(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = string Hashtbl.M(String).t

exception LoadError of string

let load_from_string text =
  let open Yojson.Safe in
  try
    from_string text
    |> Util.to_assoc
    |> List.map ~f:(fun (key, json) -> key, Util.to_string json)
    |> Hashtbl.of_alist (module String)
    |> function
    | `Duplicate_key key ->
        let message = Format.sprintf "Duplicate key `%s` in checksum map" key in
        Result.Error message
    | `Ok state -> Result.Ok state
  with
  | Util.Type_error (message, _)
  | Yojson.Json_error message ->
      Result.Error message


let load { Configuration.UnwatchedFiles.root; checksum_path } =
  let checksum_full_path = PyrePath.create_relative ~root ~relative:checksum_path in
  try Stdio.In_channel.read_all (PyrePath.absolute checksum_full_path) |> load_from_string with
  | Sys_error message -> Result.Error message


let load_exn wheel =
  match load wheel with
  | Result.Ok result -> result
  | Result.Error message -> raise (LoadError message)


let of_alist_exn list = Hashtbl.of_alist_exn (module String) list

let to_alist table = Hashtbl.to_alist table

let empty = of_alist_exn []

module Difference = struct
  module Kind = struct
    type t =
      | New
      | Deleted
      | Changed
    [@@deriving sexp, compare]
  end

  type t = {
    kind: Kind.t;
    path: string;
  }
  [@@deriving sexp, compare]
end

let difference ~original current =
  let scan_original ~key ~data sofar =
    match Hashtbl.find current key with
    | Some right_data ->
        (* The key exists in both maps *)
        if String.equal data right_data then
          sofar
        else
          { Difference.kind = Difference.Kind.Changed; path = key } :: sofar
    | None ->
        (* The key exists in original but not current *)
        { Difference.kind = Difference.Kind.Deleted; path = key } :: sofar
  in
  let scan_current ~key ~data:_ sofar =
    match Hashtbl.find original key with
    | None ->
        (* The key exists in current but not original. *)
        { Difference.kind = Difference.Kind.New; path = key } :: sofar
    | Some _ ->
        (* We've already handled this case in `scan_left`. *)
        sofar
  in
  let result = Hashtbl.fold original ~init:[] ~f:scan_original in
  Hashtbl.fold current ~init:result ~f:scan_current
