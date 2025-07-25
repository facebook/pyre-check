(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Sexplib.Std
module AstReference = Reference

let safe_int_of_string s =
  try Ok (int_of_string s) with
  | Failure error -> Error error


module T = struct
  type position = {
    line: int;
    column: int;
  }
  [@@deriving equal, compare, sexp, hash, yojson]

  (* These are not filtered: our backend is broken if any locations appear in errors. *)
  let any_position = { line = -1; column = -1 }

  let show_position { line; column } = Format.sprintf "%d:%d" line column

  let pp_position format { line; column } = Format.fprintf format "%d:%d" line column

  let position_from_string position =
    let open Core.Result.Monad_infix in
    String.split ~on:':' position
    |> List.map ~f:safe_int_of_string
    |> Result.all
    >>= function
    | [line_; column_] -> Ok { line = line_; column = column_ }
    | _ -> Error "expected line:column"


  type t = {
    start: position;
    stop: position;
  }
  [@@deriving equal, compare, sexp, hash, yojson]

  let pp format { start; stop } =
    Format.fprintf format "%d:%d-%d:%d" start.line start.column stop.line stop.column


  let equal = [%compare.equal: t]

  let show location = Format.asprintf "%a" pp location

  let create ~start ~stop =
    let create position =
      {
        line = position.Lexing.pos_lnum;
        column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
      }
    in
    { start = create start; stop = create stop }


  let any = { start = any_position; stop = any_position }

  let start { start; _ } = start

  let stop { stop; _ } = stop

  let pp_start format { start; _ } = Format.fprintf format "%d:%d" start.line start.column

  let pp_line_and_column format { start; stop } =
    Format.fprintf format "%d:%d-%d:%d" start.line start.column stop.line stop.column


  let line { start = { line; _ }; _ } = line

  let column { start = { column; _ }; _ } = column

  let stop_column { stop = { column; _ }; _ } = column

  let from_string location =
    let open Core.Result.Monad_infix in
    String.split ~on:'-' location
    |> List.map ~f:position_from_string
    |> Result.all
    >>= function
    | [start_; stop_] -> Ok { start = start_; stop = stop_ }
    | _ -> Error "expected <position>-<position>"
end

include T

module Map = struct
  include Map.Make (T)

  module Tree = Map.Make_tree (struct
    include T
    include Comparator.Make (T)
  end)
end

module SerializableMap = Data_structures.SerializableMap.Make (T)
module Set = Set.Make (T)
include Hashable.Make (T)

module WithPath = struct
  type t = {
    path: string;
    start: position;
    stop: position;
  }
  [@@deriving equal, compare, sexp, hash, to_yojson]

  let line { start = { line; _ }; _ } = line

  let pp format { path; start; stop } =
    Format.fprintf format "%s:%d:%d-%d:%d" path start.line start.column stop.line stop.column


  let any = { path = "*"; start = any_position; stop = any_position }

  let pp_line format { start; stop; _ } =
    let stop_line =
      if start.line = stop.line then
        ""
      else
        Format.asprintf "-%d" stop.line
    in
    Format.fprintf format "%d%s" start.line stop_line
end

module WithModule = struct
  module T = struct
    type t = {
      module_reference: Reference.t;
      start: position;
      stop: position;
    }
    [@@deriving compare, equal, sexp, hash, to_yojson]

    let any = { module_reference = Reference.empty; start = any_position; stop = any_position }

    let line { start = { line; _ }; _ } = line

    let pp format { module_reference; start; stop } =
      Format.fprintf
        format
        "%a:%d:%d-%d:%d"
        Reference.pp
        module_reference
        start.line
        start.column
        stop.line
        stop.column


    let show location = Format.asprintf "%a" pp location

    let instantiate ~lookup { module_reference; start; stop } =
      let path = Option.value (lookup module_reference) ~default:"*" in
      { WithPath.path; start; stop }
  end

  include T
  include Hashable.Make (T)
end

let with_path ~path { start; stop } = { WithPath.path; start; stop }

let with_module ~module_reference { start; stop } = { WithModule.module_reference; start; stop }

let strip_module { WithModule.start; stop; _ } = { start; stop }
