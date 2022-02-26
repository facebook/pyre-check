(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Sexplib.Std
module AstReference = Reference

module T = struct
  type position = {
    line: int;
    column: int;
  }
  [@@deriving compare, sexp, hash, to_yojson]

  (* These are not filtered: our backend is broken if any locations appear in errors. *)
  let any_position = { line = -1; column = -1 }

  let show_position { line; column } = Format.sprintf "%d:%d" line column

  let pp_position format { line; column } = Format.fprintf format "%d:%d" line column

  type t = {
    start: position;
    stop: position;
  }
  [@@deriving compare, sexp, hash, to_yojson]

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
end

include T
module Map = Map.Make (T)
module Set = Set.Make (T)
include Hashable.Make (T)

module WithPath = struct
  type t = {
    path: string;
    start: position;
    stop: position;
  }
  [@@deriving compare, sexp, hash, to_yojson]

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
      path: Reference.t;
      start: position;
      stop: position;
    }
    [@@deriving compare, sexp, hash, to_yojson]

    let any = { path = Reference.empty; start = any_position; stop = any_position }

    let line { start = { line; _ }; _ } = line

    let pp format { path; start; stop } =
      Format.fprintf
        format
        "%a:%d:%d-%d:%d"
        Reference.pp
        path
        start.line
        start.column
        stop.line
        stop.column


    let show location = Format.asprintf "%a" pp location

    let instantiate ~lookup { path; start; stop } =
      let path = Option.value (lookup path) ~default:"*" in
      { WithPath.path; start; stop }
  end

  include T
  include Hashable.Make (T)
end

let with_path ~path { start; stop } = { WithPath.path; start; stop }

let with_module ~qualifier { start; stop } = { WithModule.path = qualifier; start; stop }

let strip_module { WithModule.start; stop; _ } = { start; stop }
