(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std


type position = {
  line: int;
  column: int;
}
[@@deriving compare, eq, sexp, show]


type t = {
  path: string;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show]


let show =
  Format.asprintf "%a" pp


let pp format { path; start; stop; } =
  Format.fprintf format "%s:%d:%d-%d:%d" path start.line start.column stop.line stop.column


let pp_start format { path; start; _ } =
  Format.fprintf format "%s:%d:%d" path start.line start.column


module Map = Map.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


include Hashable.Make(struct
    type nonrec t = t
    let compare = compare
    let hash = Hashtbl.hash
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let create_position position =
  {
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
  }


let create ~start ~stop = {
  path = start.Lexing.pos_fname;
  start = create_position start;
  stop = create_position stop;
}


let any_position =
  { line = -1; column = -1; }


let any =
  { path = "*"; start = any_position; stop = any_position }


let line { start = { line; _ }; _ } =
  line


let column { start = { column; _ }; _ } =
  column


let path { path; _ } =
  path
