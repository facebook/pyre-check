(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std


type position = {
  line: int;
  column: int;
}
[@@deriving compare, eq, sexp, show, hash]


type 'path t = {
  path: 'path;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash]


type reference = int t
[@@deriving compare, eq, sexp, show, hash]


type instantiated = string t
[@@deriving compare, eq, sexp, show, hash]


module ReferenceMap = Map.Make(struct
    type nonrec t = reference
    let compare = compare_reference
    let sexp_of_t = sexp_of_reference
    let t_of_sexp = reference_of_sexp
  end)


module ReferenceSet = Set.Make(struct
    type nonrec t = reference
    let compare = compare_reference
    let sexp_of_t = sexp_of_reference
    let t_of_sexp = reference_of_sexp
  end)


include Hashable.Make(struct
    type nonrec t = reference
    let compare = compare_reference
    let hash = Hashtbl.hash
    let hash_fold_t = hash_fold_reference
    let sexp_of_t = sexp_of_reference
    let t_of_sexp = reference_of_sexp
  end)


let create ~start ~stop =
  let create position =
    {
      line = position.Lexing.pos_lnum;
      column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
    }
  in
  {
    path = String.hash start.Lexing.pos_fname;
    start = create start;
    stop = create stop;
  }


let pp_reference format { path; start; stop } =
  Format.fprintf format "%d:%d:%d-%d:%d" path start.line start.column stop.line stop.column


let pp_instantiated format { path; start; stop } =
  Format.fprintf format "%s:%d:%d-%d:%d" path start.line start.column stop.line stop.column


let to_string pp_path { path; start; stop } =
  Format.asprintf "%a:%d:%d-%d:%d" pp_path path start.line start.column stop.line stop.column


let to_string_reference =
  to_string Int.pp


let to_string_instantiated =
  to_string String.pp


let pp_start_instantiated format { path; start; _ } =
  Format.fprintf format "%s:%d:%d" path start.line start.column


let instantiate ~lookup { path; start; stop } =
  let path = Option.value (lookup path) ~default:"*" in
  { path; start; stop }


let to_reference { path; start; stop } =
  { path = String.hash path; start; stop }


let any_position =
  { line = -1; column = -1; }


let any =
  { path = -1; start = any_position; stop = any_position }


let any_instantiated =
  { path = "*"; start = any_position; stop = any_position }


let line { start = { line; _ }; _ } =
  line


let column { start = { column; _ }; _ } =
  column


let path { path; _ } =
  path
