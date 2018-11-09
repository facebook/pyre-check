(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std


type position = {
  line: int;
  column: int;
}
[@@deriving compare, eq, sexp, show, hash, to_yojson]


let any_position =
  { line = -1; column = -1 }

(* We explicitly do not analyze expressions/statements at synthetic positions. *)
let synthetic_position =
  { line = -1; column = -2 }


let show_position { line; column } =
  Format.sprintf "%d:%d" line column


let pp_position format { line; column } =
  Format.fprintf format "%d:%d" line column


type 'path location = {
  path: 'path;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash, to_yojson]


let show pp_path { path; start; stop } =
  Format.asprintf "%a:%d:%d-%d:%d" pp_path path start.line start.column stop.line stop.column



module Reference = struct
  type t = int location
  [@@deriving compare, eq, sexp, hash]


  let pp format { path; start; stop } =
    Format.fprintf format "%d:%d:%d-%d:%d" path start.line start.column stop.line stop.column


  let show =
    show Int.pp


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
      let hash_fold_t = hash_fold_t
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
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


  let any =
    { path = -1; start = any_position; stop = any_position }

  let synthetic =
    { path = -1; start = synthetic_position; stop = synthetic_position }
end


module Instantiated = struct
  type t = string location
  [@@deriving compare, eq, sexp, hash, to_yojson]


  let pp format { path; start; stop } =
    Format.fprintf format "%s:%d:%d-%d:%d" path start.line start.column stop.line stop.column


  let pp_start format { path; start; _ } =
    Format.fprintf format "%s:%d:%d" path start.line start.column


  let show =
    show String.pp


  let create ~start ~stop =
    let create position =
      {
        line = position.Lexing.pos_lnum;
        column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
      }
    in
    {
      path = start.Lexing.pos_fname;
      start = create start;
      stop = create stop;
    }


  let any =
    { path = "*"; start = any_position; stop = any_position }

  let synthetic =
    { path = "*"; start = synthetic_position; stop = synthetic_position }
end


let instantiate { path; start; stop } ~lookup =
  let path = Option.value (lookup path) ~default:"*" in
  { path; start; stop }


let reference { path; start; stop } =
  { path = String.hash path; start; stop }


let line { start = { line; _ }; _ } =
  line


let column { start = { column; _ }; _ } =
  column


let stop_column { stop = { column; _ }; _ } =
  column


let path { path; _ } =
  path


type t = Reference.t
[@@deriving compare, eq, sexp, show, hash]


let create =
  Reference.create
