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


let show_position { line; column } =
  Format.sprintf "%d:%d" line column


type 'path location = {
  path: 'path;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash]


let to_string pp_path { path; start; stop } =
  Format.asprintf "%a:%d:%d-%d:%d" pp_path path start.line start.column stop.line stop.column



module Reference = struct
  type t = int location
  [@@deriving compare, eq, sexp, show, hash]


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
    let any = { line = -1; column = -1; } in
    { path = -1; start = any; stop = any }


  let pp format { path; start; stop } =
    Format.fprintf format "%d:%d:%d-%d:%d" path start.line start.column stop.line stop.column


  let to_string =
    to_string Int.pp
end


module Instantiated = struct
  type t = string location
  [@@deriving compare, eq, sexp, show, hash]


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
    let any = { line = -1; column = -1; } in
    { path = "*"; start = any; stop = any }


  let pp format { path; start; stop } =
    Format.fprintf format "%s:%d:%d-%d:%d" path start.line start.column stop.line stop.column


  let pp_start format { path; start; _ } =
    Format.fprintf format "%s:%d:%d" path start.line start.column


  let to_string =
    to_string String.pp
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


let path { path; _ } =
  path


type t = Reference.t
[@@deriving compare, eq, sexp, show, hash]


let create =
  Reference.create
