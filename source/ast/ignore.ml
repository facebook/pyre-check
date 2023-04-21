(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module represents comments used to ignore Pyre errors, such as `type: ignore` (specified in
   PEP 484) and `# pyre-fixme[9]` (Pyre's custom error-suppression comment, which allows suppressing
   specific error codes). *)

open Core

type kind =
  | TypeIgnore
  | PyreFixme
  | PyreIgnore
[@@deriving compare, show, sexp, hash]

type line_or_range =
  | Line of int
  | Range of {
      start_line: int;
      end_line: int;
    }
[@@deriving compare, show, sexp, hash]

type t = {
  (* Ignore comments usually apply to one line. However, for multi-line f-strings, they need to
     cover the range of lines spanned by the f-string. *)
  ignored_line_or_range: line_or_range;
  codes: int list;
  location: Location.t;
  kind: kind;
}
[@@deriving show, sexp]

let compare left right =
  let { Location.start = left_start; _ } = left.location in
  let { Location.start = right_start; _ } = right.location in
  [%compare: line_or_range * int list * Location.position * kind]
    (left.ignored_line_or_range, left.codes, left_start, left.kind)
    (right.ignored_line_or_range, right.codes, right_start, right.kind)


let hash_fold_t state { ignored_line_or_range; codes; location; kind; _ } =
  let { Location.start; _ } = location in
  [%hash_fold: line_or_range * int list * Location.position * kind]
    state
    (ignored_line_or_range, codes, start, kind)


let hash = Hash.run hash_fold_t

let create ~ignored_line ~codes ~location ~kind =
  { ignored_line_or_range = Line ignored_line; codes; location; kind }


let create_with_range ~start_line ~end_line ~codes ~location ~kind =
  { ignored_line_or_range = Range { start_line; end_line }; codes; location; kind }


let codes { codes; _ } = codes

let location { location; _ } = location

let kind { kind; _ } = kind

let increment ({ ignored_line_or_range; _ } as ignore) =
  {
    ignore with
    ignored_line_or_range =
      (match ignored_line_or_range with
      | Line line -> Line (line + 1)
      | Range ({ start_line; _ } as range) -> Range { range with start_line = start_line + 1 });
  }


let start_of_ignored_line_or_range { ignored_line_or_range; _ } =
  match ignored_line_or_range with
  | Line start -> start
  | Range { start_line; _ } -> start_line


let lines_covered_by_ignore { ignored_line_or_range; _ } =
  match ignored_line_or_range with
  | Line line -> [line]
  | Range { start_line; end_line } ->
      List.init (end_line - start_line + 1) ~f:(fun offset -> start_line + offset)


let with_start_line ~start_line ({ ignored_line_or_range; _ } as ignore) =
  let ignored_line_or_range =
    match ignored_line_or_range with
    | Line _ -> Line start_line
    | Range range -> Range { range with start_line }
  in
  { ignore with ignored_line_or_range }


let cover_end_line ~end_line ({ ignored_line_or_range; _ } as ignore) =
  let range =
    match ignored_line_or_range with
    | Line start_line -> Range { start_line; end_line }
    | Range ({ end_line = old_end_line; _ } as range) ->
        Range { range with end_line = Int.max old_end_line end_line }
  in
  { ignore with ignored_line_or_range = range }
