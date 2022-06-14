(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

type mode =
  | Debug
  | Strict
  | Unsafe
  | Declare
[@@deriving compare, show, sexp, hash]

type local_mode =
  | Strict
  | Unsafe
  | Declare
  | PlaceholderStub
[@@deriving compare, show, sexp, hash]

module TypecheckFlags = struct
  type t = {
    local_mode: local_mode Node.t option;
    unused_local_modes: local_mode Node.t list;
    ignore_codes: int list;
    ignore_lines: Ignore.t list;
  }
  [@@deriving compare, show, hash, sexp]

  let is_placeholder_stub local_mode =
    match local_mode with
    | Some { Node.value = PlaceholderStub; _ } -> true
    | _ -> false


  let create_for_testing
      ?(local_mode = Node.create ~location:Location.any Unsafe)
      ?(unused_local_modes = [])
      ?(ignore_codes = [])
      ?(ignore_lines = [])
      ()
    =
    { local_mode = Some local_mode; unused_local_modes; ignore_codes; ignore_lines }


  let default_with_suppress_regex =
    Str.regexp "^ ?# *pyre-ignore-all-errors *\\[\\([0-9]+, *\\)*\\([0-9]+\\)\\]"


  let ignore_code_regex = Str.regexp "# *pyre-\\(ignore\\|fixme\\) *\\[\\([0-9, ]+\\)\\]"

  let ignore_location_regex = Str.regexp "# *\\(pyre-\\(ignore\\|fixme\\)\\|type: ignore\\)"

  let is_pyre_comment comment_substring line =
    let comment_regex = Str.regexp ("^ ?#+ *" ^ comment_substring ^ " *$") in
    Str.string_match comment_regex line 0


  let parse ~qualifier lines =
    let is_strict = is_pyre_comment "pyre-strict" in
    let is_unsafe = is_pyre_comment "pyre-unsafe" in
    (* We do not fall back to declarative mode on a typo when attempting to only suppress certain
       errors. *)
    let is_declare = is_pyre_comment "pyre-ignore-all-errors" in
    let is_default_with_suppress line = Str.string_match default_with_suppress_regex line 0 in
    let is_placeholder_stub = is_pyre_comment "pyre-placeholder-stub" in
    let collect index (local_mode, unused_local_modes, ignore_codes, ignore_lines) line =
      let current_line_mode =
        let location =
          let length = String.length line in
          let content_length = String.length (String.strip line) in
          let start = { Location.line = index + 1; column = length - content_length } in
          let stop = { Location.line = index + 1; column = length } in
          { Location.start; stop }
        in
        if is_strict line then
          Some (Node.create ~location Strict)
        else if is_unsafe line then
          Some (Node.create ~location Unsafe)
        else if is_declare line then
          Some (Node.create ~location Declare)
        else if is_placeholder_stub line then
          Some (Node.create ~location PlaceholderStub)
        else
          None
      in
      let ignores =
        let line_index = index + 1 in
        let create_ignore ~line ~kind =
          try
            let location =
              let start_column = Str.search_forward ignore_location_regex line 0 in
              let end_column = String.length line in
              let start = { Location.line = line_index; column = start_column } in
              let stop = { Location.line = line_index; column = end_column } in
              { Location.start; stop }
            in
            let codes =
              try
                Str.search_forward ignore_code_regex line 0 |> ignore;
                Str.matched_group 2 line
                |> Str.split (Str.regexp "[^0-9]+")
                |> List.map ~f:Int.of_string
              with
              | Not_found -> []
            in
            Some (Ignore.create ~ignored_line:line_index ~codes ~location ~kind)
          with
          | Not_found -> None
        in
        let contains_outside_quotes ~substring line =
          let find_substring index characters =
            String.is_substring ~substring characters && index mod 2 = 0
          in
          String.split_on_chars ~on:['\"'; '\''] line |> List.existsi ~f:find_substring
        in
        let ignore_lines =
          let kind =
            if
              contains_outside_quotes ~substring:"pyre-ignore" line
              && not (contains_outside_quotes ~substring:"pyre-ignore-all-errors" line)
            then
              Some Ignore.PyreIgnore
            else if contains_outside_quotes ~substring:"pyre-fixme" line then
              Some Ignore.PyreFixme
            else if contains_outside_quotes ~substring:"type: ignore" line then
              Some Ignore.TypeIgnore
            else
              None
          in
          kind
          >>= (fun kind -> create_ignore ~line ~kind)
          >>| (fun data -> Int.Map.add_multi ~key:line_index ~data ignore_lines)
          |> Option.value ~default:ignore_lines
        in
        if String.is_prefix ~prefix:"#" (String.strip line) && Option.is_none current_line_mode then
          (* Increment ignores applied to current line if it is a comment. *)
          match Int.Map.find ignore_lines line_index with
          | Some ignores -> (
              let ignore_lines = Int.Map.remove ignore_lines line_index in
              match Int.Map.find ignore_lines (line_index + 1) with
              | Some existing_ignores ->
                  Int.Map.set
                    ~key:(line_index + 1)
                    ~data:(List.map ~f:Ignore.increment ignores @ existing_ignores)
                    ignore_lines
              | None ->
                  Int.Map.set
                    ~key:(line_index + 1)
                    ~data:(List.map ~f:Ignore.increment ignores)
                    ignore_lines)
          | None -> ignore_lines
        else
          ignore_lines
      in
      let local_mode, unused_local_modes =
        match local_mode, current_line_mode with
        | Some _, Some mode -> local_mode, mode :: unused_local_modes
        | Some _, None -> local_mode, unused_local_modes
        | None, mode -> mode, unused_local_modes
      in
      let ignore_codes =
        if is_default_with_suppress line then
          let safe_int_of_string s =
            try Some (int_of_string s) with
            | _ ->
                Log.warning
                  "Parsing ignore comment: `int_of_string` failed on string `%s` in module `%s`.\n\
                   The full line was `%s`."
                  s
                  (Reference.show qualifier)
                  line;
                None
          in
          let suppressed_codes =
            Str.global_substitute (Str.regexp "[^,0-9]+") (fun _ -> "") line
            |> String.split_on_chars ~on:[',']
            |> List.filter_map ~f:safe_int_of_string
          in
          suppressed_codes @ ignore_codes
        else
          ignore_codes
      in
      local_mode, unused_local_modes, ignore_codes, ignores
    in
    let local_mode, unused_local_modes, ignore_codes, ignore_lines =
      List.map ~f:(fun line -> String.rstrip line |> String.lowercase) lines
      |> List.foldi ~init:(None, [], [], Int.Map.empty) ~f:collect
    in
    {
      local_mode;
      unused_local_modes = List.rev unused_local_modes;
      ignore_codes;
      ignore_lines = ignore_lines |> Int.Map.data |> List.concat;
    }
end

type t = {
  typecheck_flags: TypecheckFlags.t;
  module_path: ModulePath.t;
  top_level_unbound_names: Statement.Define.NameAccess.t list;
  statements: Statement.t list;
}
[@@deriving compare, hash, sexp]

let pp format { statements; _ } =
  let print_statement statement = Format.fprintf format "%a\n" Statement.pp statement in
  List.iter statements ~f:print_statement


let pp_all format source = Sexp.pp_hum format (sexp_of_t source)

let location_insensitive_compare left right =
  match TypecheckFlags.compare left.typecheck_flags right.typecheck_flags with
  | x when x <> 0 -> x
  | _ -> (
      match ModulePath.compare left.module_path right.module_path with
      | x when x <> 0 -> x
      | _ -> (
          match
            List.compare
              Statement.Define.NameAccess.compare
              left.top_level_unbound_names
              right.top_level_unbound_names
          with
          | x when x <> 0 -> x
          | _ ->
              List.compare Statement.location_insensitive_compare left.statements right.statements))


let show source = Format.asprintf "%a" pp source

let mode ~configuration ~local_mode : mode =
  match local_mode, configuration with
  | _, { Configuration.Analysis.debug = true; _ } -> Debug
  | Some { Node.value = Strict; _ }, _ -> Strict
  | Some { Node.value = Unsafe; _ }, _ -> Unsafe
  | Some { Node.value = Declare; _ }, _
  | Some { Node.value = PlaceholderStub; _ }, _ ->
      Declare
  | None, { Configuration.Analysis.strict = true; _ } -> Strict
  | None, _ -> Unsafe


(* An f-string may span multiple lines. Synthesize ignores for each of the lines using the ignores
   from the first line. *)
let synthesize_ignores_for_format_string
    ( { Node.location = { start = { line = start_line; _ }; stop = { line = stop_line; _ } }; _ },
      first_line_ignores )
  =
  let copy_ignores_from_first_line line =
    line, List.map first_line_ignores ~f:(fun ignore -> { ignore with Ignore.ignored_line = line })
  in
  List.range start_line (stop_line + 1) |> List.map ~f:copy_ignores_from_first_line


let noop_collect_format_strings ~ignore_line_map:_ _ = []

let ignored_lines_including_format_strings
    ?(collect_format_strings_with_ignores = noop_collect_format_strings)
    ({ typecheck_flags = { TypecheckFlags.ignore_lines; _ }; _ } as source)
  =
  let ignore_line_map =
    List.map ignore_lines ~f:(fun ({ Ignore.ignored_line; _ } as ignore) -> ignored_line, ignore)
    |> Int.Map.of_alist_multi
  in
  collect_format_strings_with_ignores ~ignore_line_map source
  |> List.concat_map ~f:synthesize_ignores_for_format_string
  |> Int.Map.of_alist_reduce ~f:List.append
  |> Map.merge_skewed ~combine:(fun ~key:_ -> List.append) ignore_line_map
  |> Int.Map.data
  |> List.concat_map ~f:(List.dedup_and_sort ~compare:Ignore.compare)


let create_from_module_path
    ?collect_format_strings_with_ignores
    ~typecheck_flags
    ~module_path
    statements
  =
  let source = { typecheck_flags; module_path; top_level_unbound_names = []; statements } in
  {
    source with
    typecheck_flags =
      {
        typecheck_flags with
        ignore_lines =
          ignored_lines_including_format_strings ?collect_format_strings_with_ignores source;
      };
  }


let create
    ?(typecheck_flags = TypecheckFlags.create_for_testing ())
    ?(relative = "")
    ?(is_external = false)
    ?(priority = 0)
    statements
  =
  let module_path = ModulePath.create_for_testing ~relative ~is_external ~priority in
  create_from_module_path ~typecheck_flags ~module_path statements


let ignore_lines { typecheck_flags = { TypecheckFlags.ignore_lines; _ }; _ } = ignore_lines

let statements { statements; _ } = statements

let top_level_define
    { module_path = { ModulePath.qualifier; _ }; top_level_unbound_names; statements; _ }
  =
  Statement.Define.create_toplevel
    ~qualifier:(Some qualifier)
    ~unbound_names:top_level_unbound_names
    ~statements


let top_level_define_node source =
  let location =
    { Location.start = { Location.line = 1; column = 1 }; stop = { Location.line = 1; column = 1 } }
  in
  Node.create ~location (top_level_define source)
