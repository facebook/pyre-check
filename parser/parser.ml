(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

exception Error of string

let sanitize_input lines =
  (* Remove byte order mark from first line if it exists. *)
  let lines =
    match lines with
    | first_line :: rest ->
        let byte_order_mark =
          [0xEF; 0xBB; 0xBF] |> List.map ~f:Char.of_int_exn |> String.of_char_list
        in
        if String.is_prefix first_line ~prefix:byte_order_mark then
          String.drop_prefix first_line (String.length byte_order_mark) :: rest
        else
          lines
    | [] -> []
  in
  List.map ~f:(fun line -> String.rstrip line) lines
  |> String.concat ~sep:"\n"
  |> fun input -> input ^ "\n"


let parse ?start_line ?start_column ?relative lines =
  let input = sanitize_input lines in
  let buffer =
    let buffer = Lexing.from_string input in
    buffer.Lexing.lex_curr_p <-
      {
        Lexing.pos_fname = Option.value relative ~default:"$invalid_path";
        pos_lnum = Option.value start_line ~default:1;
        pos_bol = -Option.value start_column ~default:0;
        pos_cnum = 0;
      };
    buffer
  in
  let state = Lexer.State.initial () in
  try Generator.parse (Lexer.read state) buffer with
  | Pyre.ParserError _
  | Generator.Error
  | Failure _ ->
      let location =
        Location.Instantiated.create ~start:buffer.Lexing.lex_curr_p ~stop:buffer.Lexing.lex_curr_p
      in
      let line = location.Location.start.Location.line - 1
      and column = location.Location.start.Location.column in
      let error =
        let header =
          Format.asprintf "Could not parse file at %a" Location.Instantiated.pp location
        in
        let indicator =
          if column > 0 then
            String.make (column - 1) ' ' ^ "^"
          else
            "^"
        in
        match List.nth (String.split ~on:'\n' input) line with
        | Some line -> Format.sprintf "%s\n  %s\n  %s" header line indicator
        | None -> Format.sprintf "%s" header
      in
      raise (Error error)
