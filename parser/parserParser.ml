(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast

module ParserGenerator = ParserGenerator
module Lexer = ParserLexer


exception Error of string

let sanitize_input lines =
  List.map ~f:(fun line -> String.rstrip line) lines
  |> String.concat ~sep:"\n"
  |> fun input -> input ^ "\n"

let parse ?path lines =
  let input = sanitize_input lines in
  let buffer =
    let buffer = Lexing.from_string input in
    buffer.Lexing.lex_curr_p <- {
      buffer.Lexing.lex_curr_p with
      Lexing.pos_fname = Option.value path ~default:"$invalid_path";
    };
    buffer
  in
  let state = Lexer.State.initial () in

  try
    ParserGenerator.parse (Lexer.read state) buffer
  with
  | Pyre.ParserError _
  | ParserGenerator.Error
  | Failure _ ->
      let location =
        Location.create
          ~start:buffer.Lexing.lex_curr_p
          ~stop:buffer.Lexing.lex_curr_p
      in
      let line = location.Location.start.Location.line - 1
      and column = location.Location.start.Location.column in

      let error =
        let header =
          Format.asprintf
            "Could not parse file at %a"
            Location.pp location in
        let indicator =
          if column > 0 then
            (String.make (column - 1) ' ') ^ "^"
          else
            "^" in
        match List.nth (String.split ~on:'\n' input) line with
        | Some line ->
            Format.sprintf "%s\n  %s\n  %s" header line indicator
        | None ->
            Format.sprintf "%s" header in
      raise (Error error)
