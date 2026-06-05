(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The lexer is heavily based on
   https://github.com/jeremybuisson/ocaml-pythonlib/tree/master/src.
   "Ported" to menhir and made more complete. *)

{
open Core
open Lexing
open Generator

module State = struct
  type t = {
    mutable offset: int;
    offsets: int Stack.t;
    mutable nesting: int;
  }

  let initial () =
    { offset = 0; offsets = Stack.of_list [0]; nesting = 0 }
end

open State

let line_break buffer =
  let position = buffer.lex_curr_p in
  buffer.lex_curr_p <- {
    position with
    pos_bol = position.pos_cnum;
    pos_lnum = position.pos_lnum + 1
  }

let line_breaks buffer string =
  for _ = 1 to (String.count ~f:(fun character -> Char.equal character '\n') string) do
    line_break buffer
  done

let string string_position content_position prefix value =
  let is_format_prefix prefix =
    String.contains prefix 'f' || String.contains prefix 'F' in
  if is_format_prefix prefix then
    FORMAT (string_position, content_position, value)
  else
    STRING (string_position, content_position, value)

let strip_underscores string =
  if String.contains string '_'  then
    String.split_on_chars ~on:['_'] string
    |> String.concat ~sep:""
  else
    string

let parse_float value =
  try
    Float.of_string (strip_underscores value)
  with Failure _ ->
    Float.max_value

let parse_integer value =
  try
    Int.of_string (strip_underscores value)
  with Failure _ ->
    Int.max_value

}

let empty = ""

let newline = ("\r\n" | '\n')
let whitespace = [' ' '\t']
let comment = '#' [^ '\n' '\r']*

let identifier = ['$' 'a'-'z' 'A'-'Z' '_'] ['$' '?' 'a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']
let digipart = digit (('_'? digit)* )

let integer =
  ('0' ['b' 'B'] ('_'? ['0'-'1'])+) | (* Binary. *)
  ('0' ['o' 'O'] ('_'? ['0'-'7'])+) | (* Octal. *)
  ('0' ['x' 'X'] ('_'? hexdigit)+) | (* Hexadecimal. *)
  (['1' - '9'] ('_'? digit)* | '0' ('_'? '0')* ) |  (* Decimal. *)
  ('0' digit+) (* Valid before python 3.6 *)

let exponent = ['e''E'] ['-''+']? digipart
let pointfloat = (digipart '.') | (digipart? '.' digipart)
let float = (pointfloat exponent?) | (digipart exponent)
let complex = (float | digipart) ('j' | 'J')
let long = (['0' - '9']+ ('L'))

let kind = 'b' | 'B' | 'f' | 'F'
let encoding = 'u' | 'U' | 'r' | 'R'
(* (encoding encoding) for python2 legacy support *)
let stringprefix = (encoding | kind | (encoding kind) | (kind encoding) | (encoding encoding))?
let escape = '\\' _

rule read state = parse
  | newline whitespace* comment {
      line_break lexbuf;
      read state lexbuf
    }
  | whitespace* '\\' newline {
      line_break lexbuf;
      read state lexbuf
    }
  | empty {
      let offset = state.offset
      and last_offset = Stack.top_exn state.offsets in
      if offset < last_offset then
        begin
          ignore (Stack.pop_exn state.offsets);
          DEDENT lexbuf.lex_curr_p;
        end
      else if offset > last_offset then
        begin
          Stack.push state.offsets offset;
          INDENT;
        end
      else
        read_without_indent state lexbuf
    }

and read_without_indent state = parse
  | (newline (whitespace* comment)?)+ {
      let lines =
        String.filter (lexeme lexbuf) ~f:(fun char -> Char.equal char '\n')
        |> String.length in
      for _ = 1 to lines do line_break lexbuf done;
      if state.nesting <= 0 then
        begin
          state.offset <- 0;
          offset state lexbuf;
          NEWLINE lexbuf.lex_start_p
        end
      else
        read_without_indent state lexbuf
    }
  (* handle whitespace in python3 print statements  *)
  | "print" whitespace+ "(" {
      lexbuf.lex_curr_pos <- lexbuf.lex_start_pos + 5;
      IDENTIFIER ((lexbuf.lex_start_p, lexbuf.lex_curr_p), lexeme lexbuf)
    }
  (* Don't even try to do anything with Python 2 print statements. *)
  | "print" whitespace+ [ ^ '=' '<' '>' '*' '+' '-' ] {
      lexbuf.lex_curr_pos <- lexbuf.lex_start_pos + 5;
      read_without_indent state lexbuf
    }
  | "print" whitespace+ ">>" {
      read_without_indent state lexbuf
  }

  | "is" whitespace+ "not" whitespace+ { ISNOT }

  | identifier as identifier {
      match identifier with
      | "and" -> AND
      | "async" -> ASYNC (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "class" -> CLASS lexbuf.lex_start_p
      | "def" -> DEFINE lexbuf.lex_start_p
      | "elif" -> ELSEIF lexbuf.lex_start_p
      | "else" -> ELSE
      | "False" -> FALSE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "if" -> IF lexbuf.lex_start_p
      | "in" -> IN
      | "is" -> IS
      | "None" -> NONE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "not" -> NOT lexbuf.lex_start_p
      | "or" -> OR
      | "True" -> TRUE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | _ -> IDENTIFIER ((lexbuf.lex_start_p, lexbuf.lex_curr_p), lexeme lexbuf)
    }

  | float {
      FLOAT ((lexbuf.lex_start_p, lexbuf.lex_curr_p), parse_float (lexeme lexbuf))
    }
  | integer {
      INTEGER ((lexbuf.lex_start_p, lexbuf.lex_curr_p), parse_integer (lexeme lexbuf))
    }
  | complex {
      let value =
        let value = lexeme lexbuf in
        String.slice value 0 (String.length value - 1)
        |> parse_float
      in
      COMPLEX ((lexbuf.lex_start_p, lexbuf.lex_curr_p), value)
    }
  | long {
    let value =
      let value = lexeme lexbuf in
      String.slice value 0 (String.length value - 1)
      |> parse_integer
    in
    INTEGER ((lexbuf.lex_start_p, lexbuf.lex_curr_p), value)
  }

  | "..." { ELLIPSES (lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  | '.' { DOT (lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  | '!' { EXCLAMATIONMARK }
  | '(' {
      state.nesting <- state.nesting + 1;
      LEFTPARENS lexbuf.lex_start_p
    }
  | ')' {
      state.nesting <- state.nesting - 1;
      RIGHTPARENS lexbuf.lex_curr_p
    }
  | '*' { ASTERIKS (lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  | '@' { AT }
  | '+' { PLUS lexbuf.lex_start_p }
  | ',' { COMMA }
  | '-' { MINUS lexbuf.lex_start_p }
  | '/' { SLASH (lexbuf.lex_start_p) }
  | ':' { COLON lexbuf.lex_start_p }
  | "<=" { LEFTANGLEEQUALS }
  | '<' { LEFTANGLE }
  | "==" { DOUBLEEQUALS }
  | '=' { EQUALS }
  | ">=" { RIGHTANGLEEQUALS }
  | '>' { RIGHTANGLE }
  | '[' {
      state.nesting <- state.nesting + 1;
      LEFTBRACKET lexbuf.lex_start_p
    }
  | ']' {
      state.nesting <- state.nesting - 1;
      RIGHTBRACKET lexbuf.lex_curr_p
    }
  | '~' { TILDE lexbuf.lex_start_p }

  | (stringprefix as prefix) '\'' {
      single_string (lexbuf.lex_start_p, lexbuf.lex_curr_p) prefix lexbuf
    }
  | (stringprefix as prefix) '"' {
      double_string (lexbuf.lex_start_p, lexbuf.lex_curr_p) prefix lexbuf
    }
  | (stringprefix as prefix) "'''" {
      single_long_string (lexbuf.lex_start_p, lexbuf.lex_curr_p) prefix (Buffer.create 17) lexbuf
    }
  | (stringprefix as prefix) "\"\"\"" {
      double_long_string (lexbuf.lex_start_p, lexbuf.lex_curr_p) prefix (Buffer.create 17) lexbuf
    }

  | comment { read_without_indent state lexbuf }
  | whitespace+ { read_without_indent state lexbuf }
  | eof { EOF }

and offset state = parse
  | empty {}
  | ' ' { state.offset <- state.offset + 1; offset state lexbuf }
  | '\t' { state.offset <- state.offset + 4; offset state lexbuf }

and single_string prefix_position prefix = parse
  | (([^ '\\' '\r' '\n' '\''] | escape)* as value) '\'' {
      let (prefix_start, prefix_stop) = prefix_position in
      let current_position = lexbuf.lex_curr_p in
      let content_stop = { current_position with pos_cnum = current_position.pos_cnum - 1 } in
      line_breaks lexbuf value;
      string (prefix_start, current_position) (prefix_stop, content_stop) prefix value
    }

and double_string prefix_position prefix = parse
  | (([^ '\\' '\r' '\n' '"'] | escape)* as value) '"' {
      let (prefix_start, prefix_stop) = prefix_position in
      let current_position = lexbuf.lex_curr_p in
      let content_stop = { current_position with pos_cnum = current_position.pos_cnum - 1 } in
      line_breaks lexbuf value;
      string (prefix_start, current_position) (prefix_stop, content_stop) prefix value
    }

and single_long_string prefix_position prefix buffer = parse
  | newline as value {
      line_break lexbuf;
      Buffer.add_string buffer value;
      single_long_string prefix_position prefix buffer lexbuf
    }
  | [^ '\\'] as value {
      Buffer.add_string buffer (Char.to_string value);
      single_long_string prefix_position prefix buffer lexbuf
    }
  | escape as value {
      line_breaks lexbuf value;
      Buffer.add_string buffer value;
      single_long_string prefix_position prefix buffer lexbuf
    }
  | "'''" {
      let (prefix_start, prefix_stop) = prefix_position in
      let current_position = lexbuf.lex_curr_p in
      let content_stop = { current_position with pos_cnum = current_position.pos_cnum - 3 } in
      string
        (prefix_start, current_position)
        (prefix_stop, content_stop)
        prefix
        (Buffer.contents buffer)
    }

and double_long_string prefix_position prefix buffer = parse
  | newline as value {
      line_break lexbuf;
      Buffer.add_string buffer value;
      double_long_string prefix_position prefix buffer lexbuf
    }
  | [^ '\\'] as value {
      Buffer.add_string buffer (Char.to_string value);
      double_long_string prefix_position prefix buffer lexbuf
    }
  | escape as value {
      line_breaks lexbuf value;
      Buffer.add_string buffer value;
      double_long_string prefix_position prefix buffer lexbuf
    }
  | "\"\"\"" {
      let (prefix_start, prefix_stop) = prefix_position in
      let current_position = lexbuf.lex_curr_p in
      let content_stop = { current_position with pos_cnum = current_position.pos_cnum - 3 } in
      string
        (prefix_start, current_position)
        (prefix_stop, content_stop)
        prefix
        (Buffer.contents buffer)
    }
