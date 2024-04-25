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
open Pyre

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
  let is_byte_prefix prefix =
    String.contains prefix 'b' || String.contains prefix 'B' in
  let is_format_prefix prefix =
    String.contains prefix 'f' || String.contains prefix 'F' in
  if is_byte_prefix prefix then
    BYTES (string_position, content_position, value)
  else if is_format_prefix prefix then
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

let star_type_prefix_regex = Str.regexp "\\*"

let parse_signature_comment comment =
  let strip character string =
    string
    |> String.lstrip ~drop:character
    |> String.rstrip ~drop:character
  in
  let return_annotation =
    let quote char = Char.equal char '\'' || Char.equal char '"' in
    comment
    |> Str.split (Str.regexp "-> *")
    |> fun elements -> List.nth_exn elements 1
                       |> strip quote
  in
  let parameter_annotations =
    let space char = Char.equal char ' ' || Char.equal char ',' in
    let split_annotations annotations_string =
      let rec split ~sofar ~next ~open_brackets characters =
        let reverse_stringify character_list =
          character_list
          |> List.rev
          |> String.of_char_list
        in
        if open_brackets < 0 then
          []
        else
          match characters with
          | '[' :: remaining ->
              split ~sofar ~next:('[' :: next) ~open_brackets:(open_brackets + 1) remaining
          | ']' :: remaining ->
              split ~sofar ~next:(']' :: next) ~open_brackets:(open_brackets - 1) remaining
          | ',' :: remaining when open_brackets = 0 ->
              split ~sofar:((reverse_stringify next) :: sofar) ~next:[] ~open_brackets remaining
          | character :: remaining ->
              split ~sofar ~next:(character :: next) ~open_brackets remaining
          | [] ->
              (reverse_stringify next) :: sofar
      in
      split ~sofar:[] ~next:[] ~open_brackets:0 (String.to_list annotations_string)
      (* Handle parsing *type and **type annotation in python 2 type comment. *)
      |> List.map ~f:(Str.global_substitute star_type_prefix_regex (fun _ -> ""))
      |> List.rev
    in
    let is_not_empty annotation_string =
      Str.string_match (Str.regexp "\\.*") annotation_string 0
      |> ignore;
      not (Str.match_end () = String.length annotation_string)
    in
    comment
    |> Str.split (Str.regexp ") *->")
    |> fun elements -> List.nth elements 0
    >>| Str.split (Str.regexp ": *(")
    >>= (fun elements -> List.nth elements 1)
    >>| split_annotations
    >>| List.map ~f:(strip space)
    >>| List.filter ~f:is_not_empty
                       |> Option.value ~default:[]
  in
  (parameter_annotations, return_annotation)
}

let empty = ""

let newline = ("\r\n" | '\n')
let whitespace = [' ' '\t']
let comment = '#' [^ '\n' '\r']*

let signature = '#' whitespace* "type:" whitespace* "("
  (['a'-'z' 'A'-'Z' ' ' '*' ',' '_' '[' ']' '.' '0'-'9']+)*
  ")" whitespace* "->" whitespace* [^ '\n' '\r']+

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
  | whitespace* comment newline whitespace* signature {
      line_break lexbuf;
      let parameters, return = parse_signature_comment (lexeme lexbuf) in
      SIGNATURE_COMMENT ((lexbuf.lex_start_p, lexbuf.lex_curr_p), parameters, return)
    }
  | newline whitespace* signature {
      line_break lexbuf;
      let parameters, return = parse_signature_comment (lexeme lexbuf) in
      SIGNATURE_COMMENT ((lexbuf.lex_start_p, lexbuf.lex_curr_p), parameters, return)
    }
  | whitespace* signature {
      let parameters, return = parse_signature_comment (lexeme lexbuf) in
      SIGNATURE_COMMENT ((lexbuf.lex_start_p, lexbuf.lex_curr_p), parameters, return)
    }
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
      | "as" -> AS
      | "assert" -> ASSERT lexbuf.lex_start_p
      | "async" -> ASYNC (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "await" -> AWAIT lexbuf.lex_start_p
      | "break" -> BREAK (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "class" -> CLASS lexbuf.lex_start_p
      | "continue" -> CONTINUE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "def" -> DEFINE lexbuf.lex_start_p
      | "del" -> DELETE lexbuf.lex_start_p
      | "elif" -> ELSEIF lexbuf.lex_start_p
      | "else" -> ELSE
      | "except" -> EXCEPT lexbuf.lex_start_p
      | "False" -> FALSE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "finally" -> FINALLY
      | "for" -> FOR lexbuf.lex_start_p
      | "from" -> FROM lexbuf.lex_start_p
      | "global" -> GLOBAL lexbuf.lex_start_p
      | "if" -> IF lexbuf.lex_start_p
      | "import" -> IMPORT lexbuf.lex_start_p
      | "in" -> IN
      | "is" -> IS
      | "lambda" -> LAMBDA lexbuf.lex_start_p
      | "None" -> NONE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "nonlocal" -> NONLOCAL lexbuf.lex_start_p
      | "not" -> NOT lexbuf.lex_start_p
      | "or" -> OR
      | "pass" -> PASS (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "raise" -> RAISE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "return" -> RETURN (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "True" -> TRUE (lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | "try" -> TRY lexbuf.lex_start_p
      | "with" -> WITH lexbuf.lex_start_p
      | "while" -> WHILE lexbuf.lex_start_p
      | "yield" -> YIELD (lexbuf.lex_start_p, lexbuf.lex_curr_p)
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

  | whitespace* '#' whitespace* "type" whitespace* ':' whitespace* "ignore" [^ '\n' '\r']* {
      read_without_indent state lexbuf
    }
  | whitespace* '#' whitespace* "type" whitespace* ':' whitespace* [^ '\n' '\r']* {
      let annotation_string = lexeme lexbuf in
      let comment_regex = Str.regexp ".*: *" in
      let annotation =
        annotation_string
        |> String.split ~on:':'
        |> List.tl
        >>| String.concat ~sep:":"
        >>| (fun string -> String.strip string)
        |> Option.value ~default:"$unknown"
      in
      let offset =
        Str.string_match comment_regex annotation_string 0 |> ignore;
        String.length (Str.matched_string annotation_string)
      in
      let start = lexbuf.lex_start_p in
      ANNOTATION_COMMENT (
        (
          { start with pos_cnum = start.pos_cnum + offset },
          lexbuf.lex_curr_p
        ),
        annotation
      )
    }
  | "..." { ELLIPSES (lexbuf.lex_start_p, lexbuf.lex_curr_p) }

  | '.' { DOT (lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  | '!' { EXCLAMATIONMARK }
  | "%=" { PERCENTEQUALS }
  | '%' { PERCENT }
  | "&=" { AMPERSANDEQUALS }
  | '&' { AMPERSAND }
  | '(' {
      state.nesting <- state.nesting + 1;
      LEFTPARENS lexbuf.lex_start_p
    }
  | ')' {
      state.nesting <- state.nesting - 1;
      RIGHTPARENS lexbuf.lex_curr_p
    }
  | "**=" { ASTERIKSASTERIKSEQUALS }
  | "*=" { ASTERIKSEQUALS }
  | '*' { ASTERIKS (lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  | "@=" { ATEQUALS }
  | '@' { AT }
  | "+=" { PLUSEQUALS }
  | '+' { PLUS lexbuf.lex_start_p }
  | ',' { COMMA }
  | ":=" { COLONEQUALS }
  | "-=" { MINUSEQUALS }
  | '-' { MINUS lexbuf.lex_start_p }
  | "//=" { SLASHSLASHEQUALS }
  | "/=" { SLASHEQUALS }
  | '/' { SLASH (lexbuf.lex_start_p) }
  | ':' { COLON lexbuf.lex_start_p }
  | "^=" { HATEQUALS }
  | '^' { HAT }
  | ';' { SEMICOLON }
  | "<<=" { LEFTANGLELEFTANGLEEQUALS }
  | "<<" { LEFTANGLELEFTANGLE }
  | "<=" { LEFTANGLEEQUALS }
  | '<' { LEFTANGLE }
  | "==" { DOUBLEEQUALS }
  | '=' { EQUALS }
  | ">>=" { RIGHTANGLERIGHTANGLEEQUALS }
  | ">>" { RIGHTANGLERIGHTANGLE }
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
  | '{' {
      state.nesting <- state.nesting + 1;
      LEFTCURLY lexbuf.lex_start_p
    }
  | "|=" { BAREQUALS }
  | '|' { BAR }
  | '}' {
      state.nesting <- state.nesting - 1;
      RIGHTCURLY lexbuf.lex_curr_p
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
