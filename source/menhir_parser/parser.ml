(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module provides the top-level interface to Pyre's original Menhir parser. Pyre mostly uses
   other parsers (either an ocaml wrapper around the CPython parser), but Pysa models still use this
   parser and for the moment there are still a few places where Pyre uses this parser to parse
   individual expressions *)

open Core
open Pyre
open Ast

module Error = struct
  type t = {
    location: Location.t;
    file_name: string;
    content: string option;
  }
  [@@deriving equal]

  let pp formatter { location; file_name; content } =
    let column = location.Location.start.Location.column in
    let header = Format.asprintf "Could not parse file at %s:%a" file_name Location.pp location in
    match content with
    | Some content ->
        let indicator =
          if column > 0 then
            String.make (column - 1) ' ' ^ "^"
          else
            "^"
        in
        Format.fprintf formatter "%s\n  %s\n  %s" header content indicator
    | None -> Format.fprintf formatter "%s" header


  let show = Format.asprintf "%a" pp
end

exception Error of Error.t

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


let parse_symbol ~generator_parse_function ~start_line ~start_column ~file_name input =
  let buffer =
    let buffer = Lexing.from_string input in
    buffer.Lexing.lex_curr_p <-
      { Lexing.pos_fname = file_name; pos_lnum = start_line; pos_bol = -start_column; pos_cnum = 0 };
    buffer
  in
  let state = Lexer.State.initial () in
  try Result.Ok (generator_parse_function (Lexer.read state) buffer) with
  | Error error -> Result.Error { error with Error.file_name }
  | Generator.Error
  | Failure _ ->
      let location =
        Location.create ~start:buffer.Lexing.lex_curr_p ~stop:buffer.Lexing.lex_curr_p
      in
      let line_number = location.Location.start.Location.line - 1 in
      let content = List.nth (String.split ~on:'\n' input) line_number in
      Result.Error { Error.location; file_name; content }


(* Transform parsed expressions and statements into their AST.
 *
 * This can throw `Error` exceptions when parsing format strings.
 *)
module ParserToAst = struct
  open ParserExpression
  open ParserStatement
  module AstExpression = Ast.Expression
  module AstStatement = Ast.Statement

  let rec expand_format_string substrings =
    let module State = struct
      type kind =
        | Literal
        | Expression

      type t = {
        kind: kind;
        start_line: int;
        start_column: int;
        content: string;
      }
    end
    in
    let module Split = struct
      type t =
        | Expression of {
            start_line: int;
            start_column: int;
            content: string;
          }
        | Literal of string Node.t
    end
    in
    let expand_substring
        sofar
        {
          Substring.kind;
          location = { Location.start = { Location.line; column; _ }; _ } as location;
          value;
        }
      =
      match kind with
      | Substring.Kind.Literal ->
          AstExpression.Substring.Literal (Node.create ~location value) :: sofar
      | Substring.Kind.RawFormat ->
          let parse ~sofar = function
            | Split.Literal literal -> AstExpression.Substring.Literal literal :: sofar
            | Split.Expression { start_line; start_column; content } -> (
                let string = Format.sprintf "(%s)" content in
                let start_column = start_column - 1 in
                match
                  parse_symbol
                    ~generator_parse_function:Generator.parse_expression
                    ~start_line
                    ~start_column
                    ~file_name:""
                    string
                with
                | Ok expression ->
                    AstExpression.Substring.Format
                      { value = convert_expression expression; format_spec = None }
                    :: sofar
                | Error error -> raise (Error error))
          in
          let value_length = String.length value in
          let rec expand_fstring input_string ~sofar ~line_offset ~column_offset ~index state =
            if index = value_length then
              match state with
              | { State.kind = State.Literal; content = ""; _ } ->
                  (* This means the expansion ends cleanly. *)
                  sofar
              | { State.kind = State.Expression; _ } ->
                  (* The fstring contatins unclosed an curly brace, which is malformed. *)
                  sofar
              | { State.kind = State.Literal; start_line = line; start_column = column; content } ->
                  (* This means the fstring ends with some literal characters as opposed to an
                     expression. *)
                  let split =
                    let location =
                      {
                        Location.start = { Location.line; column };
                        stop = { Location.line = line_offset; column = column_offset };
                      }
                    in
                    Split.Literal (Node.create content ~location)
                  in
                  parse ~sofar split
            else
              let token = input_string.[index] in
              let sofar, next_state =
                match token, state with
                | '{', { State.kind = State.Literal; content = ""; _ } ->
                    ( sofar,
                      {
                        State.kind = State.Expression;
                        start_line = line_offset;
                        start_column = column_offset + 1;
                        content = "";
                      } )
                | '{', { State.kind = State.Literal; start_line; start_column; content } ->
                    let split =
                      let location =
                        {
                          Location.start = { Location.line = start_line; column = start_column };
                          stop = { Location.line = line_offset; column = column_offset };
                        }
                      in
                      Split.Literal (Node.create content ~location)
                    in
                    ( parse ~sofar split,
                      {
                        State.kind = State.Expression;
                        start_line = line_offset;
                        start_column = column_offset + 1;
                        content = "";
                      } )
                | '{', { State.kind = State.Expression; start_line; start_column; content = "" } ->
                    sofar, { State.kind = State.Literal; start_line; start_column; content = "{{" }
                (* NOTE: this does not account for nested expressions in e.g. format specifiers. *)
                | '}', { State.kind = State.Expression; start_line; start_column; content } ->
                    let split = Split.Expression { start_line; start_column; content } in
                    ( parse ~sofar split,
                      {
                        State.kind = State.Literal;
                        start_line = line_offset;
                        start_column = column_offset + 1;
                        content = "";
                      } )
                (* Ignore leading whitespace in expressions. *)
                | (' ' | '\t'), ({ State.kind = State.Expression; content = ""; _ } as expression)
                  ->
                    sofar, expression
                | _, { State.kind = State.Literal; start_line; start_column; content } ->
                    ( sofar,
                      {
                        State.kind = State.Literal;
                        start_line;
                        start_column;
                        content = content ^ Char.to_string token;
                      } )
                | _, { State.kind = State.Expression; start_line; start_column; content } ->
                    ( sofar,
                      {
                        State.kind = State.Expression;
                        start_line;
                        start_column;
                        content = content ^ Char.to_string token;
                      } )
              in
              let line_offset, column_offset =
                match token with
                | '\n' -> line_offset + 1, 0
                | _ -> line_offset, column_offset + 1
              in
              expand_fstring
                input_string
                ~sofar
                ~line_offset
                ~column_offset
                ~index:(index + 1)
                next_state
          in
          expand_fstring
            value
            ~sofar
            ~line_offset:line
            ~column_offset:column
            ~index:0
            { State.kind = State.Literal; start_line = line; start_column = column; content = "" }
    in
    List.fold substrings ~init:[] ~f:expand_substring |> List.rev


  and convert_expression { Node.location; value } =
    let convert_entry entry =
      let open Dictionary.Entry in
      match entry with
      | KeyValue KeyValue.{ key; value } ->
          AstExpression.Dictionary.Entry.KeyValue
            AstExpression.Dictionary.Entry.KeyValue.
              { key = convert_expression key; value = convert_expression value }
      | Splat s -> AstExpression.Dictionary.Entry.Splat (convert_expression s)
    in
    let convert_generator { Comprehension.Generator.target; iterator; conditions; async } =
      {
        AstExpression.Comprehension.Generator.target = convert_expression target;
        iterator = convert_expression iterator;
        conditions = List.map ~f:convert_expression conditions;
        async;
      }
    in
    let open Expression in
    match value with
    | Await expression ->
        AstExpression.Expression.Await (convert_expression expression) |> Node.create ~location
    | BinaryOperator { BinaryOperator.left; operator; right } ->
        AstExpression.Expression.BinaryOperator
          {
            AstExpression.BinaryOperator.left = convert_expression left;
            operator;
            right = convert_expression right;
            origin = None;
          }
        |> Node.create ~location
    | BooleanOperator { BooleanOperator.left; operator; right } ->
        AstExpression.Expression.BooleanOperator
          {
            AstExpression.BooleanOperator.left = convert_expression left;
            operator;
            right = convert_expression right;
            origin = None;
          }
        |> Node.create ~location
    | Call { Call.callee; arguments } ->
        AstExpression.Expression.Call
          {
            AstExpression.Call.callee = convert_expression callee;
            arguments = List.map ~f:convert_argument arguments;
            origin = None;
          }
        |> Node.create ~location
    | ComparisonOperator { ComparisonOperator.left; operator; right } ->
        AstExpression.Expression.ComparisonOperator
          {
            AstExpression.ComparisonOperator.left = convert_expression left;
            operator;
            right = convert_expression right;
            origin = None;
          }
        |> Node.create ~location
    | Constant value -> AstExpression.Expression.Constant value |> Node.create ~location
    | Dictionary entries ->
        AstExpression.Expression.Dictionary (List.map ~f:convert_entry entries)
        |> Node.create ~location
    | DictionaryComprehension
        { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators } ->
        AstExpression.Expression.DictionaryComprehension
          {
            AstExpression.Comprehension.element =
              AstExpression.Dictionary.Entry.KeyValue.
                { key = convert_expression key; value = convert_expression value };
            generators = List.map ~f:convert_generator generators;
          }
        |> Node.create ~location
    | Generator { Comprehension.element; generators } ->
        AstExpression.Expression.Generator
          {
            AstExpression.Comprehension.element = convert_expression element;
            generators = List.map ~f:convert_generator generators;
          }
        |> Node.create ~location
    | FormatString substrings ->
        AstExpression.Expression.FormatString (expand_format_string substrings)
        |> Node.create ~location
    | Lambda { Lambda.parameters; body } ->
        AstExpression.Expression.Lambda
          {
            AstExpression.Lambda.parameters = List.map ~f:convert_parameter parameters;
            body = convert_expression body;
          }
        |> Node.create ~location
    | List expression_list ->
        AstExpression.Expression.List (List.map ~f:convert_expression expression_list)
        |> Node.create ~location
    | ListComprehension { Comprehension.element; generators } ->
        AstExpression.Expression.ListComprehension
          {
            AstExpression.Comprehension.element = convert_expression element;
            generators = List.map ~f:convert_generator generators;
          }
        |> Node.create ~location
    | Name (Name.Attribute { Name.Attribute.base; attribute }) ->
        AstExpression.Expression.Name
          (AstExpression.Name.Attribute
             {
               AstExpression.Name.Attribute.base = convert_expression base;
               attribute;
               origin = None;
             })
        |> Node.create ~location
    | Name (Name.Identifier name) ->
        AstExpression.Expression.Name (AstExpression.Name.Identifier name) |> Node.create ~location
    | Parenthesis expression -> convert_expression expression
    | Set expression_list ->
        AstExpression.Expression.Set (List.map ~f:convert_expression expression_list)
        |> Node.create ~location
    | SetComprehension { Comprehension.element; generators } ->
        AstExpression.Expression.SetComprehension
          {
            AstExpression.Comprehension.element = convert_expression element;
            generators = List.map ~f:convert_generator generators;
          }
        |> Node.create ~location
    | Starred (Starred.Once expression) ->
        AstExpression.Expression.Starred
          (AstExpression.Starred.Once (convert_expression expression))
        |> Node.create ~location
    | Starred (Starred.Twice expression) ->
        AstExpression.Expression.Starred
          (AstExpression.Starred.Twice (convert_expression expression))
        |> Node.create ~location
    | Slice { Slice.start; stop; step } ->
        AstExpression.Expression.Slice
          {
            AstExpression.Slice.start = start >>| convert_expression;
            stop = stop >>| convert_expression;
            step = step >>| convert_expression;
          }
        |> Node.create ~location
    | Subscript { Subscript.base; index } ->
        AstExpression.Expression.Subscript
          {
            AstExpression.Subscript.base = convert_expression base;
            index = convert_expression index;
            origin = None;
          }
        |> Node.create ~location
    | Ternary { Ternary.target; test; alternative } ->
        AstExpression.Expression.Ternary
          {
            AstExpression.Ternary.target = convert_expression target;
            test = convert_expression test;
            alternative = convert_expression alternative;
          }
        |> Node.create ~location
    | Tuple expression_list ->
        AstExpression.Expression.Tuple (List.map ~f:convert_expression expression_list)
        |> Node.create ~location
    | UnaryOperator { UnaryOperator.operator; operand } ->
        AstExpression.Expression.UnaryOperator
          {
            AstExpression.UnaryOperator.operator;
            operand = convert_expression operand;
            origin = None;
          }
        |> Node.create ~location
    | WalrusOperator { WalrusOperator.target; value } ->
        AstExpression.Expression.WalrusOperator
          {
            AstExpression.WalrusOperator.target = convert_expression target;
            value = convert_expression value;
          }
        |> Node.create ~location
    | Yield expression ->
        AstExpression.Expression.Yield (expression >>| convert_expression) |> Node.create ~location
    | YieldFrom expression ->
        AstExpression.Expression.YieldFrom (expression |> convert_expression)
        |> Node.create ~location


  and convert_argument { Call.Argument.name; value } =
    { AstExpression.Call.Argument.name; value = convert_expression value }


  and convert_parameter { Node.location; value = { Parameter.name; value; annotation } } =
    {
      AstExpression.Parameter.name;
      value = value >>| convert_expression;
      annotation = annotation >>| convert_expression;
    }
    |> Node.create ~location


  and convert_statement ~parent { Node.location; value } =
    let value =
      let open Statement in
      match value with
      | Assign { Assign.target; annotation; value } ->
          AstStatement.Statement.Assign
            {
              AstStatement.Assign.target = convert_expression target;
              annotation = annotation >>| convert_expression;
              value = value >>| convert_expression;
            }
      | Assert { Assert.test; message } ->
          AstStatement.Statement.Assert
            {
              AstStatement.Assert.test = convert_expression test;
              message = message >>| convert_expression;
              origin = None;
            }
      | AugmentedAssign { AugmentedAssign.target; operator; value } ->
          AstStatement.Statement.AugmentedAssign
            {
              AstStatement.AugmentedAssign.target = convert_expression target;
              operator;
              value = convert_expression value;
            }
      | Break -> AstStatement.Statement.Break
      | Class { Class.name; base_arguments; body; decorators } ->
          let body =
            let parent = NestingContext.create_class ~parent (Reference.show name) in
            List.map ~f:(convert_statement ~parent) body
          in
          AstStatement.Statement.Class
            {
              AstStatement.Class.name;
              base_arguments = List.map ~f:convert_argument base_arguments;
              parent;
              body;
              decorators = List.map ~f:convert_expression decorators;
              top_level_unbound_names = [];
              type_params = [];
            }
      | Continue -> AstStatement.Statement.Continue
      | Define { Define.signature; body } ->
          let body =
            let { ParserStatement.Define.Signature.name; _ } = signature in
            let parent = NestingContext.create_function ~parent (Reference.show name) in
            List.map ~f:(convert_statement ~parent) body
          in
          let convert_signature
              {
                ParserStatement.Define.Signature.name;
                parameters;
                decorators;
                return_annotation;
                async;
              }
            =
            let legacy_parent =
              match parent with
              | Ast.NestingContext.Class { name; _ } -> Some (Ast.Reference.create name)
              | _ -> None
            in
            {
              AstStatement.Define.Signature.name;
              parameters = List.map ~f:convert_parameter parameters;
              decorators = List.map ~f:convert_expression decorators;
              return_annotation = return_annotation >>| convert_expression;
              async;
              generator = Ast.Statement.is_generator body;
              parent;
              legacy_parent;
              type_params = [];
            }
          in
          AstStatement.Statement.Define
            {
              AstStatement.Define.signature = convert_signature signature;
              captures = [];
              unbound_names = [];
              body;
            }
      | Delete expressions ->
          AstStatement.Statement.Delete (List.map ~f:convert_expression expressions)
      | Expression expression -> AstStatement.Statement.Expression (convert_expression expression)
      | For { For.target; iterator; body; orelse; async } ->
          AstStatement.Statement.For
            {
              AstStatement.For.target = convert_expression target;
              iterator = convert_expression iterator;
              body = List.map ~f:(convert_statement ~parent) body;
              orelse = List.map ~f:(convert_statement ~parent) orelse;
              async;
            }
      | Global identifiers -> AstStatement.Statement.Global identifiers
      | If { If.test; body; orelse } ->
          AstStatement.Statement.If
            {
              AstStatement.If.test = convert_expression test;
              body = List.map ~f:(convert_statement ~parent) body;
              orelse = List.map ~f:(convert_statement ~parent) orelse;
            }
      | Import { Import.from = None; imports } ->
          AstStatement.Statement.Import { AstStatement.Import.from = None; imports }
      | Import { Import.from = Some from; imports } ->
          let new_location =
            match location with
            | { Location.start = { Location.line; column }; _ } ->
                (* Add 5 characters for 'from ' *)
                {
                  Location.start = { Location.line; column = column + 5 };
                  stop =
                    {
                      Location.line;
                      column = column + 5 + String.length ([%show: Reference.t] from);
                    };
                }
          in
          AstStatement.Statement.Import
            { AstStatement.Import.from = Some (Node.create ~location:new_location from); imports }
      | Nonlocal identifiers -> AstStatement.Statement.Nonlocal identifiers
      | Pass -> AstStatement.Statement.Pass
      | Raise { Raise.expression; from } ->
          AstStatement.Statement.Raise
            {
              AstStatement.Raise.expression = expression >>| convert_expression;
              from = from >>| convert_expression;
            }
      | Return { Return.is_implicit; expression } ->
          AstStatement.Statement.Return
            { AstStatement.Return.is_implicit; expression = expression >>| convert_expression }
      | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
          let convert_handler { ParserStatement.Try.Handler.kind; name; body } =
            {
              AstStatement.Try.Handler.kind = kind >>| convert_expression;
              name;
              body = List.map ~f:(convert_statement ~parent) body;
            }
          in
          AstStatement.Statement.Try
            {
              AstStatement.Try.body = List.map ~f:(convert_statement ~parent) body;
              handlers = List.map ~f:convert_handler handlers;
              orelse = List.map ~f:(convert_statement ~parent) orelse;
              finally = List.map ~f:(convert_statement ~parent) finally;
              handles_exception_group;
            }
      | With { With.items; body; async } ->
          let convert_item (resource, target) =
            convert_expression resource, target >>| convert_expression
          in
          AstStatement.Statement.With
            {
              AstStatement.With.items = List.map ~f:convert_item items;
              body = List.map ~f:(convert_statement ~parent) body;
              async;
            }
      | While { While.test; body; orelse } ->
          AstStatement.Statement.While
            {
              AstStatement.While.test = convert_expression test;
              body = List.map ~f:(convert_statement ~parent) body;
              orelse = List.map ~f:(convert_statement ~parent) orelse;
            }
    in
    Node.create ~location value


  let convert_module statements =
    let parent = NestingContext.create_toplevel () in
    List.map statements ~f:(convert_statement ~parent)
end

let parse ?start_line ?start_column ?relative lines =
  let input = sanitize_input lines in
  let start_line = Option.value start_line ~default:1 in
  let start_column = Option.value start_column ~default:0 in
  let file_name = Option.value relative ~default:"$invalid_path" in
  match
    parse_symbol
      ~generator_parse_function:Generator.parse_module
      ~start_line
      ~start_column
      ~file_name
      input
  with
  | Result.Error _ as e -> e
  | Result.Ok statements -> (
      try Result.Ok (ParserToAst.convert_module statements) with
      | Error error -> Result.Error { error with Error.file_name })


let parse_exn ?start_line ?start_column ?relative lines =
  match parse ?start_line ?start_column ?relative lines with
  | Ok statements -> statements
  | Error error -> raise (Error error)
