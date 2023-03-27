(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module contains all parsing APIs, i.e. functions that transfrom plain strings into a list
    of {!type: Ast.Statement.t}.

    Under the hood, it invokes errpy then transforms the output errpy AST (which matches CPython
    More details of ERRPY: https://github.com/facebook/errpy **)

open Base
module Errpyast = Errpy.Ast
module Errpyparser = Errpy.Parser
open Ast.Expression
open Ast.Location
open Ast.Statement
module Node = Ast.Node

let translate_comparison_operator = function
  | Errpyast.Eq -> ComparisonOperator.Equals
  | Errpyast.NotEq -> ComparisonOperator.NotEquals
  | Errpyast.Lt -> ComparisonOperator.LessThan
  | Errpyast.LtE -> ComparisonOperator.LessThanOrEquals
  | Errpyast.Gt -> ComparisonOperator.GreaterThan
  | Errpyast.GtE -> ComparisonOperator.GreaterThanOrEquals
  | Errpyast.Is -> ComparisonOperator.Is
  | Errpyast.IsNot -> ComparisonOperator.IsNot
  | Errpyast.In -> ComparisonOperator.In
  | Errpyast.NotIn -> ComparisonOperator.NotIn


let translate_binary_operator = function
  | Errpyast.Add -> "add"
  | Errpyast.Sub -> "sub"
  | Errpyast.Mult -> "mul"
  | Errpyast.MatMult -> "matmul"
  | Errpyast.Div -> "truediv"
  | Errpyast.Mod -> "mod"
  | Errpyast.Pow -> "pow"
  | Errpyast.LShift -> "lshift"
  | Errpyast.RShift -> "rshift"
  | Errpyast.BitOr -> "or"
  | Errpyast.BitXor -> "xor"
  | Errpyast.BitAnd -> "and"
  | Errpyast.FloorDiv -> "floordiv"


let translate_unary_operator = function
  | Errpyast.Invert -> UnaryOperator.Invert
  | Errpyast.Not -> UnaryOperator.Not
  | Errpyast.UAdd -> UnaryOperator.Positive
  | Errpyast.USub -> UnaryOperator.Negative


let translate_boolop = function
  | Errpyast.And -> BooleanOperator.And
  | Errpyast.Or -> BooleanOperator.Or


module StatementContext = struct
  type t = {
    (* [parent] holds the name of the immediate containing class of a statement. *)
    parent: Ast.Identifier.t option;
  }
end

let translate_alias (alias : Errpyast.alias) =
  let open Ast in
  let location =
    let end_lineno = Option.value alias.end_lineno ~default:alias.lineno in
    let end_col_offset = Option.value alias.end_col_offset ~default:alias.col_offset in
    {
      start = { line = alias.lineno; column = alias.col_offset };
      stop = { line = end_lineno; column = end_col_offset };
    }
  in
  Node.create
    ~location
    { Statement.Import.name = Reference.create alias.name; alias = alias.asname }


let rec translate_expression (expression : Errpyast.expr) =
  let translate_comprehension (comprehension : Errpyast.comprehension) =
    {
      Comprehension.Generator.target = translate_expression comprehension.target;
      iterator = translate_expression comprehension.iter;
      conditions = List.map ~f:translate_expression comprehension.ifs;
      async = comprehension.is_async;
    }
  in
  let expression_desc = expression.desc in
  let location =
    let end_lineno = Option.value expression.end_lineno ~default:expression.lineno in
    let end_col_offset = Option.value expression.end_col_offset ~default:expression.col_offset in
    {
      start = { line = expression.lineno; column = expression.col_offset };
      stop = { line = end_lineno; column = end_col_offset };
    }
  in
  match expression_desc with
  | Errpyast.Compare compare -> (
      let left = translate_expression compare.left in
      let ops = List.map ~f:translate_comparison_operator compare.ops in
      let comparators = List.map ~f:translate_expression compare.comparators in
      let f (sofar, last) (operator, next) =
        (* NOTE(jat): This is not 100% accurate since `last` is never evaluated more than once at
           runtime. But it's a fairly close approximation. *)
        let right =
          let { Node.location = { Ast.Location.start = last_start; _ }; _ } = last in
          let { Node.location = { Ast.Location.stop = next_stop; _ }; _ } = next in
          Expression.ComparisonOperator { ComparisonOperator.left = last; operator; right = next }
          |> Node.create ~location:{ Ast.Location.start = last_start; stop = next_stop }
        in
        let sofar =
          Expression.BooleanOperator
            { BooleanOperator.left = sofar; operator = BooleanOperator.And; right }
          |> Node.create ~location:{ location with stop = right.location.stop }
        in
        sofar, next
      in
      (* `ops` and `comparators` are guaranteed by Errpy parser to be of the same length. *)
      List.zip_exn ops comparators
      |> function
      | [] -> left
      | (operator, right) :: rest ->
          let { Node.location = { Ast.Location.stop = right_stop; _ }; _ } = right in
          let first_operand =
            Expression.ComparisonOperator { ComparisonOperator.left; operator; right }
            |> Node.create ~location:{ location with stop = right_stop }
          in
          let result, _ = List.fold ~init:(first_operand, right) ~f rest in
          result)
  | Errpyast.BoolOp boolop -> (
      let values = List.map ~f:translate_expression boolop.values in
      let op = translate_boolop boolop.op in
      match values with
      | [] ->
          (* ERRPY won't will give us empty boolean operands. Doing this just to be safe. *)
          let default_value =
            match op with
            | BooleanOperator.And -> Constant.True
            | BooleanOperator.Or -> Constant.False
          in
          Expression.Constant default_value |> Node.create ~location
      | [value] -> value
      | first :: second :: rest ->
          (* Boolean operators are left-associative *)
          let init =
            Expression.BooleanOperator
              { BooleanOperator.left = first; operator = op; right = second }
            |> Node.create ~location:{ location with stop = second.location.stop }
          in
          let f sofar next =
            let { Node.location = { Ast.Location.stop = next_stop; _ }; _ } = next in
            Expression.BooleanOperator { BooleanOperator.left = sofar; operator = op; right = next }
            |> Node.create ~location:{ location with stop = next_stop }
          in
          List.fold rest ~init ~f)
  | _ ->
      let as_ast_expression =
        match expression_desc with
        | Errpyast.BinOp binop ->
            let operator = Caml.Format.sprintf "__%s__" (translate_binary_operator binop.op) in
            let base = translate_expression binop.left in
            let callee =
              Expression.Name (Name.Attribute { base; attribute = operator; special = true })
              |> Node.create ~location:(Node.location base)
            in
            Expression.Call
              {
                callee;
                arguments =
                  [{ Call.Argument.name = None; value = translate_expression binop.right }];
              }
        | Errpyast.Name name -> Expression.Name (Name.Identifier name.id)
        | Errpyast.UnaryOp unaryop -> (
            let operand = translate_expression unaryop.operand in
            let operator = translate_unary_operator unaryop.op in
            match operator, operand with
            | ( UnaryOperator.Positive,
                { Node.value = Expression.Constant (Constant.Integer literal); _ } ) ->
                Expression.Constant (Constant.Integer literal)
            | ( UnaryOperator.Negative,
                { Node.value = Expression.Constant (Constant.Integer literal); _ } ) ->
                Expression.Constant (Constant.Integer (-literal))
            | _ -> Expression.UnaryOperator { UnaryOperator.operator; operand })
        | Errpyast.Attribute attribute ->
            let base = translate_expression attribute.value in
            Expression.Name (Name.Attribute { base; attribute = attribute.attr; special = false })
        | Errpyast.Constant constant ->
            let const =
              match constant.value with
              | None -> Constant.NoneLiteral
              | Some constant_desc -> (
                  match constant_desc with
                  | Errpyast.Ellipsis -> Constant.Ellipsis
                  | Errpyast.Bool bool -> if bool then Constant.True else Constant.False
                  | Errpyast.Str value ->
                      let open List in
                      let split_value = String.split ~on:'\'' value in
                      let just_string = nth_exn split_value (length split_value - 2) in
                      let is_bytes = String.contains (nth_exn split_value 0) 'b' in
                      Constant.String (StringLiteral.create ~bytes:is_bytes just_string)
                  | Errpyast.Num num -> (
                      match num with
                      | Int int -> Constant.Integer int
                      | Float float -> Constant.Float float
                      | Complex complex -> Constant.Complex complex
                      | Big_int bitint -> Constant.BigInteger bitint))
            in
            Expression.Constant const
        | Errpyast.Await expr -> Expression.Await (translate_expression expr)
        | Errpyast.YieldFrom expr -> Expression.YieldFrom (translate_expression expr)
        | Errpyast.Yield maybe_expr ->
            Expression.Yield (Option.map maybe_expr ~f:translate_expression)
        | Errpyast.Tuple tuple -> Expression.Tuple (List.map ~f:translate_expression tuple.elts)
        | Errpyast.List list -> Expression.List (List.map ~f:translate_expression list.elts)
        | Errpyast.Set set_items -> Expression.Set (List.map ~f:translate_expression set_items)
        | Errpyast.Dict { keys; values } ->
            let entries, keywords =
              (* `keys` and `values` are guaranteed by ERRPY parser to be of the same length. *)
              List.zip_exn keys values
              |> List.partition_map ~f:(fun (key, value) ->
                     match key with
                     | None -> Either.Second (translate_expression value)
                     | Some key ->
                         Either.First
                           {
                             Dictionary.Entry.key = translate_expression key;
                             value = translate_expression value;
                           })
            in
            Expression.Dictionary { Dictionary.entries; keywords }
        | Errpyast.IfExp ifexp ->
            Expression.Ternary
              {
                Ternary.target = translate_expression ifexp.body;
                test = translate_expression ifexp.test;
                alternative = translate_expression ifexp.orelse;
              }
        | Errpyast.NamedExpr walrus ->
            Expression.WalrusOperator
              {
                target = translate_expression walrus.target;
                value = translate_expression walrus.value;
              }
        | Errpyast.Starred starred ->
            Expression.Starred (Starred.Once (translate_expression starred.value))
        | Errpyast.Call call ->
            let arguments =
              List.append
                (List.map call.args ~f:convert_positional_argument)
                (List.map call.keywords ~f:convert_keyword_argument)
            in
            Expression.Call { callee = translate_expression call.func; arguments }
        | Errpyast.Subscript subscript ->
            let value = translate_expression subscript.value in
            let slice = translate_expression subscript.slice in
            let callee =
              let { Node.location = value_location; _ } = value in
              Expression.Name
                (Name.Attribute
                   { Name.Attribute.base = value; attribute = "__getitem__"; special = true })
              |> Node.create ~location:value_location
            in
            let arguments = [{ Call.Argument.name = None; value = slice }] in
            Expression.Call { callee; arguments }
        | Errpyast.Slice slice ->
            (* TODO(T101302994): We should avoid lowering slice expressions at parser phase. *)
            let callee = Expression.Name (Name.Identifier "slice") |> Node.create ~location in
            let arguments =
              let to_argument = function
                | None ->
                    Expression.Constant Constant.NoneLiteral
                    |> Node.create ~location:Ast.Location.any
                | Some expression -> translate_expression expression
              in
              [
                { Call.Argument.name = None; value = to_argument slice.lower };
                { Call.Argument.name = None; value = to_argument slice.upper };
                { Call.Argument.name = None; value = to_argument slice.step };
              ]
            in
            Expression.Call { callee; arguments }
        | Errpyast.GeneratorExp gennerator_expression ->
            Expression.Generator
              {
                Comprehension.element = translate_expression gennerator_expression.elt;
                generators = List.map ~f:translate_comprehension gennerator_expression.generators;
              }
        | Errpyast.ListComp list_comprehension ->
            Expression.ListComprehension
              {
                Comprehension.element = translate_expression list_comprehension.elt;
                generators = List.map ~f:translate_comprehension list_comprehension.generators;
              }
        | Errpyast.SetComp set_comprehension ->
            Expression.SetComprehension
              {
                Comprehension.element = translate_expression set_comprehension.elt;
                generators = List.map ~f:translate_comprehension set_comprehension.generators;
              }
        | Errpyast.DictComp dict_comprehension ->
            Expression.DictionaryComprehension
              {
                Comprehension.element =
                  {
                    Dictionary.Entry.key = translate_expression dict_comprehension.key;
                    value = translate_expression dict_comprehension.value;
                  };
                generators = List.map ~f:translate_comprehension dict_comprehension.generators;
              }
        | Errpyast.FormattedValue formatted_value ->
            Expression.FormatString [Substring.Format (translate_expression formatted_value.value)]
        | Errpyast.JoinedStr joined_string ->
            let values = List.map ~f:translate_expression joined_string in
            let collapse_formatted_value ({ Node.value; location } as expression) =
              match value with
              | Expression.Constant (Constant.String { StringLiteral.kind = String; value }) ->
                  Substring.Literal (Node.create ~location value)
              | Expression.FormatString [substring] -> substring
              | _ ->
                  (* NOTE: May be impossible for ERRPY to reach this branch *)
                  Substring.Format expression
            in
            Expression.FormatString (List.map values ~f:collapse_formatted_value)
        | Errpyast.Lambda _lambda -> failwith "not implemented yet"
        | _ -> failwith "not implemented yet"
      in
      as_ast_expression |> Node.create ~location


and convert_positional_argument value =
  { Ast.Expression.Call.Argument.name = None; value = translate_expression value }


and convert_keyword_argument (kw_argument : Errpyast.keyword) =
  let name = kw_argument.arg in
  let value = kw_argument.value in
  let value = translate_expression value in
  let location =
    let end_lineno = Option.value kw_argument.end_lineno ~default:kw_argument.lineno in
    let end_col_offset = Option.value kw_argument.end_col_offset ~default:kw_argument.col_offset in
    {
      start = { line = kw_argument.lineno; column = kw_argument.col_offset };
      stop = { line = end_lineno; column = end_col_offset };
    }
  in
  match name with
  | None ->
      (* CPython AST (and ERRPY) quirk: **arg is represented as keyword arg without a name. *)
      {
        Call.Argument.name = None;
        value = Expression.Starred (Starred.Twice value) |> Node.create ~location;
      }
  | Some name ->
      {
        Call.Argument.name =
          Some
            {
              value = name;
              location =
                {
                  location with
                  stop =
                    {
                      line = location.start.line;
                      column = location.start.column + String.length name;
                    };
                };
            };
        value;
      }


and translate_statements
    (statements : Errpyast.stmt list)
    ~context:({ StatementContext.parent = _parent; _ } as context)
  =
  let translate_statement (statement : Errpyast.stmt) =
    let statement_desc = statement.desc in
    let location =
      let end_lineno = Option.value statement.end_lineno ~default:statement.lineno in
      let end_col_offset = Option.value statement.end_col_offset ~default:statement.col_offset in
      {
        start = { line = statement.lineno; column = statement.col_offset };
        stop = { line = end_lineno; column = end_col_offset };
      }
    in
    let translate_excepthandler (excepthandler : Errpyast.excepthandler) =
      let excepthandler_desc = excepthandler.desc in
      match excepthandler_desc with
      | Errpyast.ExceptHandler handler ->
          let body = translate_statements handler.body ~context in
          let name = handler.name in
          let type_ = Option.map handler.type_ ~f:translate_expression in
          let handler_stop = location.stop in
          let new_name =
            match type_, name with
            | ( Some
                  {
                    Node.location =
                      { stop = { line = type_stop_line; column = type_stop_column }; _ };
                    _;
                  },
                Some name ) ->
                (* Stop at the beginning of body or end of handler if no body *)
                let name_stop =
                  match body with
                  | [] -> handler_stop
                  | statement :: _ -> (Node.location statement).start
                in
                Some
                  (Node.create
                     ~location:
                       {
                         (* Start " as " characters from end of expression type *)
                         start = { line = type_stop_line; column = type_stop_column + 4 };
                         stop = name_stop;
                       }
                     name)
            | _ -> None
          in
          { Ast.Statement.Try.Handler.kind = type_; name = new_name; body }
    in
    let as_ast_statement =
      match statement_desc with
      | Errpyast.Return expression ->
          let value = Option.map expression ~f:translate_expression in
          [Statement.Return { Return.expression = value; is_implicit = false }]
      | Errpyast.Raise raise ->
          let exc = Option.map raise.exc ~f:translate_expression in
          let cause = Option.map raise.cause ~f:translate_expression in
          [Statement.Raise { Raise.expression = exc; from = cause }]
      | Errpyast.Assert assert_statement ->
          let message = Option.map assert_statement.msg ~f:translate_expression in
          [
            Statement.Assert
              {
                Assert.test = translate_expression assert_statement.test;
                message;
                origin = Assert.Origin.Assertion;
              };
          ]
      | Errpyast.Import aliases ->
          [Statement.Import { Import.imports = List.map aliases ~f:translate_alias; from = None }]
      | Errpyast.ImportFrom import_from ->
          let dots =
            List.init (Option.value import_from.level ~default:0) ~f:(fun _ -> ".")
            |> String.concat ~sep:""
          in
          let from_module_name = Option.value import_from.module_ ~default:"" in
          let from_text = Caml.Format.sprintf "%s%s" dots from_module_name in
          let from = from_text |> Ast.Reference.create in
          let new_location =
            (* Add 5 characters for 'from ' *)
            {
              start = { line = location.start.line; column = location.start.column + 5 };
              stop =
                {
                  line = location.stop.line;
                  column = location.stop.column + 5 + String.length from_text;
                };
            }
          in
          [
            Statement.Import
              {
                Import.imports = List.map import_from.names ~f:translate_alias;
                from = Some (Node.create ~location:new_location from);
              };
          ]
      | Errpyast.For for_statement ->
          [
            Statement.For
              {
                For.target = translate_expression for_statement.target;
                iterator = translate_expression for_statement.iter;
                body = translate_statements for_statement.body ~context;
                orelse = translate_statements for_statement.orelse ~context;
                async = false;
              };
          ]
      | Errpyast.AsyncFor for_statement ->
          [
            Statement.For
              {
                For.target = translate_expression for_statement.target;
                iterator = translate_expression for_statement.iter;
                body = translate_statements for_statement.body ~context;
                orelse = translate_statements for_statement.orelse ~context;
                async = true;
              };
          ]
      | Errpyast.While while_statement ->
          [
            Statement.While
              {
                While.test = translate_expression while_statement.test;
                body = translate_statements while_statement.body ~context;
                orelse = translate_statements while_statement.orelse ~context;
              };
          ]
      | Errpyast.If if_statement ->
          [
            Statement.If
              {
                If.test = translate_expression if_statement.test;
                body = translate_statements if_statement.body ~context;
                orelse = translate_statements if_statement.orelse ~context;
              };
          ]
      | Errpyast.Try try_statement ->
          [
            Statement.Try
              {
                Try.body = translate_statements try_statement.body ~context;
                orelse = translate_statements try_statement.orelse ~context;
                finally = translate_statements try_statement.finalbody ~context;
                handlers = List.map ~f:translate_excepthandler try_statement.handlers;
              };
          ]
      | Errpyast.With _with_statement -> failwith "not implemented yet"
      | Errpyast.AsyncWith _with_statement -> failwith "not implemented yet"
      | Errpyast.AnnAssign _ann_assign -> failwith "not implemented yet"
      | Errpyast.AugAssign _aug_assign -> failwith "not implemented yet"
      | Errpyast.Assign _assign -> failwith "not implemented yet"
      | Errpyast.FunctionDef _function_def -> failwith "not implemented yet"
      | Errpyast.AsyncFunctionDef _async_function_def -> failwith "not implemented yet"
      | Errpyast.Delete targets -> [Statement.Delete (List.map targets ~f:translate_expression)]
      | Errpyast.Global names -> [Statement.Global names]
      | Errpyast.Nonlocal names -> [Statement.Nonlocal names]
      | Errpyast.Pass -> [Statement.Pass]
      | Errpyast.Break -> [Statement.Break]
      | Errpyast.Continue -> [Statement.Continue]
      | Errpyast.Expr expression -> [Statement.Expression (translate_expression expression)]
      | Errpyast.ClassDef _class_def -> failwith "not implemented yet"
      | Errpyast.Match _match -> failwith "not implemented yet"
    in
    let make_node statement = statement |> Node.create ~location in
    List.map ~f:make_node as_ast_statement
  in
  List.concat (List.map ~f:translate_statement statements)


let translate_module errpy_module =
  match errpy_module with
  | Errpyast.Module { body; _ } ->
      translate_statements body ~context:{ StatementContext.parent = None }
  | _ -> []


let parse_module text =
  let open Result in
  let format_recoverable_errors recoverable_errors =
    recoverable_errors
    |> List.map ~f:Errpyast.show_recoverableerrorwithlocation
    |> String.concat ~sep:", "
    |> Format.asprintf "[%s]"
  in
  match Errpyparser.parse_module text with
  | Ok (module_, recoverable_errors) -> (
      match recoverable_errors with
      | [] -> Ok (translate_module module_)
      | _syntax_errors -> Result.Error (format_recoverable_errors recoverable_errors))
  | Error error -> Result.Error error
