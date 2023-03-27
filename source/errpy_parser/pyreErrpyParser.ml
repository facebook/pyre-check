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

let _translate_comparison_operator = function
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


let _translate_binary_operator = function
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

let rec translate_expression (expression : Errpyast.expr) =
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
  | Errpyast.Compare _compare -> failwith "not implemented yet"
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
        | Errpyast.BinOp _binop -> failwith "not implemented yet"
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
        | Errpyast.Tuple _tuple -> failwith "not implemented yet"
        | Errpyast.List _list -> failwith "not implemented yet"
        | Errpyast.Set _set_items -> failwith "not implemented yet"
        | Errpyast.Dict _ -> failwith "not implemented yet"
        | Errpyast.IfExp _ifexp -> failwith "not implemented yet"
        | Errpyast.NamedExpr _walrus -> failwith "not implemented yet"
        | Errpyast.Starred _starred -> failwith "not implemented yet"
        | Errpyast.Call _call -> failwith "not implemented yet"
        | Errpyast.Subscript _subscript -> failwith "not implemented yet"
        | Errpyast.Slice _slice -> failwith "not implemented yet"
        | Errpyast.GeneratorExp _gennerator_expression -> failwith "not implemented yet"
        | Errpyast.ListComp _list_comprehension -> failwith "not implemented yet"
        | Errpyast.SetComp _set_comprehension -> failwith "not implemented yet"
        | Errpyast.DictComp _dict_comprehension -> failwith "not implemented yet"
        | Errpyast.FormattedValue _formatted_value -> failwith "not implemented yet"
        | Errpyast.JoinedStr _joined_string -> failwith "not implemented yet"
        | Errpyast.Lambda _lambda -> failwith "not implemented yet"
        | _ -> failwith "not implemented yet"
      in
      as_ast_expression |> Node.create ~location


and translate_statements
    (statements : Errpyast.stmt list)
    ~context:({ StatementContext.parent = _parent; _ } as _context)
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
    let as_ast_statement =
      match statement_desc with
      | Errpyast.Return _expression -> failwith "not implemented yet"
      | Errpyast.Raise _raise -> failwith "not implemented yet"
      | Errpyast.Assert _assert_statement -> failwith "not implemented yet"
      | Errpyast.Import _aliases -> failwith "not implemented yet"
      | Errpyast.ImportFrom _import_from -> failwith "not implemented yet"
      | Errpyast.For _for_statement -> failwith "not implemented yet"
      | Errpyast.AsyncFor _for_statement -> failwith "not implemented yet"
      | Errpyast.While _while_statement -> failwith "not implemented yet"
      | Errpyast.If _if_statement -> failwith "not implemented yet"
      | Errpyast.Try _try_statement -> failwith "not implemented yet"
      | Errpyast.With _with_statement -> failwith "not implemented yet"
      | Errpyast.AsyncWith _with_statement -> failwith "not implemented yet"
      | Errpyast.AnnAssign _ann_assign -> failwith "not implemented yet"
      | Errpyast.AugAssign _aug_assign -> failwith "not implemented yet"
      | Errpyast.Assign _assign -> failwith "not implemented yet"
      | Errpyast.FunctionDef _function_def -> failwith "not implemented yet"
      | Errpyast.AsyncFunctionDef _async_function_def -> failwith "not implemented yet"
      | Errpyast.Delete _targets -> failwith "not implemented yet"
      | Errpyast.Global _names -> failwith "not implemented yet"
      | Errpyast.Nonlocal _names -> failwith "not implemented yet"
      | Errpyast.Pass -> failwith "not implemented yet"
      | Errpyast.Break -> failwith "not implemented yet"
      | Errpyast.Continue -> failwith "not implemented yet"
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
