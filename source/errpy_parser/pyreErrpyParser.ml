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

module SingleParameter = struct
  type t = {
    location: Ast.Location.t;
    identifier: Ast.Identifier.t;
    annotation: Ast.Expression.t option;
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


let create_assign ~location ~target ~annotation ~value () =
  let value =
    (* TODO(T101298692): Make `value` optional in assign statement and stop auto-filling `...` *)
    let location =
      let open Ast.Location in
      { location with start = location.stop }
    in
    Option.value value ~default:(Expression.Constant Constant.Ellipsis |> Node.create ~location)
  in
  match Node.value target with
  | Expression.Call
      {
        callee =
          {
            Node.value =
              Expression.Name
                (Name.Attribute { Name.Attribute.base; attribute = "__getitem__"; special = true });
            location = callee_location;
          };
        arguments;
      } ->
      let setitem =
        Expression.Call
          {
            callee =
              Expression.Name
                (Name.Attribute { Name.Attribute.base; attribute = "__setitem__"; special = true })
              |> Node.create ~location:callee_location;
            arguments = List.append arguments [{ Call.Argument.name = None; value }];
          }
        |> Node.create ~location
      in
      Statement.Expression setitem
  | _ ->
      (* TODO(T101303314): This does not take into account things like `a[0], b = ...`, where we'll
         need to turn `a[0]` into `__setitem__` call. *)
      Statement.Assign { target; annotation; value }


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
        | Errpyast.Lambda lambda ->
            Expression.Lambda
              {
                Lambda.parameters = translate_arguments lambda.args;
                body = translate_expression lambda.body;
              }
        | _ ->
            let fail_message =
              Format.asprintf
                "not yet implemented expression: %s"
                (Errpyast.show_expr_desc expression_desc)
            in
            failwith fail_message
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


and translate_argument (argument : Errpyast.arg) =
  let location =
    let end_lineno = Option.value argument.end_lineno ~default:argument.lineno in
    let end_col_offset = Option.value argument.end_col_offset ~default:argument.col_offset in
    {
      start = { line = argument.lineno; column = argument.col_offset };
      stop = { line = end_lineno; column = end_col_offset };
    }
  in
  let annotation = Option.map ~f:translate_expression argument.annotation in
  let annotation =
    match annotation with
    | Some _ -> annotation
    | None -> (
        match argument.type_comment with
        | None -> None
        | Some comment ->
            let comment_annotation =
              Ast.Expression.(
                Expression.Constant
                  (Constant.String { StringLiteral.kind = String; value = comment }))
            in
            Some (Ast.Node.create ~location comment_annotation))
  in
  { SingleParameter.location; identifier = argument.arg; annotation }


and translate_arguments (arguments : Errpyast.arguments) =
  let posonlyargs = List.map ~f:translate_argument arguments.posonlyargs in
  let args = List.map ~f:translate_argument arguments.args in
  let vararg = Option.map ~f:translate_argument arguments.vararg in
  let kwonlyargs = List.map ~f:translate_argument arguments.kwonlyargs in
  let kw_defaults =
    List.map ~f:(fun default -> Option.map ~f:translate_expression default) arguments.kw_defaults
  in
  let kwarg = Option.map ~f:translate_argument arguments.kwarg in
  let defaults = List.map ~f:translate_expression arguments.defaults in

  let to_parameter ({ SingleParameter.location; identifier; annotation }, default_value) =
    { Parameter.name = identifier; value = default_value; annotation } |> Node.create ~location
  in
  let to_parameters parameter_list default_list =
    List.zip_exn parameter_list default_list |> List.map ~f:to_parameter
  in
  let positional_only_defaults, regular_defaults =
    let positional_only_count = List.length posonlyargs in
    let regular_count = List.length args in
    let expanded_defaults =
      let total_counts = positional_only_count + regular_count in
      let fill_counts = total_counts - List.length defaults in
      List.map defaults ~f:Option.some |> List.append (List.init fill_counts ~f:(fun _ -> None))
    in
    List.split_n expanded_defaults positional_only_count
  in
  let positional_only_parameters = to_parameters posonlyargs positional_only_defaults in
  let regular_parameters = to_parameters args regular_defaults in
  let keyword_only_parameters = to_parameters kwonlyargs kw_defaults in
  let vararg_parameter =
    let handle_vararg { SingleParameter.location; identifier; annotation } =
      let name = Caml.Format.sprintf "*%s" identifier in
      { Parameter.name; value = None; annotation } |> Node.create ~location
    in
    Option.map vararg ~f:handle_vararg
  in
  let kwarg_parameter =
    let handle_kwarg { SingleParameter.location; identifier; annotation } =
      let name = Caml.Format.sprintf "**%s" identifier in
      { Parameter.name; value = None; annotation } |> Node.create ~location
    in
    Option.map kwarg ~f:handle_kwarg
  in
  let delimiter_parameter ~should_insert name =
    (* TODO(T101307161): This is just an ugly temporary hack that helps preserve backward
       compatibility. *)
    if should_insert then
      [Node.create_with_default_location { Parameter.name; value = None; annotation = None }]
    else
      []
  in
  List.concat
    [
      positional_only_parameters;
      delimiter_parameter ~should_insert:(not (List.is_empty positional_only_parameters)) "/";
      regular_parameters;
      Option.to_list vararg_parameter;
      delimiter_parameter
        ~should_insert:
          ((not (List.is_empty keyword_only_parameters)) && Option.is_none vararg_parameter)
        "*";
      keyword_only_parameters;
      Option.to_list kwarg_parameter;
    ]


and translate_statements
    (statements : Errpyast.stmt list)
    ~context:({ StatementContext.parent; _ } as context)
  =
  let translate_statement (statement : Errpyast.stmt) =
    let translate_withitem (with_item : Errpyast.withitem) =
      ( translate_expression with_item.context_expr,
        Option.map with_item.optional_vars ~f:translate_expression )
    in
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
    let create_function_definition ~async ~name ~args ~body ~decorator_list ~returns ~_type_comment =
      let signature =
        {
          Define.Signature.name = Ast.Reference.create name;
          parameters = args;
          decorators = decorator_list;
          return_annotation = returns;
          async;
          generator = is_generator body;
          parent = Option.map parent ~f:Ast.Reference.create;
          nesting_define = None;
        }
      in
      [Statement.Define { Define.signature; captures = []; unbound_names = []; body }]
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
                handles_exception_group = false;
              };
          ]
      | Errpyast.With with_statement ->
          [
            Statement.With
              {
                With.items = List.map ~f:translate_withitem with_statement.items;
                body = translate_statements with_statement.body ~context;
                async = false;
              };
          ]
      | Errpyast.AsyncWith with_statement ->
          [
            Statement.With
              {
                With.items = List.map ~f:translate_withitem with_statement.items;
                body = translate_statements with_statement.body ~context;
                async = true;
              };
          ]
      | Errpyast.AnnAssign ann_assign ->
          [
            create_assign
              ~location
              ~target:(translate_expression ann_assign.target)
              ~annotation:(Some (translate_expression ann_assign.annotation))
              ~value:(Option.map ann_assign.value ~f:translate_expression)
              ();
          ]
      | Errpyast.AugAssign aug_assign ->
          let target = translate_expression aug_assign.target in
          let callee =
            let dunder_name =
              Caml.Format.sprintf "__i%s__" (translate_binary_operator aug_assign.op)
            in
            Expression.Name
              (Name.Attribute { base = target; attribute = dunder_name; special = true })
            |> Node.create ~location:(Node.location target)
          in
          let value =
            Expression.Call
              {
                callee;
                arguments =
                  [{ Call.Argument.name = None; value = translate_expression aug_assign.value }];
              }
            |> Node.create ~location
          in
          [create_assign ~location ~target ~annotation:None ~value:(Some value) ()]
      | Errpyast.Assign assign ->
          let value = translate_expression assign.value in

          (* Eagerly turn chained assignments `a = b = c` into `a = c; b = c`. *)
          let create_assign_for_target (target : Errpyast.expr) =
            let target = translate_expression target in
            let location =
              let { start; _ } = Node.location target in
              { location with start }
            in
            let annotation =
              match assign.type_comment, target with
              | Some comment, { Node.value = Expression.Name _; _ } ->
                  let annotation =
                    Expression.Constant (Constant.String (StringLiteral.create comment))
                  in
                  let location =
                    (* Type comments do not have locations attached in CPython. This is just a rough
                       guess.*)
                    let open Ast.Location in
                    let { stop = { line = start_line; column = start_column }; _ } =
                      Node.location value
                    in
                    let { stop; _ } = location in
                    { start = { line = start_line; column = start_column + 1 }; stop }
                  in
                  Some (Node.create ~location annotation)
              | _ ->
                  (* TODO (T104971233): Support type comments when the LHS of assign is a
                     list/tuple. *)
                  None
            in
            create_assign ~location ~target ~annotation ~value:(Some value) ()
          in
          List.map assign.targets ~f:create_assign_for_target
      | Errpyast.FunctionDef function_def ->
          create_function_definition
            ~async:false
            ~name:function_def.name
            ~args:(translate_arguments function_def.args)
            ~body:(translate_statements function_def.body ~context:{ parent = None })
            ~decorator_list:(List.map ~f:translate_expression function_def.decorator_list)
            ~returns:(Option.map ~f:translate_expression function_def.returns)
            ~_type_comment:function_def.type_comment
      | Errpyast.AsyncFunctionDef async_function_def ->
          create_function_definition
            ~async:true
            ~name:async_function_def.name
            ~args:(translate_arguments async_function_def.args)
            ~body:(translate_statements async_function_def.body ~context:{ parent = None })
            ~decorator_list:(List.map ~f:translate_expression async_function_def.decorator_list)
            ~returns:(Option.map ~f:translate_expression async_function_def.returns)
            ~_type_comment:async_function_def.type_comment
      | Errpyast.Delete targets -> [Statement.Delete (List.map targets ~f:translate_expression)]
      | Errpyast.Global names -> [Statement.Global names]
      | Errpyast.Nonlocal names -> [Statement.Nonlocal names]
      | Errpyast.Pass -> [Statement.Pass]
      | Errpyast.Break -> [Statement.Break]
      | Errpyast.Continue -> [Statement.Continue]
      | Errpyast.Expr expression -> [Statement.Expression (translate_expression expression)]
      | Errpyast.ClassDef class_def ->
          let base_arguments =
            List.append
              (List.map class_def.bases ~f:convert_positional_argument)
              (List.map class_def.keywords ~f:convert_keyword_argument)
          in
          let name = class_def.name in
          [
            Statement.Class
              {
                Class.name = Ast.Reference.create name;
                base_arguments;
                body = translate_statements class_def.body ~context:{ parent = Some name };
                decorators = List.map ~f:translate_expression class_def.decorator_list;
                top_level_unbound_names = [];
              };
          ]
      | Errpyast.Match _match ->
          let fail_message =
            Format.asprintf
              "not yet implemented statement: %s"
              (Errpyast.show_stmt_desc statement_desc)
          in
          failwith fail_message
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


module SyntaxError = struct
  type t = {
    line: int;
    column: int;
    end_line: int;
    end_column: int;
    message: string;
  }
end

module ParserError = struct
  type t =
    | Recoverable of {
        recovered_ast: Ast.Statement.t list;
        errors: SyntaxError.t list;
      }
    | Unrecoverable of string
end

let parse_module text =
  let open Result in
  let make_syntax_error (recoverable_error : Errpyast.recoverableerrorwithlocation) =
    {
      SyntaxError.message = recoverable_error.error;
      line = recoverable_error.lineno;
      column = recoverable_error.col_offset;
      end_line = recoverable_error.end_lineno;
      end_column = recoverable_error.end_col_offset;
    }
  in
  match Errpyparser.parse_module text with
  | Ok (module_, recoverable_errors) -> (
      let (transformed_ast : Ast.Statement.t list) = translate_module module_ in
      match recoverable_errors with
      | [] -> Ok transformed_ast
      | recoverable_errors ->
          Result.Error
            (ParserError.Recoverable
               {
                 recovered_ast = transformed_ast;
                 errors = List.map ~f:make_syntax_error recoverable_errors;
               }))
  | Error error_string -> Result.Error (ParserError.Unrecoverable error_string)
  | exception e -> Result.Error (ParserError.Unrecoverable (Printexc.to_string e))
