(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
module AstExpression = Ast.Expression
module Node = Ast.Node
module Identifier = Ast.Identifier

module Substring = struct
  module Kind = struct
    type t =
      | Literal
      | RawFormat
  end

  type t = {
    kind: Kind.t;
    location: Ast.Location.t;
    value: string;
  }
end

module rec BooleanOperator : sig
  type t = {
    left: Expression.t;
    operator: AstExpression.BooleanOperator.operator;
    right: Expression.t;
  }
end = struct
  type t = {
    left: Expression.t;
    operator: AstExpression.BooleanOperator.operator;
    right: Expression.t;
  }
end

and Call : sig
  module Argument : sig
    type t = {
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
  }
end = struct
  module Argument = struct
    type t = {
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
  }
end

and ComparisonOperator : sig
  type t = {
    left: Expression.t;
    operator: AstExpression.ComparisonOperator.operator;
    right: Expression.t;
  }
end = struct
  type t = {
    left: Expression.t;
    operator: AstExpression.ComparisonOperator.operator;
    right: Expression.t;
  }
end

and Comprehension : sig
  module Generator : sig
    type t = {
      target: Expression.t;
      iterator: Expression.t;
      conditions: Expression.t list;
      async: bool;
    }
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
end = struct
  module Generator = struct
    type t = {
      target: Expression.t;
      iterator: Expression.t;
      conditions: Expression.t list;
      async: bool;
    }
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
end

and Dictionary : sig
  module Entry : sig
    type t = {
      key: Expression.t;
      value: Expression.t;
    }
  end

  type t = {
    entries: Entry.t list;
    keywords: Expression.t list;
  }
end = struct
  module Entry = struct
    type t = {
      key: Expression.t;
      value: Expression.t;
    }
  end

  type t = {
    entries: Entry.t list;
    keywords: Expression.t list;
  }
end

and Lambda : sig
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
end = struct
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
end

and Name : sig
  module Attribute : sig
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      special: bool;
    }
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
end = struct
  module Attribute = struct
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      special: bool;
    }
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
end

and Parameter : sig
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }

  type t = parameter Node.t
end = struct
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }

  type t = parameter Node.t
end

and Starred : sig
  type t =
    | Once of Expression.t
    | Twice of Expression.t
end = struct
  type t =
    | Once of Expression.t
    | Twice of Expression.t
end

and Ternary : sig
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
end = struct
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
end

and UnaryOperator : sig
  type t = {
    operator: AstExpression.UnaryOperator.operator;
    operand: Expression.t;
  }
end = struct
  type t = {
    operator: AstExpression.UnaryOperator.operator;
    operand: Expression.t;
  }
end

and WalrusOperator : sig
  type t = {
    target: Expression.t;
    value: Expression.t;
  }
end = struct
  type t = {
    target: Expression.t;
    value: Expression.t;
  }
end

and Expression : sig
  type expression =
    | Await of t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of AstExpression.Constant.t
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.t Comprehension.t
    | Generator of t Comprehension.t
    | FormatString of Substring.t list
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Parenthesis of t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Starred of Starred.t
    | Ternary of Ternary.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option
    | YieldFrom of t

  and t = expression Node.t
end = struct
  type expression =
    | Await of t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of AstExpression.Constant.t
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.t Comprehension.t
    | Generator of t Comprehension.t
    | FormatString of Substring.t list
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Parenthesis of t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Starred of Starred.t
    | Ternary of Ternary.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option
    | YieldFrom of t

  and t = expression Node.t
end

let rec convert { Node.location; value } =
  let convert_entry { Dictionary.Entry.key; value } =
    { AstExpression.Dictionary.Entry.key = convert key; value = convert value }
  in
  let convert_generator { Comprehension.Generator.target; iterator; conditions; async } =
    {
      AstExpression.Comprehension.Generator.target = convert target;
      iterator = convert iterator;
      conditions = List.map ~f:convert conditions;
      async;
    }
  in
  match value with
  | Expression.Await expression ->
      AstExpression.Expression.Await (convert expression) |> Node.create ~location
  | BooleanOperator { left; operator; right } ->
      AstExpression.Expression.BooleanOperator
        { left = convert left; operator; right = convert right }
      |> Node.create ~location
  | Call { callee; arguments } ->
      AstExpression.Expression.Call
        { callee = convert callee; arguments = List.map ~f:convert_argument arguments }
      |> Node.create ~location
  | ComparisonOperator { left; operator; right } ->
      AstExpression.Expression.ComparisonOperator
        { left = convert left; operator; right = convert right }
      |> Node.create ~location
  | Constant value -> AstExpression.Expression.Constant value |> Node.create ~location
  | Dictionary { Dictionary.entries; keywords } ->
      AstExpression.Expression.Dictionary
        { entries = List.map ~f:convert_entry entries; keywords = List.map ~f:convert keywords }
      |> Node.create ~location
  | DictionaryComprehension { Comprehension.element; generators } ->
      AstExpression.Expression.DictionaryComprehension
        { element = convert_entry element; generators = List.map ~f:convert_generator generators }
      |> Node.create ~location
  | Generator { Comprehension.element; generators } ->
      AstExpression.Expression.Generator
        { element = convert element; generators = List.map ~f:convert_generator generators }
      |> Node.create ~location
  | FormatString substrings ->
      let convert_substring { Substring.value; location; _ } =
        (* FIXME: The legacy parser no longer has the capability of parsing expressions in fstrings. *)
        AstExpression.Substring.Literal (Node.create ~location value)
      in
      AstExpression.Expression.FormatString (List.map substrings ~f:convert_substring)
      |> Node.create ~location
  | Lambda { Lambda.parameters; body } ->
      AstExpression.Expression.Lambda
        { parameters = List.map ~f:convert_parameter parameters; body = convert body }
      |> Node.create ~location
  | List expression_list ->
      AstExpression.Expression.List (List.map ~f:convert expression_list) |> Node.create ~location
  | ListComprehension { Comprehension.element; generators } ->
      AstExpression.Expression.ListComprehension
        { element = convert element; generators = List.map ~f:convert_generator generators }
      |> Node.create ~location
  | Name (Name.Attribute { base; attribute; special }) ->
      AstExpression.Expression.Name
        (AstExpression.Name.Attribute { base = convert base; attribute; special })
      |> Node.create ~location
  | Name (Name.Identifier name) ->
      AstExpression.Expression.Name (AstExpression.Name.Identifier name) |> Node.create ~location
  | Parenthesis expression -> convert expression
  | Set expression_list ->
      AstExpression.Expression.Set (List.map ~f:convert expression_list) |> Node.create ~location
  | SetComprehension { Comprehension.element; generators } ->
      AstExpression.Expression.SetComprehension
        { element = convert element; generators = List.map ~f:convert_generator generators }
      |> Node.create ~location
  | Starred (Once expression) ->
      AstExpression.Expression.Starred (Once (convert expression)) |> Node.create ~location
  | Starred (Twice expression) ->
      AstExpression.Expression.Starred (Twice (convert expression)) |> Node.create ~location
  | Ternary { target; test; alternative } ->
      AstExpression.Expression.Ternary
        { target = convert target; test = convert test; alternative = convert alternative }
      |> Node.create ~location
  | Tuple expression_list ->
      AstExpression.Expression.Tuple (List.map ~f:convert expression_list) |> Node.create ~location
  | UnaryOperator { UnaryOperator.operator; operand } ->
      AstExpression.Expression.UnaryOperator { operator; operand = convert operand }
      |> Node.create ~location
  | WalrusOperator { target; value } ->
      AstExpression.Expression.WalrusOperator { target = convert target; value = convert value }
      |> Node.create ~location
  | Yield expression ->
      AstExpression.Expression.Yield (expression >>| convert) |> Node.create ~location
  | YieldFrom expression ->
      AstExpression.Expression.YieldFrom (expression |> convert) |> Node.create ~location


and convert_argument { Call.Argument.name; value } =
  { AstExpression.Call.Argument.name; value = convert value }


and convert_parameter { Node.location; value = { Parameter.name; value; annotation } } =
  { AstExpression.Parameter.name; value = value >>| convert; annotation = annotation >>| convert }
  |> Node.create ~location
