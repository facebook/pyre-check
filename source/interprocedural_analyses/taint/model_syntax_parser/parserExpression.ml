(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

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

module rec BinaryOperator : sig
  type t = {
    left: Expression.t;
    operator: AstExpression.BinaryOperator.operator;
    right: Expression.t;
  }
end = struct
  type t = {
    left: Expression.t;
    operator: AstExpression.BinaryOperator.operator;
    right: Expression.t;
  }
end

and BooleanOperator : sig
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

and Name : sig
  module Attribute : sig
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
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

and Slice : sig
  type t = {
    start: Expression.t option;
    stop: Expression.t option;
    step: Expression.t option;
  }
end = struct
  type t = {
    start: Expression.t option;
    stop: Expression.t option;
    step: Expression.t option;
  }
end

and Subscript : sig
  type t = {
    base: Expression.t;
    index: Expression.t;
  }
end = struct
  type t = {
    base: Expression.t;
    index: Expression.t;
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

and Expression : sig
  type expression =
    | BinaryOperator of BinaryOperator.t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of AstExpression.Constant.t
    | FormatString of Substring.t list
    | List of t list
    | Name of Name.t
    | Parenthesis of t
    | Slice of Slice.t
    | Starred of Starred.t
    | Subscript of Subscript.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t

  and t = expression Node.t
end = struct
  type expression =
    | BinaryOperator of BinaryOperator.t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of AstExpression.Constant.t
    | FormatString of Substring.t list
    | List of t list
    | Name of Name.t
    | Parenthesis of t
    | Slice of Slice.t
    | Starred of Starred.t
    | Subscript of Subscript.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t

  and t = expression Node.t
end
