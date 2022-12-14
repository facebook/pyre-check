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
