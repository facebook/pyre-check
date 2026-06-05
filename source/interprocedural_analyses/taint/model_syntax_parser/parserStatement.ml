(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

module Expression = ParserExpression.Expression
module Node = Ast.Node
module Reference = Ast.Reference
module Identifier = Ast.Identifier
module AstStatement = Ast.Statement
module AstExpression = Ast.Expression

module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t option;
    (* If this is in a assignment chain `a = b = c`, the index of that assignment *)
    index_in_chain: int option;
  }
end

module rec Class : sig
  type t = {
    name: Reference.t;
    base_arguments: ParserExpression.Call.Argument.t list;
    body: Statement.t list;
    decorators: Expression.t list;
  }
end = struct
  type t = {
    name: Reference.t;
    base_arguments: ParserExpression.Call.Argument.t list;
    body: Statement.t list;
    decorators: Expression.t list;
  }
end

and Define : sig
  module Signature : sig
    type t = {
      name: Reference.t;
      parameters: ParserExpression.Parameter.t list;
      decorators: Expression.t list;
      return_annotation: Expression.t option;
      async: bool;
    }
  end

  type t = {
    signature: Signature.t;
    body: Statement.t list;
  }
end = struct
  module Signature = struct
    type t = {
      name: Reference.t;
      parameters: ParserExpression.Parameter.t list;
      decorators: Expression.t list;
      return_annotation: Expression.t option;
      async: bool;
    }
  end

  type t = {
    signature: Signature.t;
    body: Statement.t list;
  }
end

and If : sig
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
end = struct
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
end

and Statement : sig
  type statement =
    | Assign of Assign.t
    | Class of Class.t
    | Define of Define.t
    | Expression of Expression.t
    | If of If.t

  type t = statement Node.t
end = struct
  type statement =
    | Assign of Assign.t
    | Class of Class.t
    | Define of Define.t
    | Expression of Expression.t
    | If of If.t

  type t = statement Node.t
end
