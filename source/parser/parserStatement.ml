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

module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t;
  }
end

module Import = struct
  type t = {
    from: Reference.t option;
    imports: Ast.Statement.Import.import Node.t list;
  }
end

module Raise = struct
  type t = {
    expression: Expression.t option;
    from: Expression.t option;
  }
end

module Return = struct
  type t = {
    is_implicit: bool;
    expression: Expression.t option;
  }
end

module Decorator = struct
  type t = {
    name: Reference.t Node.t;
    arguments: ParserExpression.Call.Argument.t list option;
  }
end

module rec Assert : sig
  type t = {
    test: Expression.t;
    message: Expression.t option;
  }
end = struct
  type t = {
    test: Expression.t;
    message: Expression.t option;
  }
end

and Class : sig
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
      (* The class owning the method. *)
      parent: Reference.t option;
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
      (* The class owning the method. *)
      parent: Reference.t option;
    }
  end

  type t = {
    signature: Signature.t;
    body: Statement.t list;
  }
end

and For : sig
  type t = {
    target: Expression.t;
    iterator: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
    async: bool;
  }
end = struct
  type t = {
    target: Expression.t;
    iterator: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
    async: bool;
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

and Try : sig
  module Handler : sig
    type t = {
      kind: Expression.t option;
      name: Identifier.t option;
      body: Statement.t list;
    }
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
  }
end = struct
  module Handler = struct
    type t = {
      kind: Expression.t option;
      name: Identifier.t option;
      body: Statement.t list;
    }
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
  }
end

and While : sig
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

and With : sig
  type t = {
    items: (Expression.t * Expression.t option) list;
    body: Statement.t list;
    async: bool;
  }
end = struct
  type t = {
    items: (Expression.t * Expression.t option) list;
    body: Statement.t list;
    async: bool;
  }
end

and Statement : sig
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t list
    | Expression of Expression.t
    | For of For.t
    | Global of Identifier.t list
    | If of If.t
    | Import of Import.t
    | Nonlocal of Identifier.t list
    | Pass
    | Raise of Raise.t
    | Return of Return.t
    | Try of Try.t
    | With of With.t
    | While of While.t

  type t = statement Node.t
end = struct
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t list
    | Expression of Expression.t
    | For of For.t
    | Global of Identifier.t list
    | If of If.t
    | Import of Import.t
    | Nonlocal of Identifier.t list
    | Pass
    | Raise of Raise.t
    | Return of Return.t
    | Try of Try.t
    | With of With.t
    | While of While.t

  type t = statement Node.t
end
