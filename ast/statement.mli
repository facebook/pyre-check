(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Record : sig
  module Define : sig
    type 'statement t = {
      name: Expression.Access.t;
      parameters: (Expression.t Parameter.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
      return_annotation: Expression.t option;
      async: bool;
      generated: bool;
      parent: Expression.Access.t option;
    }
    [@@deriving compare, eq, sexp, show]
  end
end

module Class : sig
  type 'statement t = {
    name: Expression.Access.t;
    bases: (Expression.t Argument.t) list;
    body: 'statement list;
    decorators: Expression.t list;
    docstring: string option;
  }
  [@@deriving compare, eq, sexp, show]
end

module For : sig
  type 'statement t = {
    target: Expression.t;
    iterator: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show]
end

module While : sig
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]
end

module If : sig
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]
end

module With : sig
  type 'statement t = {
    items: (Expression.t * Expression.t option) list;
    body: 'statement list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show]
end

module Try : sig
  type 'statement handler = {
    kind: Expression.t option;
    name: Identifier.t option;
    handler_body: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]

  type 'statement t = {
    body: 'statement list;
    handlers: 'statement handler list;
    orelse: 'statement list;
    finally: 'statement list;
  }
  [@@deriving compare, eq, sexp, show]
end

module Assert : sig
  type t = {
    test: Expression.t;
    message: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show]
end

module Import : sig
  type import = {
    name: Expression.Access.t;
    alias: Expression.Access.t option;
  }
  [@@deriving compare, eq, sexp, show]

  type t = {
    from: Expression.Access.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show]
end

module Assign : sig
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t option;
    compound: Expression.BinaryOperator.operator option;
    parent: Expression.Access.t option;
  }
  [@@deriving compare, eq, sexp, show]

  val is_static_field_initialization: t -> bool
end

module Stub : sig
  type 'statement t =
    | Assign of Assign.t
    | Class of 'statement Class.t
    | Define of 'statement Record.Define.t
  [@@deriving compare, eq, sexp, show]
end

type statement =
  | Assign of Assign.t
  | Assert of Assert.t
  | Break
  | Class of t Class.t
  | Continue
  | Define of t Record.Define.t
  | Delete of Expression.t
  | Expression of Expression.t
  | For of t For.t
  | Global of Identifier.t list
  | If of t If.t
  | Import of Import.t
  | Nonlocal of Identifier.t list
  | Pass
  | Raise of Expression.t option
  | Return of Expression.t option
  | Stub of t Stub.t
  | Try of t Try.t
  | With of t With.t
  | While of t While.t
  | Yield of Expression.t
  | YieldFrom of Expression.t

and t = statement Node.t
[@@deriving compare, eq, sexp, show]

(* Oh ffs ohcaml... *)
type statement_node = t
[@@deriving compare, eq, sexp, show]

module Define : sig
  type t = statement_node Record.Define.t
  [@@deriving compare, eq, sexp, show]

  val is_method: t -> bool
  val is_abstract_method: t -> bool
  val is_overloaded_method: t -> bool
  val is_static_method: t -> bool
  val is_class_method: t -> bool
  val is_constructor: t -> bool
  val is_generated_constructor: t -> bool
  val is_untyped: t -> bool

  val create_generated_constructor: statement_node Class.t -> t
  val dump: t -> bool
  val dump_cfg: t -> bool
end

(* Alias for when we open both Statment and Expression. *)
module RecordDefine = Record.Define

val assume: Expression.t -> t

val terminates: t list -> bool

val pp : Format.formatter -> t -> unit

val show : t -> string

val extract_docstring : t list -> string option
