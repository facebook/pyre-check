(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Access = Expression.Access
module Argument = Expression.Argument


module Record : sig
  module Define : sig
    type 'statement record = {
      name: Access.t;
      parameters: (Expression.t Parameter.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
      return_annotation: Expression.t option;
      async: bool;
      generated: bool;
      parent: Access.t option;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Class : sig
    type 'statement record = {
      name: Access.t;
      bases: Argument.t list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module For : sig
    type 'statement record = {
      target: Expression.t;
      iterator: Expression.t;
      body: 'statement list;
      orelse: 'statement list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module With : sig
    type 'statement record = {
      items: (Expression.t * Expression.t option) list;
      body: 'statement list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Try : sig
    type 'statement handler = {
      kind: Expression.t option;
      name: Identifier.t option;
      handler_body: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash]

    type 'statement record = {
      body: 'statement list;
      handlers: 'statement handler list;
      orelse: 'statement list;
      finally: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module While : sig
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module If : sig
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Assert : sig
  type t = {
    test: Expression.t;
    message: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Import : sig
  type import = {
    name: Access.t;
    alias: Access.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t = {
    from: Access.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module Assign : sig
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t;
    parent: Access.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]

  val is_static_attribute_initialization: t -> bool
end

module Return : sig
  type t = {
    is_implicit: bool;
    expression: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

type statement =
  | Assign of Assign.t
  | Assert of Assert.t
  | Break
  | Class of t Record.Class.record
  | Continue
  | Define of t Record.Define.record
  | Delete of Expression.t
  | Expression of Expression.t
  | For of t Record.For.record
  | Global of Identifier.t list
  | If of t If.t
  | Import of Import.t
  | Nonlocal of Identifier.t list
  | Pass
  | Raise of Expression.t option
  | Return of Return.t
  | Try of t Record.Try.record
  | With of t Record.With.record
  | While of t While.t
  | Yield of Expression.t
  | YieldFrom of Expression.t

and t = statement Node.t
[@@deriving compare, eq, sexp, show, hash]

(* Oh ffs ohcaml... *)
type statement_t = t
[@@deriving compare, eq, sexp, show, hash]

module Attribute : sig
  type attribute = {
    target: Expression.t;
    annotation: Expression.t option;
    defines: ((statement_t Record.Define.record) list) option;
    value: Expression.t option;
    async: bool;
    setter: bool;
    property: bool;
    primitive: bool;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t = attribute Node.t
  [@@deriving compare, eq, sexp, show, hash]
end

module Define : sig
  include module type of struct include Record.Define end

  type t = statement_t Record.Define.record
  [@@deriving compare, eq, sexp, show, hash]

  val create_toplevel: qualifier: Access.t -> statements: statement_t list -> t
  val create_class_toplevel: qualifier: Access.t -> statements: statement_t list -> t
  val create_generated_constructor: statement_t Record.Class.record -> t

  val unqualified_name: t -> Access.t
  val self_identifier: t -> Identifier.t

  val is_method: t -> bool
  val is_coroutine: t -> bool
  val is_abstract_method: t -> bool
  val is_overloaded_method: t -> bool
  val is_static_method: t -> bool
  val is_class_method: t -> bool
  val is_dunder_method: t -> bool
  val is_constructor: ?in_test: bool -> t -> bool
  val is_generated_constructor: t -> bool
  val is_property_setter: t -> bool
  val is_untyped: t -> bool
  val is_stub: t -> bool

  val is_toplevel: t -> bool
  val is_class_toplevel: t -> bool

  val dump: t -> bool
  val dump_cfg: t -> bool

  val implicit_attributes
    :  t
    -> definition: statement_t Record.Class.record
    -> Attribute.t Access.SerializableMap.t
  val property_attribute: location: Location.t -> t -> Attribute.t option

  val has_decorator: t -> string -> bool
end

module Class : sig
  include module type of struct include Record.Class end

  type t = statement_t Record.Class.record
  [@@deriving compare, eq, sexp, show, hash]

  val constructors: ?in_test: bool -> t -> Define.t list

  val implicit_attributes: ?in_test: bool -> t -> Attribute.t Access.SerializableMap.t
  val explicitly_assigned_attributes: t -> Attribute.t Access.SerializableMap.t
  val attributes
    :  ?include_generated_attributes: bool
    -> ?in_test: bool
    -> t
    -> Attribute.t Access.SerializableMap.t

  val update: t -> definition: t -> t

  val has_decorator: t -> string -> bool
end

module For : sig
  include module type of struct include Record.For end

  type t = statement_t Record.For.record
  [@@deriving compare, eq, sexp, show, hash]

  val preamble: t -> statement_t
end

module With : sig
  include module type of struct include Record.With end

  type t = statement_t Record.With.record
  [@@deriving compare, eq, sexp, show, hash]

  val preamble: t -> statement_t list
end

module Try : sig
  include module type of struct include Record.Try end

  type t = statement_t Record.Try.record
  [@@deriving compare, eq, sexp, show, hash]

  val preamble: statement_t handler -> statement_t list
end

val assume: Expression.t -> t

val terminates: t list -> bool

val extract_docstring : t list -> string option
