(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Assign : sig
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t;
    parent: Reference.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]

  val is_static_attribute_initialization : t -> bool

  val location_insensitive_compare : t -> t -> int
end

module Import : sig
  type import = {
    name: Reference.t Node.t;
    alias: Identifier.t Node.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t = {
    from: Reference.t Node.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show, hash]

  val location_insensitive_compare : t -> t -> int
end

module Raise : sig
  type t = {
    expression: Expression.t option;
    from: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

module Return : sig
  type t = {
    is_implicit: bool;
    expression: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]

  val location_insensitive_compare : t -> t -> int
end

module Decorator : sig
  type t = {
    name: Reference.t Node.t;
    arguments: Expression.Call.Argument.t list option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val to_expression : t -> Expression.t
end

module rec Assert : sig
  module Origin : sig
    type t =
      | Assertion
      | If of { true_branch: bool }
      | While of { true_branch: bool }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    test: Expression.t;
    message: Expression.t option;
    origin: Origin.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Class : sig
  type t = {
    name: Reference.t Node.t;
    base_arguments: Expression.Call.Argument.t list;
    body: Statement.t list;
    decorators: Decorator.t list;
    top_level_unbound_names: Define.NameAccess.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val toplevel_define : t -> Define.t

  val constructors : ?in_test:bool -> t -> Define.t list

  val defines : t -> Define.t list

  val find_define : t -> method_name:Identifier.t -> Define.t Node.t option

  val is_frozen : t -> bool

  val base_classes : t -> Expression.t list

  val metaclass : t -> Expression.t option

  val init_subclass_arguments : t -> Expression.Call.Argument.t list

  type class_t = t [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

and Define : sig
  module Signature : sig
    type t = {
      name: Reference.t Node.t;
      parameters: Expression.Parameter.t list;
      decorators: Decorator.t list;
      return_annotation: Expression.t option;
      async: bool;
      generator: bool;
      (* The class owning the method. *)
      parent: Reference.t option;
      (* If the define is nested, this is the name of the nesting define. *)
      nesting_define: Reference.t option;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int

    val create_toplevel : qualifier:Reference.t option -> t

    val create_class_toplevel : parent:Reference.t -> t

    val unqualified_name : t -> Identifier.t

    val self_identifier : t -> Identifier.t

    val is_method : t -> bool

    val is_coroutine : t -> bool

    val is_abstract_method : t -> bool

    val is_overloaded_function : t -> bool

    val is_static_method : t -> bool

    val is_final_method : t -> bool

    val is_class_method : t -> bool

    val is_class_property : t -> bool

    val is_dunder_method : t -> bool

    val is_constructor : ?in_test:bool -> t -> bool

    val is_property_setter : t -> bool

    val is_untyped : t -> bool

    val is_toplevel : t -> bool

    val is_class_toplevel : t -> bool

    val has_decorator : ?match_prefix:bool -> t -> string -> bool

    val has_return_annotation : t -> bool [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module Capture : sig
    module Kind : sig
      type t =
        | Annotation of Expression.t option
        | Self of Reference.t
        | ClassSelf of Reference.t
        | DefineSignature of Define.Signature.t
      [@@deriving compare, eq, sexp, show, hash, to_yojson]
    end

    type t = {
      name: Identifier.t;
      kind: Kind.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module NameAccess : sig
    type t = {
      name: Identifier.t;
      location: Location.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  type t = {
    signature: Signature.t;
    captures: Capture.t list;
    unbound_names: NameAccess.t list;
    body: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val create_toplevel
    :  unbound_names:NameAccess.t list ->
    qualifier:Reference.t option ->
    statements:Statement.t list ->
    t

  val name : t -> Reference.t Node.t

  val unqualified_name : t -> Identifier.t

  val self_identifier : t -> Identifier.t

  val is_method : t -> bool

  val is_coroutine : t -> bool

  val is_abstract_method : t -> bool

  val is_overloaded_function : t -> bool

  val is_static_method : t -> bool

  val is_final_method : t -> bool

  val is_class_method : t -> bool

  val is_class_property : t -> bool

  val is_dunder_method : t -> bool

  val is_constructor : ?in_test:bool -> t -> bool

  val is_test_setup : t -> bool

  val is_property_setter : t -> bool

  val is_untyped : t -> bool

  val is_stub : t -> bool

  val is_toplevel : t -> bool

  val is_class_toplevel : t -> bool

  val is_async : t -> bool

  val dump : t -> bool

  val dump_cfg : t -> bool

  val dump_locations : t -> bool

  val dump_call_graph : t -> bool

  val show_json : t -> string

  val has_decorator : ?match_prefix:bool -> t -> string -> bool

  val has_return_annotation : t -> bool
end

and For : sig
  type t = {
    target: Expression.t;
    iterator: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val preamble : t -> Statement.t

  val location_insensitive_compare : t -> t -> int
end

and If : sig
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Try : sig
  module Handler : sig
    type t = {
      kind: Expression.t option;
      name: Identifier.t option;
      body: Statement.t list;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val preamble : Handler.t -> Statement.t list

  val location_insensitive_compare : t -> t -> int
end

and While : sig
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and With : sig
  type t = {
    items: (Expression.t * Expression.t option) list;
    body: Statement.t list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val preamble : t -> Statement.t list

  val location_insensitive_compare : t -> t -> int
end

and Statement : sig
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t
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
  [@@deriving compare, eq, sexp, hash, to_yojson]

  type t = statement Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val assume : ?origin:Assert.Origin.t -> Expression.t -> t

  val generator_assignment : Expression.Comprehension.Generator.t -> Assign.t
end

type statement = Statement.statement [@@deriving compare, eq, sexp, show, hash, to_yojson]

type t = Statement.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

val location_insensitive_compare : t -> t -> int

val is_generator : t list -> bool
