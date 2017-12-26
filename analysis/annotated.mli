(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement


module Class : sig
  type t
  [@@deriving compare, eq, sexp, show]

  type parent_class = t
  [@@deriving compare, eq, sexp, show]

  val create: Statement.t Class.t -> t

  val name: t -> Expression.access
  val bases: t -> (Expression.t Argument.t) list
  val body: t -> Statement.t list

  val annotation: t -> resolution: Resolution.t -> Type.t

  module Field : sig
    type t = {
      name: Expression.expression;
      parent: parent_class;
      annotation: Annotation.t;
      value: Expression.t option;
      location: Location.t;
    }
    [@@deriving eq, show]

    val create: resolution: Resolution.t -> Assign.t Node.t -> t option

    val name: t -> Expression.expression
    val annotation: t -> Annotation.t
    val parent: t -> parent_class
    val value: t -> Expression.t option
    val location: t -> Location.t
  end

  module Method : sig
    type t
    [@@deriving compare, eq, sexp, show]

    val create: define: Statement.define -> parent: parent_class -> t

    val name: t -> Instantiated.Access.t

    val define: t -> Statement.define
    val parent: t -> parent_class

    val parameter_annotations
      :  t
      -> resolution: Resolution.t
      -> Type.t Identifier.Map.t
    val parameter_annotations_positional
      :  t
      -> resolution: Resolution.t
      -> Type.t Int.Map.t
    val return_annotation: t -> resolution: Resolution.t -> Type.t

    val overrides: t -> resolution: Resolution.t -> t option

    val implements: t -> resolution: Resolution.t -> protocol_method: t -> bool
  end

  val generics: t -> resolution: Resolution.t -> Type.t list
  (* Find free variables in the parametric type. E.g. for generic class
      `class A(typing.Generic[_T], typing.Generic[_S]): ...`
      and instantiated type `A[int, Bottom]` we consider `_S` to be free. *)
  val free_variables
    :  t
    -> resolution: Resolution.t
    -> parameters: Type.t list
    -> (Type.t option) list
  val superclasses
    :  t
    -> resolution: Resolution.t
    -> t list

  val constructors: t -> resolution: Resolution.t -> Statement.define list
  val methods: t -> Method.t list

  val is_protocol: t -> bool
  val implements: t -> resolution: Resolution.t -> protocol: t -> bool

  val field_fold
    :  ?transitive: bool
    -> t
    -> initial: 'accumulator
    -> f: ('accumulator -> Field.t -> 'accumulator)
    -> resolution: Resolution.t
    -> 'accumulator
  val fields
    :  ?transitive: bool
    -> t
    -> resolution:Resolution.t
    -> Field.t list
end

module Field = Class.Field
module Method = Class.Method

module Define : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val create: Statement.t Define.t -> t

  val parameter_annotations
    :  t
    -> resolution: Resolution.t
    -> Type.t Identifier.Map.t
  val parameter_annotations_positional
    :  t
    -> resolution: Resolution.t
    -> Type.t Int.Map.t
  val return_annotation: t -> resolution: Resolution.t -> Type.t

  val parent_definition: t -> resolution: Resolution.t -> Class.t option
  val method_definition: t -> resolution: Resolution.t -> Method.t option

  val infer_argument_name
    :  t
    -> index: int
    -> argument: Expression.t Argument.t
    -> Identifier.t option
end

module BinaryOperator : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val create: Expression.t BinaryOperator.t -> t

  val override: t -> Expression.t
end

module Call : sig
  type kind =
    | Function
    | Method
  [@@deriving compare, eq, sexp, show]

  type t
  [@@deriving compare, eq, sexp, show]

  val create: kind: kind -> Expression.t Call.t -> t
  val call: t -> Expression.t Call.t

  val arguments: t -> (Expression.t Argument.t) list
  val with_arguments: t -> (Expression.t Argument.t) list -> t
  val prepend_self_argument: t -> t

  val name: t -> Expression.t

  (* Some calls are redirected to method calls, e.g. `repr(x)` will call
     `x.__repr__()`. *)
  type redirect = {
    access: access;
    call: access;
  }
  val redirect: t -> redirect option

  (* Some calls have a backup in case the original call raises `NotImplemented`. *)
  val backup: t -> t option

  (* In some cases python magically calls a backup method in case a call fails. *)
  val backup: t -> t option

  val argument_annotations
    :  t
    -> resolution: Resolution.t
    -> Signature.argument list

  val check_parameters
    :  resolution: Resolution.t
    -> check_parameter: (
        argument: Expression.t Argument.t
        -> position: int
        -> offset: int
        -> location: Location.t
        -> name: Identifier.t
        -> actual: Type.t
        -> expected: Type.t
        -> 'error option)
    -> add_error: ('accumulator -> 'error -> 'accumulator)
    -> init: 'accumulator
    -> t
    -> Signature.t
    -> 'accumulator
end

module ComparisonOperator : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val override: t -> (Expression.t option) list
end

module UnaryOperator : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val override: t -> Expression.t option
end


module Access: sig
  module Element: sig
    type call = {
      location: Location.t;
      call: Call.t;
      callee: Signature.t option
    }

    type method_call = {
      location: Location.t;
      access: access;
      annotation: Annotation.t;
      call: Call.t;
      callee: Signature.t option;
      backup: (Call.t * Signature.t) option;
    }

    type undefined_field = {
      name: Expression.access;
      parent: Class.t option;
    }

    type field =
      | Defined of Field.t
      | Undefined of undefined_field

    type t =
      | Array
      | Call of call
      | Expression
      | Field of field
      | Global
      | Identifier
      | Method of method_call
  end

  type t
  [@@deriving compare, eq, sexp, show]

  val create: Expression.access -> t

  val fold
    :  resolution: Resolution.t
    -> initial: 'accumulator
    -> f:
         ('accumulator
          -> annotations: Annotation.t Instantiated.Access.Map.t
          -> resolved: Annotation.t
          -> element: Element.t
          -> 'accumulator)
    -> t
    -> 'accumulator

  val last_element: resolution: Resolution.t -> t -> Element.t
end

val resolve
  :  resolution: Resolution.t
  -> Expression.t
  -> Type.t
