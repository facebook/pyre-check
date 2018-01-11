(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement


module Assign : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val create: Assign.t -> t

  val fold
    :  resolution: Resolution.t
    -> initial: 'accumulator
    -> f:
         (access: Access.t Node.t
          -> value_annotation: Type.t
          -> 'accumulator
          -> 'accumulator)
    -> t
    -> 'accumulator
end

module Class : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val name_equal: t -> t -> bool

  type parent_class = t
  [@@deriving compare, eq, sexp, show]

  val create: Class.t Node.t -> t

  val name: t -> Access.t
  val bases: t -> (Expression.t Argument.t) list
  val body: t -> Statement.t list

  val annotation: t -> resolution: Resolution.t -> Type.t

  module Attribute : sig
    type t = {
      name: Expression.expression;
      parent: parent_class;
      annotation: Annotation.t;
      value: Expression.t option;
      location: Location.t;
      defined: bool;
    }
    [@@deriving eq, show]

    val create
      :  resolution: Resolution.t
      -> parent: parent_class
      -> ?defined: bool
      -> Statement.Assign.t Node.t
      -> t

    val name: t -> Expression.expression
    val access: t -> Access.t

    val annotation: t -> Annotation.t
    val parent: t -> parent_class
    val value: t -> Expression.t option
    val location: t -> Location.t
    val defined: t -> bool
  end

  module Method : sig
    type t
    [@@deriving compare, eq, sexp, show]

    val create: define: Define.t -> parent: parent_class -> t

    val name: t -> Access.t

    val define: t -> Define.t
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

  val constructors: t -> resolution: Resolution.t -> Define.t list
  val methods: t -> Method.t list

  val is_protocol: t -> bool
  val implements: t -> resolution: Resolution.t -> protocol: t -> bool

  val attribute_fold
    :  ?transitive: bool
    -> t
    -> initial: 'accumulator
    -> f: ('accumulator -> Attribute.t -> 'accumulator)
    -> resolution: Resolution.t
    -> 'accumulator
  val attributes
    :  ?transitive: bool
    -> t
    -> resolution:Resolution.t
    -> Attribute.t list
end

module Attribute = Class.Attribute
module Method = Class.Method

module Define : sig
  type t
  [@@deriving compare, eq, sexp, show]

  val create: Define.t -> t

  val define: t -> Define.t

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

  val create: kind: kind -> Call.t -> t

  val call: t -> Call.t
  val name: t -> Expression.t

  val arguments: t -> (Expression.t Argument.t) list
  val with_arguments: t -> (Expression.t Argument.t) list -> t

  (* Some calls are redirected to method calls, e.g. `repr(x)` will call
     `x.__repr__()`. *)
  type redirect = {
    access: Access.t;
    call: Access.t;
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
      access: Access.t;
      annotation: Annotation.t;
      call: Call.t;
      callee: Signature.t option;
      backup: (Call.t * Signature.t) option;
    }

    type t =
      | Array
      | Call of call
      | Expression
      | Attribute of Attribute.t
      | Global
      | Identifier
      | Method of method_call
  end

  type t
  [@@deriving compare, eq, sexp, show]

  val create: Access.t -> t

  val fold
    :  resolution: Resolution.t
    -> initial: 'accumulator
    -> f:
         ('accumulator
          -> annotations: Annotation.t Access.Map.t
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
