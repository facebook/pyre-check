(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement

module Error = AnalysisError
module AccessState: sig
  (* Keep track of objects whose type might be determined later on or that might serve as implicit
     argument to a call. *)
  type target = {
    access: Access.t;
    annotation: Type.t;
  }

  type origin =
    | Instance of Annotated.Attribute.t
    | Module of Access.t
  [@@deriving show]

  type element =
    | Signature of {
        signature: AnnotatedSignature.t;
        callees: Type.Callable.t list;
        arguments: Argument.t list;
      }
    | Attribute of { attribute: Access.t; origin: origin; defined: bool }
    | NotCallable of Type.t
    | Value
  [@@deriving show]

  val redirect: resolution: Resolution.t -> Access.t -> Access.t * Resolution.t

  val resolve_exports: resolution: Resolution.t -> Access.t -> Access.t
end


module State : sig
  (* Keep track of nested functions to analyze and their initial states. *)
  type nested_define

  (* `configuration` provides access to global options.

     `resolution` provides access to the local and global type environment.

     `errors` is a map from locations to errors. We assume there can be only
     one error per location.

     `define` is the function we're currently checking.

     `nested_defines` keeps track of entry points and states for nested function definitions.

     `bottom` indicates whether the state is reachable.

     The order is defined by values of the map, i.e.
     left <= right <=>
        keys(left) \subset \keys(right) \and
        \forall key \in keys(left): left(key) <= right(key)

     The join takes the union of keys and does an element-wise join on the
     values. *)
  and t = {
    configuration: Configuration.Analysis.t;
    resolution: Resolution.t;
    errors: Error.t Int.Map.t;
    define: Define.t Node.t;
    nested_defines: nested_define Location.Reference.Map.t;
    bottom: bool;
    resolution_fixpoint: ResolutionSharedMemory.annotation_map Int.Map.Tree.t
  }
  [@@deriving eq, show]

  val create
    :  ?configuration: Configuration.Analysis.t
    -> ?bottom: bool
    -> resolution: Resolution.t
    -> define: Statement.Define.t Node.t
    -> ?resolution_fixpoint: ResolutionSharedMemory.annotation_map Int.Map.Tree.t
    -> unit
    -> t

  val errors: t -> Error.t list
  val coverage: t -> Coverage.t

  val initial
    :  ?configuration: Configuration.Analysis.t
    -> resolution: Resolution.t
    -> Define.t Node.t
    -> t

  val widening_threshold: int

  (* Visible for testing. *)
  type resolved = {
    state: t;
    resolved: Type.t;
  }
  val forward_access
    :  resolution: Resolution.t
    -> initial: 'a
    -> f:('a
          -> resolution: Resolution.t
          -> resolved: Annotation.t
          -> element: AccessState.element
          -> lead: Access.t
          -> 'a)
    -> Expression.t Access.access sexp_list
    -> 'a
  val last_element: resolution: Resolution.t -> Access.t -> AccessState.element

  val parse_and_check_annotation: state: t -> Expression.t -> t * Type.t
  val forward_expression: state: t -> expression: Expression.t -> resolved
  val forward_statement: state: t -> statement: Statement.t -> t

  include Fixpoint.State with type t := t
end

module Fixpoint : Fixpoint.Fixpoint with type state := State.t

val resolution
  :  (module Environment.Handler)
  -> ?annotations: Annotation.t Access.Map.t
  -> unit
  -> Resolution.t

val resolution_with_key
  :  environment: (module Environment.Handler)
  -> parent: Expression.Access.t option
  -> access: Expression.Access.t
  -> key: int option
  -> Resolution.t

val name: string

val run
  :  configuration: Configuration.Analysis.t
  -> environment: (module Environment.Handler)
  -> source: Source.t
  -> Error.t list
