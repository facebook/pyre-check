(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement


type global = Annotation.t Node.t
[@@deriving eq, show]

type class_metadata = {
  successors: Type.primitive list;
  is_test: bool;
  is_final: bool;
}
[@@deriving eq]

type t
[@@deriving show]

type generic_type_problems =
  | IncorrectNumberOfParameters of {
      actual: int;
      expected: int;
    }
  | ViolateConstraints of {
      actual: Type.t;
      expected: Type.Variable.t;
    }
[@@deriving compare, eq, sexp, show, hash]


type type_parameters_mismatch = {
  name: string;
  kind: generic_type_problems;
}
[@@deriving compare, eq, sexp, show, hash]

val create
  :  annotations: Annotation.t Reference.Map.t
  -> order: (module TypeOrder.Handler)
  -> resolve: (resolution: t -> Expression.t -> Type.t)
  -> aliases: (Type.primitive -> Type.t option)
  -> global: (Reference.t -> global option)
  -> module_definition: (Reference.t -> Module.t option)
  -> class_definition: (?convert: bool -> Type.primitive -> (Class.t Node.t) option)
  -> class_metadata: (Type.primitive -> class_metadata option)
  -> constructor: (resolution: t -> Type.primitive -> Type.t option)
  -> generics: (resolution: t -> Class.t Node.t -> Type.t list)
  -> undecorated_signature: (Reference.t -> Type.t Type.Callable.overload option)
  -> attributes: (resolution: t -> Type.t -> AnnotatedAttribute.t list option)
  -> is_protocol: (Type.t -> bool)
  -> ?parent: Reference.t
  -> unit
  -> t

val set_local: t -> reference: Reference.t -> annotation: Annotation.t -> t
val get_local: ?global_fallback: bool -> reference: Reference.t -> t -> Annotation.t option
val unset_local: t -> reference: Reference.t -> t
val is_global: t -> reference: Reference.t -> bool

val add_type_variable: t -> variable: Type.t -> t
val type_variable_exists: t -> variable: Type.t -> bool

val annotations: t -> Annotation.t Reference.Map.t
val with_annotations: t -> annotations: Annotation.t Reference.Map.t -> t

val parent: t -> Reference.t option
val with_parent: t -> parent: Reference.t option -> t

val order: t -> (module TypeOrder.Handler)

val resolve: t -> Expression.t -> Type.t
val resolve_literal: t -> Expression.t -> Type.t
val resolve_mutable_literals
  :  t
  -> expression:Ast.Expression.t option
  -> resolved:Type.t
  -> expected:Type.t
  -> Type.t

val global: t -> Reference.t -> global option

val module_definition: t -> Reference.t -> Module.t option
val class_definition: ?convert: bool -> t -> Type.t -> (Class.t Node.t) option
val class_metadata: t -> Type.t -> class_metadata option
val is_protocol: t -> Type.t -> bool

module FunctionDefinitionsCache : sig
  val enable: unit -> unit
  val invalidate: unit -> unit
end

val function_definitions: t -> Reference.t -> ((Define.t Node.t) list) option
val is_suppressed_module: t -> Reference.t -> bool
(*  Exposed only for parallelism. Future not guaranteed. *)
val undecorated_signature: t -> Reference.t -> Type.t Type.Callable.overload option

val less_or_equal: t -> left: Type.t -> right: Type.t -> bool
(* Only for use in monkey check. *)
val is_compatible_with: t -> left: Type.t -> right: Type.t -> bool
val join: t -> Type.t -> Type.t -> Type.t
val meet: t -> Type.t -> Type.t -> Type.t
val widen
  :  t
  -> widening_threshold: int
  -> previous: Type.t
  -> next: Type.t
  -> iteration: int
  -> Type.t
val is_consistent_with: t -> Type.t -> Type.t -> expression: Ast.Expression.t option -> bool
val consistent_solution_exists: t -> Type.t -> Type.t -> bool
val is_instantiated: t -> Type.t -> bool
val is_tracked: t -> Type.t -> bool
val contains_untracked: t -> Type.t -> bool

val is_string_to_any_mapping: t -> Type.t -> bool

val check_invalid_type_parameters: t -> Type.t -> type_parameters_mismatch list * Type.t

val parse_reference: ?allow_untracked:bool -> t -> Reference.t -> Type.t
val parse_annotation
  :  ?allow_untracked:bool
  -> ?allow_invalid_type_parameters:bool
  -> t
  -> Expression.t
  -> Type.t
val is_invariance_mismatch: t -> left: Type.t -> right: Type.t -> bool
val solve_less_or_equal
  :  t
  -> constraints: TypeConstraints.t
  -> left: Type.t
  -> right: Type.t
  -> TypeConstraints.t list
val constraints_solution_exists: left: Type.t -> right: Type.t -> t -> bool
val solve_constraints: t -> TypeConstraints.t -> TypeConstraints.Solution.t option
val partial_solve_constraints
  :  t
  -> TypeConstraints.t
  -> variables: Type.Variable.t list
  -> (TypeConstraints.t * TypeConstraints.Solution.t) option

module Cache : sig
  val clear: unit -> unit
end
