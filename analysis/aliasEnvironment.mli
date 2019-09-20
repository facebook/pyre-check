(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast

type t

type dependency =
  | TypeCheckSource of Reference.t
  | ClassConnect of Type.Primitive.t
  | RegisterClassMetadata of Type.Primitive.t
  | UndecoratedFunction of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey : Memory.DependencyKey.S with type t = dependency

module ReadOnly : sig
  type t

  val get_alias : t -> ?dependency:dependency -> Type.Primitive.t -> Type.alias option

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val parse_annotation_without_validating_type_parameters
    :  t ->
    ?modify_aliases:(Type.alias -> Type.alias) ->
    ?dependency:dependency ->
    ?allow_untracked:bool ->
    ?allow_primitives_from_empty_stubs:bool ->
    Expression.t ->
    Type.t

  val parse_as_concatenation
    :  t ->
    ?dependency:dependency ->
    Expression.t ->
    (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t
    option

  val parse_as_parameter_specification_instance_annotation
    :  t ->
    ?dependency:dependency ->
    variable_parameter_annotation:Expression.t ->
    keywords_parameter_annotation:Expression.t ->
    Type.Variable.Variadic.Parameters.t option
end

val create : UnannotatedGlobalEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> UnannotatedGlobalEnvironment.UpdateResult.t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  UnannotatedGlobalEnvironment.UpdateResult.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
