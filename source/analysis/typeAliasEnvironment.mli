(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open SharedMemoryKeys

module RawAlias : sig
  type t =
    | TypeAlias of Type.t
    | VariableDeclaration of Type.Variable.Declaration.t
  [@@deriving equal, compare, sexp, show, hash]
end

module AliasReadOnly : sig
  include Environment.ReadOnly

  val get_type_alias
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.Primitive.t ->
    Type.t option

  val get_variable_declaration
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.Primitive.t ->
    Type.Variable.Declaration.t option

  val get_variable
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.Primitive.t ->
    Type.Variable.t option

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val parse_annotation_without_sanitizing_type_arguments
    :  t ->
    ?dependency:DependencyKey.registered ->
    ?modify_aliases:(?replace_unbound_parameters_with_any:bool -> Type.t -> Type.t) ->
    variables:(string -> Type.Variable.t option) ->
    ?allow_untracked:bool ->
    Expression.t ->
    Type.t

  val param_spec_from_vararg_annotations
    :  t ->
    ?dependency:DependencyKey.registered ->
    args_annotation:Expression.t ->
    kwargs_annotation:Expression.t ->
    unit ->
    Type.Variable.ParamSpec.t option
end

include
  Environment.S
    with module ReadOnly = AliasReadOnly
     and module PreviousEnvironment = UnannotatedGlobalEnvironment
