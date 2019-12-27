(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open SharedMemoryKeys
open Core

type unannotated_global =
  | SimpleAssign of {
      explicit_annotation: Expression.t option;
      value: Expression.t;
      target_location: Location.t;
    }
  | TupleAssign of {
      value: Expression.t;
      target_location: Location.t;
      index: int;
      total_length: int;
    }
  | Imported of Reference.t
  | Define of Define.Signature.t Node.t list
[@@deriving compare, show, sexp]

module FunctionDefinition : sig
  module Sibling : sig
    module Kind : sig
      type t =
        | Overload
        | PropertySetter
      [@@deriving sexp, compare]
    end

    type t = {
      kind: Kind.t;
      body: Define.t Node.t;
    }
    [@@deriving sexp, compare]
  end

  type t = {
    qualifier: Reference.t;
    body: Define.t Node.t option;
    siblings: Sibling.t list;
  }
  [@@deriving sexp, compare]

  val all_bodies : t -> Define.t Node.t list
end

module ReadOnly : sig
  type t

  val hash_to_key_map : t -> string String.Map.t

  val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

  val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val class_exists : t -> ?dependency:dependency -> string -> bool

  (* APIs that start with prefix `all_` are not dependency tracked and are for testing purpose only.
     DO NOT USE THEM IN PROD. *)

  val all_classes : t -> Type.Primitive.t list

  val all_indices : t -> IndexTracker.t list

  val all_unannotated_globals : t -> Reference.t list

  val all_defines : t -> Reference.t list

  val all_defines_in_module : t -> Reference.t -> Reference.t list

  val get_class_definition : t -> ?dependency:dependency -> string -> ClassSummary.t Node.t option

  val get_unannotated_global
    :  t ->
    ?dependency:dependency ->
    Reference.t ->
    unannotated_global option

  val get_define : t -> ?dependency:dependency -> Reference.t -> FunctionDefinition.t option

  val get_define_body : t -> ?dependency:dependency -> Reference.t -> Define.t Node.t option

  val is_protocol : t -> ?dependency:dependency -> Type.t -> bool
end

module UpdateResult : sig
  (* This type is sealed to reify that Environment updates must follow and be based off of
     preenvironment updates *)
  type t

  type read_only = ReadOnly.t

  val previous_unannotated_globals : t -> Reference.Set.t

  val previous_classes : t -> Type.Primitive.Set.t

  val previous_defines : t -> Reference.Set.t

  val define_additions : t -> Reference.Set.t

  val locally_triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> AstEnvironment.UpdateResult.t

  val all_triggered_dependencies : t -> DependencyKey.KeySet.t list

  val unannotated_global_environment_update_result : t -> t

  val read_only : t -> read_only
end

val update_this_and_all_preceding_environments
  :  AstEnvironment.ReadOnly.t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ast_environment_update_result:AstEnvironment.UpdateResult.t ->
  Reference.Set.t ->
  UpdateResult.t
