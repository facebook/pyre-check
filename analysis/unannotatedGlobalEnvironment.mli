(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open SharedMemoryKeys

type t

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
  | Define of Define.t Node.t list
[@@deriving compare, show]

module ReadOnly : sig
  include Environment.ReadOnly

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val get_class_definition : t -> ?dependency:dependency -> string -> Class.t Node.t option

  val class_exists : t -> ?dependency:dependency -> string -> bool

  val all_classes : t -> Type.Primitive.t list

  val all_indices : t -> IndexTracker.t list

  val all_unannotated_globals : t -> Reference.t list

  val get_unannotated_global
    :  t ->
    ?dependency:dependency ->
    Reference.t ->
    unannotated_global option
end

val create : AstEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  (* This type is sealed to reify that Environment updates must follow and be based off of
     preenvironment updates *)
  type t

  val added_unannotated_globals : t -> Reference.Set.t

  val added_classes : t -> Type.Primitive.Set.t

  (* In principle we should only need to pass on those of these that are newly introduced, but we
     pass all current classes in the specified modules as a compatibility feature for downstream
     consumers not recording dependcies *)
  val current_classes : t -> Type.Primitive.Set.t

  val current_unannotated_globals : t -> Reference.Set.t

  (* Purely a compatibility feature for downstream consumers that are tracking their own
     dependencies off of these names rather than recording them directly *)
  val current_classes_and_removed_classes : t -> Type.Primitive.Set.t

  val current_and_previous_unannotated_globals : t -> Reference.Set.t

  val locally_triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> AstEnvironment.UpdateResult.t

  val all_triggered_dependencies : t -> DependencyKey.KeySet.t list

  val unannotated_global_environment_update_result : t -> t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ast_environment_update_result:AstEnvironment.UpdateResult.t ->
  Reference.Set.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
