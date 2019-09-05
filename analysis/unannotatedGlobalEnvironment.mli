(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

type t

module ReadOnly : sig
  type t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val get_class_definition : t -> ?dependency:Reference.t -> string -> Class.t Node.t option

  val class_exists : t -> ?dependency:Reference.t -> string -> bool

  (* This should not be used for any analysis, since it is not dependency tracked. It is intended
     for use for in debugging tasks like like validating and printing the class hierarchy *)
  val all_classes : t -> Type.Primitive.t list
end

val create : AstEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  (* This type is sealed to reify that Environment updates must follow and be based off of
     preenvironment updates *)
  type t

  (* In principle we should only need to pass on those of these that are newly introduced, but we
     pass all current classes in the specified modules as a compatibility feature for downstream
     consumers not recording dependcies *)
  val current_classes : t -> Type.Primitive.Set.t

  (* Purely a compatibility feature for downstream consumers that are tracking their own
     dependencies off of these names rather than recording them directly *)
  val current_classes_and_removed_classes : t -> Type.Primitive.Set.t

  val triggered_dependencies : t -> SharedMemoryKeys.ReferenceDependencyKey.KeySet.t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  Reference.Set.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
