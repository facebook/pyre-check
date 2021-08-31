(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Statement

type decorator_reference_and_module = {
  decorator: Reference.t;
  module_reference: Reference.t option;
}
[@@deriving compare, hash, sexp, eq, show]

type define_and_originating_module = {
  decorator_define: Define.t;
  module_reference: Reference.t option;
}
[@@deriving compare, hash, sexp, eq, show]

val uniquify_names
  :  get_reference:('a -> Reference.t) ->
  set_reference:(Reference.t -> 'a -> 'a) ->
  'a list ->
  'a list

val all_decorators : TypeEnvironment.ReadOnly.t -> decorator_reference_and_module list

val all_decorator_bodies
  :  TypeEnvironment.ReadOnly.t ->
  define_and_originating_module Reference.Map.t

val inline_decorators_for_define
  :  decorator_bodies:define_and_originating_module Reference.Map.t ->
  location:Location.t ->
  Define.t ->
  Define.t

val inline_decorators
  :  decorator_bodies:define_and_originating_module Reference.Map.t ->
  Source.t ->
  Source.t

val type_environment_with_decorators_inlined
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  recheck:
    (configuration:Configuration.Analysis.t ->
    scheduler:Scheduler.t ->
    environment:Analysis.TypeEnvironment.t ->
    errors:Analysis.AnalysisError.t list Ast.Reference.Table.t ->
    PyrePath.t list ->
    Ast.Reference.t list * Analysis.AnalysisError.t list) ->
  decorators_to_skip:Reference.Set.t ->
  TypeEnvironment.t ->
  TypeEnvironment.t

val sanitize_defines : strip_decorators:bool -> Source.t -> Source.t

val requalify_name
  :  old_qualifier:Reference.t ->
  new_qualifier:Reference.t ->
  Expression.Name.t ->
  Expression.Name.t

val replace_signature_if_always_passing_on_arguments
  :  callee_name:Identifier.t ->
  new_signature:Define.Signature.t ->
  Define.t ->
  Define.t option

val rename_local_variables : pairs:(Identifier.t * Identifier.t) list -> Define.t -> Define.t

module DecoratorModuleValue : Memory.ValueType with type t = Ast.Reference.t

module DecoratorModule :
  Memory.WithCache.S
    with type t = DecoratorModuleValue.t
     and type key = Analysis.SharedMemoryKeys.ReferenceKey.t
