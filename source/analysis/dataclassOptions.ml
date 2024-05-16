(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module Queries = struct
  type t = {
    controls: EnvironmentControls.t;
    resolve_exports: ?from:Ast.Reference.t -> Ast.Reference.t -> ResolvedReference.t option;
    is_protocol: Type.t -> bool;
    get_unannotated_global: Ast.Reference.t -> Ast.UnannotatedGlobal.t option;
    get_class_summary: string -> ClassSummary.t Ast.Node.t option;
    first_matching_class_decorator:
      names:string list -> ClassSummary.t Ast.Node.t -> Ast.Statement.Decorator.t option;
    exists_matching_class_decorator: names:string list -> ClassSummary.t Ast.Node.t -> bool;
    class_exists: string -> bool;
    parse_annotation_without_validating_type_parameters:
      ?modify_aliases:(?replace_unbound_parameters_with_any:bool -> Type.alias -> Type.alias) ->
      ?allow_untracked:bool ->
      Ast.Expression.t ->
      Type.t;
    parse_as_parameter_specification_instance_annotation:
      variable_parameter_annotation:Ast.Expression.t ->
      keywords_parameter_annotation:Ast.Expression.t ->
      unit ->
      Type.Variable.Variadic.Parameters.t option;
    class_hierarchy: unit -> (module ClassHierarchy.Handler);
    variables:
      ?default:Type.Variable.t list option -> Type.Primitive.t -> Type.Variable.t list option;
    successors: Type.Primitive.t -> string list;
    get_class_metadata: Type.Primitive.t -> ClassSuccessorMetadataEnvironment.class_metadata option;
    is_typed_dictionary: Type.Primitive.t -> bool;
    has_transitive_successor:
      placeholder_subclass_extends_all:bool ->
      successor:Type.Primitive.t ->
      Type.Primitive.t ->
      bool;
    least_upper_bound: Type.Primitive.t -> Type.Primitive.t -> Type.Primitive.t option;
  }

  let class_summary_for_outer_type { get_class_summary; _ } annotation =
    Type.split annotation |> fst |> Type.primitive_name >>= get_class_summary
end
