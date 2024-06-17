(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type options = {
  init: bool;
  repr: bool;
  eq: bool;
  order: bool;
  match_args: bool;
  field_specifiers: Ast.Expression.t list;
  keyword_only: bool;
  has_slots: bool;
  frozen: bool;
  unsafe_hash: bool;
}

(** This is necessary as an abstraction over AnnotatedAttribute to determine which attributes are
    keyword_only. *)
type 'annotation dataclass_constructor_parameter = {
  name: Identifier.t;
  annotation: 'annotation;
  default: bool;
  keyword_only: bool;
}

val dataclass_options
  :  first_matching_class_decorator:(names:string list -> 'a -> Ast.Statement.Decorator.t option) ->
  'a ->
  options option

val attrs_attributes
  :  first_matching_class_decorator:(names:string list -> 'a -> Ast.Statement.Decorator.t option) ->
  'a ->
  options option

val options_from_custom_dataclass_transform_decorator
  :  get_unannotated_global:(Ast.Reference.t -> UnannotatedGlobal.t option) ->
  ClassSummary.t Ast.Node.t ->
  options option

val options_from_custom_dataclass_transform_base_class_or_metaclass
  :  get_class_summary:(string -> ClassSummary.t Ast.Node.t option) ->
  successors:(string -> string list) ->
  ClassSummary.t Ast.Node.t ->
  options option
