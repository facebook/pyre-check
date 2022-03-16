(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

val decorators_to_skip : path:PyrePath.t -> string -> Reference.t list

val uniquify_names
  :  get_reference:('a -> Reference.t) ->
  set_reference:(Reference.t -> 'a -> 'a) ->
  'a list ->
  'a list

val decorator_body
  :  should_skip_decorator:(Reference.t -> bool) ->
  get_source:(Reference.t -> Source.t option) ->
  Reference.t ->
  Define.t option

val inline_decorators_for_define
  :  get_decorator_body:(Reference.t -> Define.t option) ->
  location:Location.t ->
  Define.t ->
  Define.t

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

(** Mapping from an inlined decorator function name to its original name. *)
module InlinedNameToOriginalName :
  Memory.WithCache.S
    with type value = Ast.Reference.t
     and type key = SharedMemoryKeys.ReferenceKey.t

(** The keys represent the decorators to skip. The values are dont-care values. *)
module DecoratorsToSkip :
  Memory.WithCache.S
    with type value = Ast.Reference.t
     and type key = SharedMemoryKeys.ReferenceKey.t

val inline_decorators : get_source:(Reference.t -> Source.t option) -> Source.t -> Source.t

val set_should_inline_decorators : bool -> unit
