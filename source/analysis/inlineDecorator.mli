(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

val setup_decorator_inlining : decorators_to_skip:Ast.Reference.t list -> enable:bool -> unit

val original_name_from_inlined_name : Ast.Reference.t -> Ast.Reference.t option

(* This is called automatically by the AST environment during preprocessing. *)
val inline_decorators : get_source:(Reference.t -> Source.t option) -> Source.t -> Source.t

(* This is called by `pyre query` for debugging and testing purposes. *)
val inline_decorators_for_define
  :  get_decorator_body:(Reference.t -> Define.t option) ->
  location:Location.t ->
  Define.t ->
  Define.t

(* exposed for testing purposes only. *)
val uniquify_names
  :  get_reference:('a -> Reference.t) ->
  set_reference:(Reference.t -> 'a -> 'a) ->
  'a list ->
  'a list

(* exposed for testing purposes only. *)
val decorator_body
  :  should_skip_decorator:(Reference.t -> bool) ->
  get_source:(Reference.t -> Source.t option) ->
  Reference.t ->
  Define.t option

(* exposed for testing purposes only. *)
val sanitize_defines : strip_decorators:bool -> Source.t -> Source.t

(* exposed for testing purposes only. *)
val requalify_name
  :  old_qualifier:Reference.t ->
  new_qualifier:Reference.t ->
  Expression.Name.t ->
  Expression.Name.t

(* exposed for testing purposes only. *)
val replace_signature_if_always_passing_on_arguments
  :  callee_name:Identifier.t ->
  new_signature:Define.Signature.t ->
  Define.t ->
  Define.t option

(* exposed for testing purposes only. *)
val rename_local_variables : pairs:(Identifier.t * Identifier.t) list -> Define.t -> Define.t
