(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

module Action : sig
  type t =
    (* Do not try to inline that decorator, keep it as-is. *)
    | DoNotInline
    (* Remove that decorator from decorated function, assuming it is a no-op. *)
    | Discard
  [@@deriving eq, show]

  val to_mode : t -> string
end

module Configuration : sig
  type t = {
    actions: Action.t Reference.SerializableMap.t;
    enable_inlining: bool;
    enable_discarding: bool;
  }
  [@@deriving eq]

  val disable_preprocessing : t
end

val setup_preprocessing : Configuration.t -> unit

val get_configuration : unit -> Configuration.t option

val original_name_from_inlined_name : Ast.Reference.t -> Ast.Reference.t option

val original_decorators_from_preprocessed_signature : Define.Signature.t -> Expression.t list

(* This is called automatically by the AST environment during preprocessing. *)
val preprocess_source : get_source:(Reference.t -> Source.t option) -> Source.t -> Source.t

(* This is called by `pyre query` for debugging and testing purposes. *)
val inline_decorators_for_define
  :  get_source:(Reference.t -> Source.t option) ->
  get_decorator_action:(Reference.t -> Action.t option) ->
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
val find_decorator_body
  :  get_source:(Reference.t -> Source.t option) ->
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
