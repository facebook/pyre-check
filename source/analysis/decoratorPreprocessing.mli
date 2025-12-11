(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module Action : sig
  type t = (* Remove that decorator from decorated function, assuming it is a no-op. *)
    | Discard
  [@@deriving compare, equal, show, sexp]

  val to_mode : t -> string

  module Set : Stdlib.Set.S with type elt = t
end

module Configuration : sig
  type t = {
    actions: Action.t Reference.SerializableMap.t;
    enable_discarding: bool;
  }
  [@@deriving compare, equal, sexp]

  val disable_preprocessing : t
end

val has_any_decorator_action : actions:Action.Set.t -> Expression.t -> bool

val setup_preprocessing : Configuration.t -> unit

val get_configuration : unit -> Configuration.t option

val original_decorators_from_preprocessed_signature
  :  define_name:Reference.t ->
  decorators:Expression.t list ->
  Expression.t list

(* This is called automatically by the AST environment during preprocessing. *)
val preprocess_source : Source.t -> Source.t
