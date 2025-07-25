(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

(** Roots representing parameters, locals, and special return value in models. *)
module Root : sig
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | PositionalParameter of {
        position: int;
        name: Identifier.t;
        positional_only: bool;
      }
    | NamedParameter of { name: Identifier.t }
    | StarParameter of { position: int }
    | StarStarParameter of { excluded: Identifier.t list }
    | Variable of Identifier.t
    | CapturedVariable of { name: Identifier.t }
  [@@deriving compare, equal, hash, sexp, show]

  val is_parameter : t -> bool

  val parameter_name : t -> string option

  val prepend_parameter_prefix : string -> string

  val pp_for_issue_handle : Format.formatter -> t -> unit

  val show_for_issue_handle : t -> string

  val pp_for_via_breadcrumb : Format.formatter -> t -> unit

  val show_for_via_breadcrumb : t -> string

  val variable_to_captured_variable : t -> t

  val captured_variable_to_variable : t -> t

  val is_captured_variable : t -> bool

  val sink_port_in_string_combine_functions : t

  module Set : Stdlib.Set.S with type elt = t

  module List : sig
    type nonrec t = t list [@@deriving show]
  end
end

module NormalizedParameter : sig
  type t = {
    root: Root.t;
    (* Qualified name (prefixed with `$parameter$`), ignoring stars. *)
    qualified_name: Identifier.t;
    original: Parameter.t;
  }
end

val normalize_parameters : Parameter.t list -> NormalizedParameter.t list

module Path : sig
  type t = Abstract.TreeDomain.Label.t list [@@deriving compare, equal, show]

  val empty : t

  val is_empty : t -> bool

  val is_prefix : prefix:t -> t -> bool
end

type t = {
  root: Root.t;
  path: Path.t;
}
[@@deriving compare, equal, sexp, hash]

val pp : Format.formatter -> t -> unit

val show : t -> string

val create : Root.t -> Path.t -> t

val extend : t -> path:Path.t -> t

val of_expression : self_variable:Root.t option -> Expression.t -> t option

val get_index : Expression.t -> Abstract.TreeDomain.Label.t

(* Evaluates to the representation of literal strings, integers and enums. *)
val extract_constant_name : Expression.t -> string option

type argument_match = {
  root: Root.t;
  actual_path: Path.t;
  formal_path: Path.t;
}
[@@deriving compare, show]

(* Will preserve the order in which the arguments were matched to formals. *)
val match_actuals_to_formals
  :  Call.Argument.t list ->
  Root.t list ->
  (Call.Argument.t * argument_match list) list

(* Only returns arguments that match with the given formal parameter. *)
val match_actuals_to_one_formal
  :  Call.Argument.t list ->
  Root.t ->
  (Call.Argument.t * argument_match) list

val dictionary_keys : Abstract.TreeDomain.Label.t
