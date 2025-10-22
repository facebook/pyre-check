(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
module AstResult = PyrePysaApi.AstResult

module CallableSignature : sig
  type t = {
    qualifier: Reference.t;
    define_name: Reference.t;
    location: Location.t AstResult.t;
    parameters: Expression.Parameter.t list AstResult.t;
    return_annotation: Expression.t option AstResult.t;
    decorators: Expression.t list AstResult.t;
    captures: string list;
    method_kind: Target.MethodKind.t option;
    is_stub_like: bool;
  }

  val from_define_for_pyre1 : target:Target.t -> qualifier:Reference.t -> Define.t Node.t -> t
end

(* Exposed for testing purposes only. *)
val get_signature_and_definition_for_test
  :  pyre_api:PyrePysaApi.ReadOnly.t ->
  Target.t ->
  (CallableSignature.t * Define.t Node.t AstResult.t) option

module DefineAndQualifier : sig
  type t = {
    qualifier: Reference.t;
    define: Define.t Node.t;
  }
end

module ReadWrite : sig
  type t

  val empty : unit -> t

  val from_callables
    :  scheduler:Scheduler.t ->
    scheduler_policy:Scheduler.Policy.t ->
    pyre_api:PyrePysaApi.ReadOnly.t ->
    Target.t list ->
    t

  val cleanup : t -> unit

  val add_alist_sequential : t -> (Target.t * CallableSignature.t * Define.t Node.t) list -> t
end

module ReadOnly : sig
  type t

  val read_only : ReadWrite.t -> t

  val get_define : t -> Target.t -> DefineAndQualifier.t AstResult.t

  val get_location : t -> Target.t -> Ast.Location.WithModule.t AstResult.t

  val get_location_opt : t -> Target.t -> Ast.Location.WithModule.t option

  val get_signature : t -> Target.t -> CallableSignature.t option

  val get_qualifier : t -> Target.t -> Reference.t option

  val get_method_kind : t -> Target.t -> bool * bool

  val is_stub_like : t -> Target.t -> bool option

  val get_captures : t -> Target.t -> string list option

  val callable_from_reference : t -> Reference.t -> Target.t option

  val mem : t -> Target.t -> bool
end

val class_method_decorators : string list

val static_method_decorators : string list
