(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open Statement
module Error = AnalysisError

module Resolution : sig
  type t

  val of_list : (Reference.t * ReadOnlyness.t) list -> t

  val to_alist : t -> (Reference.t * ReadOnlyness.t) list
end

module Resolved : sig
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]
end

module LocalErrorMap : sig
  type t = Error.t list Int.Table.t

  val empty : unit -> t

  val set : statement_key:int -> errors:Error.t list -> t -> unit

  val append : statement_key:int -> error:Error.t -> t -> unit

  val all_errors : t -> Error.t list
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  val error_map : LocalErrorMap.t option
end

module State (Context : Context) : sig
  val forward_expression : resolution:Resolution.t -> Expression.t -> Resolved.t
end

val readonly_errors_for_define
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  qualifier:Reference.t ->
  Define.t Node.t ->
  Error.t list
