(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
module Error = AnalysisError

module ErrorMap : sig
  type key = {
    location: Location.t;
    identifier: string;
  }
  [@@deriving compare, eq, sexp, show, hash]

  include Hashable with type t := key

  type t = Error.t Table.t
end

module NestedDefineLookup : sig
  type key = Reference.t [@@deriving compare, eq, sexp, show, hash]

  include Hashable with type t := key

  type t = Identifier.Set.t Table.t
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val environment : TypeEnvironment.ReadOnly.t

  val errors : ErrorMap.t

  val nested_define_lookup : NestedDefineLookup.t
end

module State (Context : Context) : sig
  type t = {
    used: Identifier.Set.t;
    local_annotations: LocalAnnotationMap.ReadOnly.t option;
  }

  val show : t -> string

  val pp : Format.formatter -> t -> unit

  val errors : t -> Error.t list

  val initial : t

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:int -> t

  val forward : statement_key:int -> t -> statement:Statement.t -> t

  val backward : statement_key:int -> t -> statement:Statement.t -> t
end

val name : string

val run
  :  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  source:Source.t ->
  Error.t list
