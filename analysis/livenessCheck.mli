(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
  type key = Define.t [@@deriving compare, eq, sexp, show, hash]

  include Hashable with type t := key

  type 'data t = 'data Table.t
end

module type Context = sig
  val global_resolution : GlobalResolution.t

  val errors : ErrorMap.t
end

module State (Context : Context) : sig
  type t = {
    used: Identifier.Set.t;
    define: Define.t Node.t;
    nested_define_lookup: t NestedDefineLookup.t;
  }

  val show : t -> string

  val pp : Format.formatter -> t -> unit

  val errors : t -> Error.t list

  val initial : lookup:t NestedDefineLookup.t -> define:Define.t Node.t -> t

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:'a -> t

  val forward : ?key:int -> t -> statement:Statement.t -> t

  val backward : ?key:int -> t -> statement:Statement.t -> t
end

val name : string

val run
  :  configuration:Configuration.Analysis.t ->
  global_resolution:GlobalResolution.t ->
  source:Source.t ->
  Error.t list
