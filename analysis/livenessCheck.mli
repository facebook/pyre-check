(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement
open CustomAnalysis
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

module type Context = sig
  val global_resolution : GlobalResolution.t

  val errors : ErrorMap.t
end

module State (Context : Context) : sig
  type t = {
    unused: Location.Reference.Set.t Identifier.Map.t;
    bottom: bool;
    define: Define.t Node.t;
    nested_defines: t NestedDefines.t;
  }

  val show : t -> string

  val pp : Format.formatter -> t -> unit

  val errors : t -> Error.t list

  val initial : state:t option -> define:Define.t Node.t -> t

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:'a -> t

  val nested_defines : t -> t NestedDefines.t

  val forward : ?key:int -> t -> statement:Statement.t -> t
end

val name : string

val run
  :  configuration:Configuration.Analysis.t ->
  global_resolution:GlobalResolution.t ->
  source:Source.t ->
  Error.t list
