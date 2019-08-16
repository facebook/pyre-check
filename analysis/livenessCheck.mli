(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open CustomAnalysis
module Error = AnalysisError

module type Context = sig end

module State (Context : Context) : sig
  type t = {
    unused: Location.Reference.Set.t Identifier.Map.t;
    define: Define.t Node.t;
    nested_defines: t NestedDefines.t;
  }

  val show : t -> string

  val pp : Format.formatter -> t -> unit

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
