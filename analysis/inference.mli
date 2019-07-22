(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
module Error = AnalysisError

module type Context = sig
  val configuration : Configuration.Analysis.t

  val define : Define.t Node.t
end

module type Signature = sig
  type t [@@deriving eq]

  val create : ?bottom:bool -> resolution:Resolution.t -> unit -> t

  val initial : resolution:Resolution.t -> t

  val initial_forward : resolution:Resolution.t -> t

  val initial_backward : forward:t -> t

  include Fixpoint.State with type t := t
end

module State (Context : Context) : Signature

val name : string

val run
  :  configuration:Configuration.Analysis.t ->
  global_resolution:GlobalResolution.t ->
  source:Source.t ->
  Error.t list
