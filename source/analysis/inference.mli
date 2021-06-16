(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

module type Context = sig
  val configuration : Configuration.Analysis.t

  val qualifier : Reference.t

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

val infer_for_define
  :  configuration:Configuration.Analysis.t ->
  global_resolution:GlobalResolution.t ->
  source:Source.t ->
  define:Define.t Node.t ->
  InferenceError.t list

val skip_infer : configuration:Configuration.Analysis.t -> SourcePath.t -> bool

val run
  :  configuration:Configuration.Analysis.t ->
  global_resolution:GlobalResolution.t ->
  source:Source.t ->
  InferenceError.t list
