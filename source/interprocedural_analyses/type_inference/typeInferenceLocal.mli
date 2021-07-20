(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type Context = sig
  val configuration : Configuration.Analysis.t

  val qualifier : Ast.Reference.t

  val define : Ast.Statement.Define.t Ast.Node.t
end

module type Signature = sig
  type t [@@deriving eq]

  val create : ?bottom:bool -> resolution:Analysis.Resolution.t -> unit -> t

  val initial : resolution:Analysis.Resolution.t -> t

  val initial_forward : resolution:Analysis.Resolution.t -> t

  val initial_backward : forward:t -> t

  include Analysis.Fixpoint.State with type t := t
end

module State (Context : Context) : Signature

val skip_infer : configuration:Configuration.Analysis.t -> Ast.SourcePath.t -> bool

val infer_for_define
  :  configuration:Configuration.Analysis.t ->
  global_resolution:Analysis.GlobalResolution.t ->
  source:Ast.Source.t ->
  qualifier:TypeInferenceData.SerializableReference.t ->
  define:Ast.Statement.Define.t Ast.Node.t ->
  TypeInferenceData.LocalResult.t

val empty_infer_for_define
  :  configuration:Configuration.Analysis.t ->
  global_resolution:Analysis.GlobalResolution.t ->
  qualifier:TypeInferenceData.SerializableReference.t ->
  define:Ast.Statement.Define.t Ast.Node.t ->
  TypeInferenceData.LocalResult.t

val infer_for_module
  :  configuration:Configuration.Analysis.t ->
  global_resolution:Analysis.GlobalResolution.t ->
  source:Ast.Source.t ->
  TypeInferenceData.LocalResult.t list
