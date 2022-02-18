(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type Context = sig
  val configuration : Configuration.Analysis.t

  val qualifier : Ast.Reference.t

  val define : Ast.Statement.Define.t Ast.Node.t

  val resolution_fixpoint : Analysis.LocalAnnotationMap.t option

  val error_map : Analysis.TypeCheck.LocalErrorMap.t option
end

module type Signature = sig
  type t [@@deriving eq]

  val create : ?bottom:bool -> resolution:Analysis.Resolution.t -> unit -> t

  val initial : resolution:Analysis.Resolution.t -> t

  val initial_forward : resolution:Analysis.Resolution.t -> t

  val initial_backward : forward:t -> t

  val widen_resolution_with_snapshots : t -> t

  include Analysis.Fixpoint.State with type t := t
end

module State (Context : Context) : Signature

val infer_for_define
  :  configuration:Configuration.Analysis.t ->
  global_resolution:Analysis.GlobalResolution.t ->
  source:Ast.Source.t ->
  qualifier:Ast.Reference.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  define:Ast.Statement.Define.t Ast.Node.t ->
  TypeInferenceData.LocalResult.t

val infer_for_module
  :  ?skip_annotated:bool ->
  configuration:Configuration.Analysis.t ->
  global_resolution:Analysis.GlobalResolution.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  Ast.Source.t ->
  TypeInferenceData.LocalResult.t list

module Testing : sig
  val define_names_to_analyze
    :  global_resolution:Analysis.GlobalResolution.t ->
    Ast.Source.t ->
    Ast.Reference.t list
end
