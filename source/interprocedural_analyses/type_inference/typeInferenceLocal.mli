(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
