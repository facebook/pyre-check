(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

val verify_signature
  :  path:PyrePath.t option ->
  location:Location.t ->
  normalized_model_parameters:Analysis.TaintAccessPath.NormalizedParameter.t list ->
  name:Reference.t ->
  imported_name:Reference.t option ->
  Interprocedural.PyrePysaApi.ModelQueries.FunctionSignature.t list option ->
  (unit, ModelVerificationError.t) result

val verify_global_attribute
  :  path:PyrePath.t option ->
  location:Location.t ->
  pyre_api:Interprocedural.PyrePysaApi.ReadOnly.t ->
  name:Reference.t ->
  (unit, ModelVerificationError.t) result

val filter_unused_stdlib_modules_errors
  :  ModelVerificationError.t list ->
  ModelVerificationError.t list
