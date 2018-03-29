(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Type = AnalysisType

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Call = AnnotatedCall
module Access = AnnotatedAccess
module Signature = AnnotatedSignature


val resolve
  :  resolution: Resolution.t
  -> Expression.t
  -> Type.t

val resolve_literal
  :  resolution: Resolution.t
  -> Expression.t
  -> Type.t
