(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement

module Resolution = AnalysisResolution
module Type = AnalysisType

module Callable = AnnotatedCallable
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method


type t
[@@deriving compare, eq, sexp, show, hash]

val create: Define.t -> t
val create_toplevel: Statement.t list -> t

val define: t -> Define.t

val parameter_annotations
  :  t
  -> resolution: Resolution.t
  -> Type.t Identifier.Map.t
val parameter_annotations_positional
  :  t
  -> resolution: Resolution.t
  -> Type.t Int.Map.t

val parent_definition: t -> resolution: Resolution.t -> Class.t option
val method_definition: t -> resolution: Resolution.t -> Method.t option

val infer_argument_name
  :  t
  -> index: int
  -> argument: Argument.t
  -> Identifier.t option
