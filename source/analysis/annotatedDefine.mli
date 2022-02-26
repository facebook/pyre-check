(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
module Callable = AnnotatedCallable
module Class = AnnotatedClass

type t [@@deriving compare, sexp, show, hash]

val create : Define.t Node.t -> t

val define : t -> Define.t Node.t

val parameter_annotations : t -> resolution:GlobalResolution.t -> Type.t Int.Map.t

val parent_definition : t -> resolution:GlobalResolution.t -> ClassSummary.t Node.t option

val decorate : t -> resolution:GlobalResolution.t -> t

val is_constructor : t -> resolution:GlobalResolution.t -> bool
