(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement

module Callable = AnnotatedCallable
module Class = AnnotatedClass


type t
[@@deriving compare, eq, sexp, show, hash]

val create: Define.t -> t

val define: t -> Define.t

val is_generator: t -> bool

val parameter_annotations
  :  t
  -> resolution: Resolution.t
  -> Type.t Int.Map.t

val parent_definition: t -> resolution: Resolution.t -> Class.t option
