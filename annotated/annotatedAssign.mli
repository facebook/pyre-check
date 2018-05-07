(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement

module Resolution = AnalysisResolution
module Type = AnalysisType


type t
[@@deriving compare, eq, sexp, show, hash]

val create: Assign.t -> t

val fold
  :  resolution: Resolution.t
  -> initial: 'accumulator
  -> f:
       (access: Access.t Node.t
        -> value_annotation: Type.t
        -> 'accumulator
        -> 'accumulator)
  -> t
  -> 'accumulator
