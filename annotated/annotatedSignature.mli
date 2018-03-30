(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

module Resolution = AnalysisResolution
module Type = AnalysisType

module Call = AnnotatedCall


type mismatch = {
  actual: Type.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show]

type reason =
  | Mismatch of mismatch Node.t
[@@deriving eq, show]

type closest = {
  rank: int;
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving eq, show]

type t =
  | Found of Type.Callable.t
  | NotFound of closest
[@@deriving eq, show]

val select
  :  Call.t
  -> resolution: Resolution.t
  -> callable: Type.Callable.t
  -> t
