(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module Resolution = AnalysisResolution
module Type = AnalysisType


val return_annotation: define: Define.t -> resolution: Resolution.t -> Type.t

val apply_decorators: define: Define.t -> resolution: Resolution.t -> Define.t

val create: Define.t list -> resolution: Resolution.t -> Type.Callable.t
