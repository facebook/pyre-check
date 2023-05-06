(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

type t = { global_constants: Expression.t Reference.Map.t }

val from_source : Source.t -> t
