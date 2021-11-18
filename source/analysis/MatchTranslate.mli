(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
open Expression

val to_condition : subject:Expression.t -> case:Match.Case.t -> Expression.t

val pattern_to_condition : subject:Expression.t -> Match.Pattern.t -> Expression.t
