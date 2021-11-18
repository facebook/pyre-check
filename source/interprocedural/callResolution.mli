(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis

(* Evaluates to the representation of literal strings, integers and enums. *)
val extract_constant_name : Expression.t -> string option

(* Evaluates to whether the provided expression is a superclass of define. *)
val is_super : resolution:Resolution.t -> define:Statement.Define.t Node.t -> Expression.t -> bool
