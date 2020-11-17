(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val is_local : Ast.Identifier.t -> bool

(* Evaluates to the representation of literal strings, integers and enums. *)
val extract_constant_name : Ast.Expression.t -> string option
