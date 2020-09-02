(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis

(* Exposed for testing. *)
val demangle_class_attribute : string -> string

val verify_signature
  :  normalized_model_parameters:(AccessPath.Root.t * string * Ast.Expression.Parameter.t) list ->
  name:Reference.t ->
  Type.Callable.t option ->
  unit
