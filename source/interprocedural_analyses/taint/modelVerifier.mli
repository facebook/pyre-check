(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis

type incompatible_model_error_reason =
  | UnexpectedPositionalOnlyParameter of string
  | UnexpectedNamedParameter of string
  | UnexpectedStarredParameter
  | UnexpectedDoubleStarredParameter

type verification_error =
  | GlobalVerificationError of {
      name: string;
      message: string;
    }
  | InvalidDefaultValue of {
      callable_name: string;
      name: string;
      expression: Expression.t;
    }
  | IncompatibleModelError of {
      name: string;
      callable_type: Type.Callable.t;
      reasons: incompatible_model_error_reason list;
    }
  | ImportedFunctionModel of {
      name: Reference.t;
      actual_name: Reference.t;
    }
  | ModelledObjectNotInEnvironment
  | UnclassifiedError of string

val display_verification_error
  :  path:Pyre.Path.t option ->
  location:Location.t ->
  verification_error ->
  string

(* Exposed for testing. *)
val demangle_class_attribute : string -> string

val verify_signature
  :  normalized_model_parameters:(AccessPath.Root.t * string * Ast.Expression.Parameter.t) list ->
  name:Reference.t ->
  Type.Callable.t option ->
  (unit, verification_error) result

val verify_global : resolution:Resolution.t -> name:Reference.t -> (unit, verification_error) result
