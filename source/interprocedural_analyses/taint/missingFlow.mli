(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Interprocedural

(* Create a symbolic callable representing an unknown callee at a call site. *)
val unknown_callee : location:Location.WithModule.t -> call:Expression.expression -> Target.t

(* Register a model with sinks on all parameters for a symbolic callable that
 * represents an unknown callee, in order to find missing flows. *)
val register_unknown_callee_model : Target.t -> unit
