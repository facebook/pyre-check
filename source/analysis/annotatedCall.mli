(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

(* Redirect a call to `str(e)` to the proper method for any expression `e` *)
val resolve_stringify_call : resolution:Resolution.t -> Expression.t -> string

val redirect_special_calls : resolution:Resolution.t -> Expression.Call.t -> Expression.Call.t
