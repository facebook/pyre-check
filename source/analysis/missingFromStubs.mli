(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val missing_builtin_globals : UnannotatedGlobal.Collector.Result.t list

val missing_builtin_classes : Statement.Class.t Node.t list

val missing_typing_classes : Statement.Class.t Node.t list

val missing_typing_extensions_classes : Statement.Class.t Node.t list
