(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

val remove_dot : cursor_position:Location.position -> string -> string

val find_module_reference : cursor_position:Location.position -> string -> Reference.t option

val get_completion_items
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  path:PyrePath.t ->
  cursor_position:Location.position ->
  LanguageServer.Types.CompletionItems.t
