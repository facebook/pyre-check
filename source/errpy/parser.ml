(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** [parse_module input] takes the string [input] and parse it as a Python module, represented by a
    list of {!type: Ast.mod_}.

    In the future we can add support for type comments here *)
external parse_module
  :  string ->
  (Ast.mod_ * Ast.recoverableerrorwithlocation list, string) result
  = "parse_module"
