(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module contains all parsing APIs, i.e. functions that transfrom plain strings into a list
    of {!type: Ast.Statement.t}.

    Under the hood, it invokes errpy then transforms the output errpy AST (which matches CPython
    More details of ERRPY: https://github.com/facebook/errpy **)

(** [parse_module input] takes the string [input] and parse it as a Python module, represented by a
    list of {!type: Ast.Statement.t}.

    Optionally an [enable_type_comment] argument can be specified. It is presently ignored by ERRPY.
    In the future, if it is true, the parser will attempt to populate the [type_comment] section of
    each AST node that has it. Otherwise, contents in comments will all get ignored and
    [type_comment] will always be unset. *)
val parse_module : string -> (Ast.Statement.t list, string) Result.t
