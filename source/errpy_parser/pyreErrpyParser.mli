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

(** This module contains a type that represents syntax errors. *)
module SyntaxError : sig
  type t = {
    line: int;
    column: int;
    end_line: int;
    end_column: int;
    message: string;
  }
end

(** This module contains a type that represents a recoverable or unrecoverable parser error. *)
module ParserError : sig
  type t =
    | Recoverable of {
        recovered_ast: Ast.Statement.t list;
        errors: SyntaxError.t list;
      }
    | Unrecoverable of string
end

(** [parse_module input] takes the string [input] and parse it as a Python module, represented by a
    list of {!type: Ast.Statement.t}.

    ERRPY will try to recover as much AST as possible from the input string without reporting of
    errors.

    An Error of [ParserError.t] can be returned of the following two variants:

    1. Recoverable - There are syntax errors in the input, but an AST can still be recovered

    2. Unrecoverable - Where ERRPY has thrown a stacktrace on input. Note that this is not usually
    expected and probably indicates a bug in ERRPY. *)
val parse_module : string -> (Ast.Statement.t list, ParserError.t) Result.t
