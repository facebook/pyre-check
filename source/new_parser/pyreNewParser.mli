(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module contains all parsing APIs, i.e. functions that transfrom plain strings into a list
    of {!type: Ast.Statement.t}.

    Under the hood, it is merely a thin wrapper around the parsing APIs in
    {{:https://grievejia.github.io/pyre-ast/doc/pyre-ast/PyreAst/Parser/index.html} [pyre-ast]},
    specialized for this project. *)

(** This module contains a type that abstracts away the details of global states required to set up
    the parser. *)
module Context : sig
  type t
  (** An opaque type representing the global state of the parser. Obtaining a value of this type is
      a pre-requisite for invoking all other parsing-related APIs.

      A value of this type can be obtained via {!val: PyreNewParser.with_context}. *)
end

(** This module contains a type that represents parsing errors. *)
module Error : sig
  type t = {
    message: string;
    line: int;
    column: int;
    end_line: int;
    end_column: int;
  }
  (** Line numbers start from 1 and column numbers start from 0. *)
end

exception Exception of Error.t
(** Parser exception used for raising APIs.*)

val with_context : ?on_failure:(unit -> 'a) -> (Context.t -> 'a) -> 'a
(** [with_context ?on_failure f] first creates a value [c] of type {!type: Context.t} and then
    invoke [f] on [c]. It is guaranteed that the created context [c] will be destroyed in the end
    regardless of whether [f] raises an exception or not.

    If the creation of [c] fails, [on_failure ()] will be invoked, and [f] will not be called. By
    default, if not explicitly overridden then [on_failure] would simply raise a [Failure]
    exception. *)

val parse_module
  :  ?enable_type_comment:bool ->
  context:Context.t ->
  string ->
  (Ast.Statement.t list, Error.t) Result.t
(** [parse_module ~context input] takes the string [input] and parse it as a Python module,
    represented by a list of {!type: Ast.Statement.t}. See documentation of {!type: Context.t} for
    the meaning of the [context] argument.

    Optionally an [enable_type_comment] argument can be specified. If it is true, the parser will
    attempt to populate the [type_comment] section of each AST node that has it. Otherwise, contents
    in comments will all get ignored and [type_comment] will always be unset. *)

val parse_module_exn
  :  ?enable_type_comment:bool ->
  context:Context.t ->
  string ->
  Ast.Statement.t list
(** Same as [parse_module], except that parsing errors are returned to its caller via exceptions
    instead of [Result.t]. *)

val parse_expression : context:Context.t -> string -> (Ast.Expression.t, Error.t) Result.t
(** [parse_expression ~context input] takes the string [input] and parse it as a Python expression,
    represented by {!type: Ast.Expression.t}. See documentation of {!type: Context.t} for the
    meaning of the [context] argument. *)
