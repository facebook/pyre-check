(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

exception Error of string

(** Parse python source. ?handle is path relative to the file's source root, if any. *)
val parse
  :  ?start_line: int
  -> ?start_column: int
  -> ?handle: string
  -> string list
  -> Statement.t list
