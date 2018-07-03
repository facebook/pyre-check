(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

exception Error of string

(** Parse python source. ?path specifies a relative path of the python file, if
    any. *)
val parse
  :  ?start_line: int
  -> ?start_column: int
  -> ?path:string
  -> string list
  -> Statement.t list
