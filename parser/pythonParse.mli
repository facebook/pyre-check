(** Copyright 2016-present Facebook. All rights reserved. **)

open Ast

exception Error of string

(** Parse python source. ?path specifies a relative path of the python file, if
    any. *)
val parse : ?path:string -> string list -> Statement.t list
