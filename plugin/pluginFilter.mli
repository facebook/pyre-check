(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Expression


type class_filter = {
  name: string option;
  bases: string list option;
  decorator: string option;
  docstring: string option;
}

val create_class_filter
  :   ?name: string option
  ->  ?bases: string list option
  ->  ?decorator: string option
  ->  ?docstring: string option
  ->  unit
  ->  class_filter

val filter_classes
  :   class_filter: class_filter
  ->  statement Node.t List.t
  ->  Class.t Node.t List.t
