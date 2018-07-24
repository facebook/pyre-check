(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Expression


type define_filter = {
  name: string option;
  decorator: string option;
  docstring: string option;
  parent: string option;
}

val create_define_filter
  :   ?name: string option
  ->  ?decorator: string option
  ->  ?docstring: string option
  ->  ?parent: string option
  ->  unit
  ->  define_filter

val filter_defines
  :   define_filter: define_filter
  ->  statement Node.t List.t
  ->  Define.t Node.t List.t

type class_filter = {
  name: string option;
  decorator: string option;
  docstring: string option;
  bases: string list option;
  define_filter: define_filter option;
}

val create_class_filter
  :   ?name: string option
  ->  ?bases: string list option
  ->  ?decorator: string option
  ->  ?docstring: string option
  ->  ?define_filter: define_filter option
  ->  unit
  ->  class_filter

val filter_classes
  :   class_filter: class_filter
  ->  statement Node.t List.t
  ->  Class.t Node.t List.t
