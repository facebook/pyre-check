(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

exception Error of string

module Error : sig
  type t = {
    location: Location.t;
    file_name: string;
    content: string option;
  }
  [@@deriving show]
end

val parse
  :  ?start_line:int ->
  ?start_column:int ->
  ?relative:string ->
  string list ->
  (Statement.t list, Error.t) Result.t
(** Parse python source. ?handle is path relative to the file's source root, if any. *)

val parse_exn
  :  ?start_line:int ->
  ?start_column:int ->
  ?relative:string ->
  string list ->
  Statement.t list
