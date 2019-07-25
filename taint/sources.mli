(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type t =
  | Cookies
  | Demo
  | Attach
  | NamedSource of string
  | PII
  | Secrets (* Such as passwords, tokens *)
  | Test
  | Thrift
  | UserControlled
[@@deriving compare, eq, sexp, show, hash]

val parse : allowed:string list -> string -> t

val ignore_leaf_at_call : t -> bool
