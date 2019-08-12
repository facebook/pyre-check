(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

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

let _ = show (* unused *)

let show = function
  | Cookies -> "Cookies"
  | Demo -> "Demo"
  | Attach -> "Attach"
  | NamedSource name -> name
  | PII -> "PII"
  | Secrets -> "Secrets"
  | Test -> "Test"
  | Thrift -> "Thrift"
  | UserControlled -> "UserControlled"


let create = function
  | "Cookies" -> Cookies
  | "PII" -> PII
  | "Secrets" -> Secrets
  | "Demo" -> Demo
  | "Test" -> Test
  | "Thrift" -> Thrift
  | "UserControlled" -> UserControlled
  | name -> failwith (Format.sprintf "Unsupported taint source `%s`" name)


let ignore_leaf_at_call = function
  | Attach -> true
  | _ -> false


let parse ~allowed name =
  if List.mem allowed name ~equal:String.equal then
    NamedSource name
  else
    create name
