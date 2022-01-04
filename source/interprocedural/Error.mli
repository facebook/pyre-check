(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type kind = {
  code: int;
  name: string;
  messages: string list;
}
[@@deriving sexp, compare]

type t [@@deriving sexp, compare]

val code : t -> int

val create : location:Location.WithModule.t -> kind:kind -> define:Statement.Define.t Node.t -> t

module Instantiated : sig
  type t

  val code : t -> int

  val location : t -> Location.WithPath.t

  val description : t -> string

  val to_yojson : t -> Yojson.Safe.t
end

val instantiate
  :  show_error_traces:bool ->
  lookup:(Reference.t -> string option) ->
  t ->
  Instantiated.t
