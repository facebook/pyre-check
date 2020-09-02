(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement

module type Kind = sig
  type t [@@deriving compare, eq, show, sexp, hash]

  val code : t -> int

  val name : t -> string

  val messages
    :  concise:bool ->
    signature:Define.Signature.t Node.t ->
    Location.WithPath.t ->
    t ->
    string list

  val inference_information : signature:Define.Signature.t Node.t -> t -> Yojson.Safe.json
end

module type Error = sig
  type kind

  type t = {
    location: Location.WithModule.t;
    kind: kind;
    signature: Define.Signature.t Node.t;
  }
  [@@deriving compare, eq, show, sexp, hash]

  module Instantiated : sig
    type t [@@deriving sexp, compare, eq, show, hash, yojson { strict = false }]

    val location : t -> Location.WithPath.t

    val path : t -> string

    val code : t -> int

    val description : t -> string

    val long_description : t -> string

    val concise_description : t -> string
  end

  include Hashable with type t := t

  val create : location:Location.WithModule.t -> kind:kind -> define:Define.t Node.t -> t

  val path : t -> Reference.t

  val key : t -> Location.WithModule.t

  val code : t -> int

  val instantiate
    :  show_error_traces:bool ->
    lookup:(Reference.t -> string option) ->
    t ->
    Instantiated.t
end

module Make (Kind : Kind) : Error with type kind := Kind.t
