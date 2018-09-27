(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement


module type KIND = sig
  type t
  [@@deriving compare, eq, show, sexp, hash]

  val code: t -> int
  val name: t -> string
  val messages:
    detailed: bool
    -> define: Define.t Node.t
    -> Location.Instantiated.t
    -> t
    -> string list
  val inference_information:
    define: Define.t Node.t
    -> t
    -> Yojson.Safe.json
end


module type ERROR = sig
  type kind

  type t = {
    location: Location.Instantiated.t;
    kind: kind;
    define: Statement.Define.t Node.t;
  }
  [@@deriving compare, eq, show, sexp, hash]

  include Hashable with type t := t

  val create: location: Location.t -> kind: kind -> define: Statement.Define.t Node.t -> t

  val path: t -> string
  val location: t -> Location.Instantiated.t
  val key: t -> Location.t
  val code: t -> int
  val description: t -> detailed: bool -> string

  val to_json: detailed: bool -> t -> Yojson.Safe.json
end


module Make(Kind : KIND): ERROR
  with type kind := Kind.t
