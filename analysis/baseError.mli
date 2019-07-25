(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement

module type Kind = sig
  type t [@@deriving compare, eq, show, sexp, hash]

  val code : t -> int

  val name : t -> string

  val messages
    :  concise:bool ->
    signature:Define.signature Node.t ->
    Location.Instantiated.t ->
    t ->
    string list

  val inference_information : signature:Define.signature Node.t -> t -> Yojson.Safe.json
end

module type Error = sig
  type kind

  type t = {
    location: Location.Instantiated.t;
    kind: kind;
    signature: Define.signature Node.t;
  }
  [@@deriving compare, eq, show, sexp, hash]

  include Hashable with type t := t

  val create : location:Location.t -> kind:kind -> define:Define.t Node.t -> t

  val kind : t -> kind

  val path : t -> string

  val location : t -> Location.Instantiated.t

  val key : t -> Location.t

  val code : t -> int

  val description : ?separator:string -> ?concise:bool -> t -> show_error_traces:bool -> string

  val to_json : show_error_traces:bool -> t -> Yojson.Safe.json
end

module Make (Kind : Kind) : Error with type kind := Kind.t
