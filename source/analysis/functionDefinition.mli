(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

module Sibling : sig
  module Kind : sig
    type t =
      | Overload
      | PropertySetter
    [@@deriving sexp, equal, compare]
  end

  type t = {
    kind: Kind.t;
    body: Define.t Node.t;
  }
  [@@deriving sexp, equal, compare]
end

type t = {
  qualifier: Reference.t;
  body: Define.t Node.t option;
  siblings: Sibling.t list;
}
[@@deriving sexp, equal, compare]

val qualified_name_of_signature : module_name:Reference.t -> Define.Signature.t -> Reference.t

val qualified_name_of_define : module_name:Reference.t -> Define.t -> Reference.t

val all_bodies : t -> Define.t Node.t list

val body_for_location : t -> location:Location.t -> Define.t Node.t option

val collect_defines : Source.t -> (Reference.t * t) list
