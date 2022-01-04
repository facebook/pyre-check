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
    [@@deriving sexp, compare]
  end

  type t = {
    kind: Kind.t;
    body: Define.t Node.t;
  }
  [@@deriving sexp, compare]
end

type t = {
  qualifier: Reference.t;
  body: Define.t Node.t option;
  siblings: Sibling.t list;
}
[@@deriving sexp, compare]

val all_bodies : t -> Define.t Node.t list

val collect_defines : Source.t -> (Reference.t * t) list
