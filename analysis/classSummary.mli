(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

type t = {
  name: Reference.t;
  bases: Expression.Call.Argument.t list;
  decorators: Expression.t list;
  attribute_components: Class.AttributeComponents.t;
}
[@@deriving compare, eq, sexp, show, hash]

val create : Class.t -> t

val is_unit_test : t -> bool

val is_protocol : t -> bool

val has_decorator : t -> string -> bool

val is_final : t -> bool

val is_abstract : t -> bool
