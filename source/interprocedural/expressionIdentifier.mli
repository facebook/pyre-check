(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

type t [@@deriving compare, equal, sexp, hash, show]

val location : t -> Location.t

val of_expression : Expression.t -> t

val of_call : location:Location.t -> Call.t -> t

val of_attribute_access : location:Location.t -> Name.Attribute.t -> t

val of_identifier : location:Location.t -> string -> t

val of_format_string_artificial : location:Location.t -> t

val of_format_string_stringify : location:Location.t -> t

val of_define_statement : Location.t -> t

val of_return_statement : Location.t -> t

val pp_json_key : Format.formatter -> t -> unit

val json_key : t -> string

module Map : Data_structures.SerializableMap.S with type key = t
