(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

type t =
  (* Identifier for a regular expression, as returned by the parser. *)
  | Regular of Location.t
  (* Identifier for all expressions generated from the different preprocessing steps. *)
  | ArtificialAttributeAccess of Origin.t
  | ArtificialCall of Origin.t
  | ArtificialComparisonOperator of Origin.t
  | ArtificialBinaryOperator of Origin.t
  | ArtificialUnaryOperator of Origin.t
  | ArtificialBooleanOperator of Origin.t
  | ArtificialSubscript of Origin.t
  | ArtificialWalrusOperator of Origin.t
  | ArtificialSlice of Origin.t
  | ArtificialAwait of Origin.t
  (* Qualification creates identifiers without an origin. *)
  | IdentifierExpression of {
      location: Location.t;
      identifier: string;
    }
  | FormatStringArtificial of Location.t
  | FormatStringStringify of Location.t
  (* Identifier for constant expression.
   *
   * This is necessary because we sometimes create constant expressions during preprocessing:
   * - `Expression.normalize` can create True/False constant nodes with the same location
   * - `Preprocessing.expand_named_tuples` can create string literals
   * - `Cfg.MatchTranslate` can create integer constants
   * - `Cfg.create` can create ellipsis for try handlers
   *)
  | Constant of {
      location: Location.t;
      constant: Constant.t;
    }
[@@deriving compare, equal, sexp, hash, show]

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
