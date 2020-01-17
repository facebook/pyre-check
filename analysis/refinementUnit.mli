(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t [@@deriving show]

val create : ?base:Annotation.t -> ?attribute_refinements:t Identifier.Map.t -> unit -> t

val add_attribute_refinement : t -> reference:Reference.t -> base:Annotation.t -> t

val annotation : t -> reference:Reference.t -> Annotation.t option

val base : t -> Annotation.t option
