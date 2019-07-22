(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val create_of_source : GlobalResolution.t -> Source.t -> t

val get_annotation : t -> position:Location.position -> (Location.Instantiated.t * Type.t) option

val get_all_annotations : t -> (Location.Instantiated.t * Type.t) list

val get_definition : t -> position:Location.position -> Location.Instantiated.t option

val get_all_definitions : t -> (Location.t * Location.t) list
