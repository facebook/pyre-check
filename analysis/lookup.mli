(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val create_of_module : TypeEnvironment.ReadOnly.t -> Reference.t -> t

val get_annotation : t -> position:Location.position -> (Location.t * Type.t) option

val get_all_annotations : t -> (Location.t * Type.t) list

val get_definition : t -> position:Location.position -> Location.t option

val get_all_definitions : t -> (Location.t * Location.t) list

val get_all_qualified_names : t -> (Location.t * Reference.t) list
