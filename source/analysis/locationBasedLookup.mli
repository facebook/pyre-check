(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type t

val create_of_module : TypeEnvironment.ReadOnly.t -> Reference.t -> t

val get_resolved_type : t -> position:Location.position -> (Location.t * Type.t) option

val get_all_resolved_types : t -> (Location.t * Type.t) list

val get_definition : t -> position:Location.position -> Location.WithModule.t option

val get_all_definitions : t -> (Location.t * Location.WithModule.t) list
