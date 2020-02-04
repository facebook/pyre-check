(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val create : AstEnvironment.ReadOnly.t -> t

val of_list : t -> modules:Reference.t list -> Reference.Set.t

val to_dot : t -> qualifier:Reference.t -> string

val register_all_dependencies : t -> Source.t list -> unit

val add_manual_dependency_for_test : t -> source:Reference.t -> target:Reference.t -> unit

val normalize : t -> Reference.t list -> unit

val purge : t -> Reference.t list -> unit
