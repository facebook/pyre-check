(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression

module Type = AnalysisType


type index = {
  function_keys: (Access.t Hash_set.t) String.Table.t;
  class_keys: (Type.t Hash_set.t) String.Table.t;
  alias_keys: (Type.t Hash_set.t) String.Table.t;
  global_keys: (Access.t Hash_set.t) String.Table.t;
  dependent_keys: (string Hash_set.t) String.Table.t;
}

type t = {
  index: index;
  dependents: (string list) String.Table.t;
}

module type Handler = sig
  val add_function_key: path: string -> Access.t -> unit
  val add_class_key: path: string -> Type.t -> unit
  val add_alias_key: path: string -> Type.t -> unit
  val add_global_key: path: string -> Access.t -> unit
  val add_dependent_key: path: string -> string -> unit

  val add_dependent: path: string -> string -> unit

  val dependents: string -> (string list) option

  val get_function_keys: path: string -> Access.t list
  val get_class_keys: path: string -> Type.t list
  val get_alias_keys: path: string -> Type.t list
  val get_global_keys: path: string -> Access.t list
  val get_dependent_keys: path: string -> string list

  val clear_keys_batch: string list -> unit

end

val create: unit -> t

val copy: t -> t

val handler: t -> (module Handler)

val transitive
  :  get_dependencies: (string -> (string list) option)
  -> path: string
  -> String.Set.t

val transitive_of_list
  :  get_dependencies: (string -> (string list) option)
  -> paths: string list
  -> String.Set.t

val of_list
  :  get_dependencies: (string -> (string list) option)
  -> paths: string list
  -> String.Set.t
