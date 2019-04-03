(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast


type index = {
  function_keys: (Reference.t Hash_set.t) File.Handle.Table.t;
  class_keys: (Type.t Hash_set.t) File.Handle.Table.t;
  alias_keys: (Type.t Hash_set.t) File.Handle.Table.t;
  global_keys: (Reference.t Hash_set.t) File.Handle.Table.t;
  dependent_keys: (Reference.t Hash_set.t) File.Handle.Table.t;
  protocol_keys: (Type.t Hash_set.t) File.Handle.Table.t;
}

type t = {
  index: index;
  dependents: (File.Handle.Set.t) Reference.Table.t;
}

module type Handler = sig
  val add_function_key: handle: File.Handle.t -> Reference.t -> unit
  val add_class_key: handle: File.Handle.t -> Type.t -> unit
  val add_alias_key: handle: File.Handle.t -> Type.t -> unit
  val add_global_key: handle: File.Handle.t -> Reference.t -> unit
  val add_dependent_key: handle: File.Handle.t -> Reference.t -> unit
  val add_protocol_key: handle: File.Handle.t -> Type.t -> unit

  val add_dependent: handle: File.Handle.t -> Reference.t -> unit

  val dependents: Reference.t -> File.Handle.Set.Tree.t option

  val get_function_keys: handle: File.Handle.t -> Reference.t list
  val get_class_keys: handle: File.Handle.t -> Type.t list
  val get_alias_keys: handle: File.Handle.t -> Type.t list
  val get_global_keys: handle: File.Handle.t -> Reference.t list
  val get_dependent_keys: handle: File.Handle.t -> Reference.t list
  val get_protocol_keys: handle: File.Handle.t -> Type.t list

  val clear_keys_batch: File.Handle.t list -> unit

  val normalize: File.Handle.t list -> unit
end

val create: unit -> t

val copy: t -> t

val handler: t -> (module Handler)

val transitive_of_list
  :  get_dependencies: (File.Handle.t -> File.Handle.Set.Tree.t option)
  -> handles: File.Handle.t list
  -> File.Handle.Set.t

val of_list
  :  get_dependencies: (File.Handle.t -> File.Handle.Set.Tree.t option)
  -> handles: File.Handle.t list
  -> File.Handle.Set.t

val to_dot
  :  get_dependencies: (Reference.t -> File.Handle.Set.Tree.t option)
  -> handle: File.Handle.t
  -> string
