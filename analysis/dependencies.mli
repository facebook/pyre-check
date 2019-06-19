(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
module SharedMemory = Memory

type index = {
  function_keys: Reference.t Hash_set.t File.Handle.Table.t;
  class_keys: Identifier.t Hash_set.t File.Handle.Table.t;
  alias_keys: Identifier.t Hash_set.t File.Handle.Table.t;
  global_keys: Reference.t Hash_set.t File.Handle.Table.t;
  dependent_keys: Reference.t Hash_set.t File.Handle.Table.t
}

type t = {
  index: index;
  dependents: Reference.Set.t Reference.Table.t
}

module type Handler = sig
  val add_function_key : handle:File.Handle.t -> Reference.t -> unit

  val add_class_key : handle:File.Handle.t -> Identifier.t -> unit

  val add_alias_key : handle:File.Handle.t -> Identifier.t -> unit

  val add_global_key : handle:File.Handle.t -> Reference.t -> unit

  val add_dependent_key : handle:File.Handle.t -> Reference.t -> unit

  val add_dependent : handle:File.Handle.t -> Reference.t -> unit

  val dependents : Reference.t -> Reference.Set.Tree.t option

  val get_function_keys : handle:File.Handle.t -> Reference.t list

  val get_class_keys : handle:File.Handle.t -> Identifier.t list

  val get_alias_keys : handle:File.Handle.t -> Identifier.t list

  val get_global_keys : handle:File.Handle.t -> Reference.t list

  val get_dependent_keys : handle:File.Handle.t -> Reference.t list

  val clear_keys_batch : File.Handle.t list -> unit

  val normalize : File.Handle.t list -> unit
end

val create : unit -> t

val copy : t -> t

val handler : t -> (module Handler)

val transitive_of_list
  :  get_dependencies:(Reference.t -> Reference.Set.Tree.t option) ->
  modules:Reference.t list ->
  Reference.Set.t

val of_list
  :  get_dependencies:(Reference.t -> Reference.Set.Tree.t option) ->
  modules:Reference.t list ->
  Reference.Set.t

val to_dot
  :  get_dependencies:(Reference.t -> Reference.Set.Tree.t option) ->
  qualifier:Reference.t ->
  string

module Callgraph : sig
  type dispatch =
    | Dynamic
    | Static

  and callee =
    | Function of Reference.t
    | Method of { direct_target: Reference.t; static_target: Reference.t; dispatch: dispatch }
  [@@deriving show]

  module CalleeValue : Value.Type with type t = callee list

  module SharedMemory : module type of SharedMemory.WithCache (Reference.Key) (CalleeValue)

  val set : caller:Reference.t -> callees:callee list -> unit

  val get : caller:Reference.t -> callee list
end
