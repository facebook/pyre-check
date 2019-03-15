(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module HandleKey: Memory.KeyType with type t = File.Handle.t and type out = File.Handle.t

module AccessKey: Memory.KeyType with
  type t = Expression.Access.t and
type out = Expression.Access.t

module IntKey: Memory.KeyType with type t = int and type out = int

module SymlinksToPaths: sig
  val get: string -> PyrePath.t option

  val add: string -> PyrePath.t -> unit

  val remove: targets: string list -> unit

  (* Exposed for testing. *)
  val hash_of_key: string -> string
  val serialize_key: string -> string

  val compute_hashes_to_keys: keys: string list -> string String.Map.t
end

module Sources: sig
  val get: File.Handle.t -> Source.t option

  val get_for_qualifier: Expression.Access.t -> Source.t option

  val add: File.Handle.t -> Source.t -> unit

  val remove: handles: File.Handle.t list -> unit

  (* Exposed for testing. *)
  val hash_of_handle: File.Handle.t -> string
  val serialize_handle: File.Handle.t -> string
  val hash_of_qualifier: Expression.Access.t -> string
  val serialize_qualifier: Expression.Access.t -> string

  val compute_hashes_to_keys: keys: File.Handle.t list -> string String.Map.t
end

module HandleKeys: sig
  val get: unit -> File.Handle.Set.Tree.t

  val add: handles: File.Handle.Set.Tree.t -> unit

  (* Can only be called from the master process. *)
  val clear: unit -> unit

  val normalize: unit -> unit

  (* Exposed for testing. *)
  val hash_of_key: int -> string
  val serialize_key: int -> string


  val compute_hashes_to_keys: unit -> string String.Map.t
end

module Modules: sig
  val get: qualifier: Expression.Access.t -> Module.t option

  val get_exports: qualifier: Expression.Access.t -> (Expression.Access.t list) option

  val add: qualifier: Expression.Access.t -> ast_module: Module.t -> unit

  val remove: qualifiers: Expression.Access.t list -> unit

  val exists: qualifier: Expression.Access.t -> bool

  (* Exposed for testing. *)
  val hash_of_key: Expression.Access.t -> string
  val serialize_key: Expression.Access.t -> string

  val compute_hashes_to_keys: keys: Expression.Access.t list -> string String.Map.t
end

module Handles: sig
  val get: hash: int -> string option

  val add_handle_hash: handle: string -> unit

  (* Exposed for testing. *)
  val hash_of_key: int -> string
  val serialize_key: int -> string

  val compute_hashes_to_keys: keys: string list -> string String.Map.t
end
