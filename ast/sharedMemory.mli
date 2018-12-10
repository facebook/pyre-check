(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module SymlinksToPaths: sig
  val get: string -> PyrePath.t option
  val add: string -> PyrePath.t -> unit
  val remove: targets: string list -> unit
end

module Sources: sig
  val get: File.Handle.t -> Source.t option

  val add: File.Handle.t -> Source.t -> unit

  val remove: handles: File.Handle.t list -> unit
end

module HandleKeys: sig
  val get: unit -> File.Handle.t list
  (* Can only be called from the master process. *)
  val clear: unit -> unit
  val add: handles: File.Handle.t list -> unit
end

module Modules: sig
  val add: qualifier: Expression.Access.t -> ast_module: Module.t -> unit

  val remove: qualifiers: Expression.Access.t list -> unit

  val get: qualifier: Expression.Access.t -> Module.t option

  val get_exports: qualifier: Expression.Access.t -> (Expression.Access.t list) option

  val exists: qualifier: Expression.Access.t -> bool
end

module Handles: sig
  val get: hash: int -> string option

  val add_handle_hash: handle: string -> unit
end
