(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val get_source: File.Handle.t -> Source.t option
val add_source: File.Handle.t -> Source.t -> unit
val remove_paths: File.Handle.t list -> unit

module HandleKeys: sig
  val get: unit -> File.Handle.t list
  (* Can only be called from the master process. *)
  val clear: unit -> unit
  val add: handles: File.Handle.t list -> unit
end

val add_module: Expression.Access.t -> Module.t -> unit
val remove_modules: Expression.Access.t list -> unit
val get_module: Expression.Access.t -> Module.t option
val get_module_exports: Expression.Access.t -> (Expression.Access.t list) option
val in_modules: Expression.Access.t -> bool

val get_handle: hash: int -> string option
val add_handle_hash: handle: string -> unit
