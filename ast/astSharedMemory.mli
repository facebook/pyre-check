(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Expression = AstExpression
module Module = AstModule
module Source = AstSource


val get_source: File.Handle.t -> Source.t option
val add_source: File.Handle.t -> Source.t -> unit

val remove_paths: File.Handle.t list -> unit

val add_module: Expression.Access.t -> Module.t -> unit
val remove_modules: Expression.Access.t list -> unit
val get_module: Expression.Access.t -> Module.t option
val get_module_exports: Expression.Access.t -> (Expression.Access.t list) option
val in_modules: Expression.Access.t -> bool
