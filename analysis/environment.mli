(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Ast
open Statement


type t = {
  class_definitions: Resolution.class_representation Type.Table.t;
  protocols: Type.Hash_set.t;
  modules: Module.t Access.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Reference.Table.t;
  dependencies: Dependencies.t;
}

(** The handler module is an interface for performing lookups on the type
    environment. It abstracts the underlying data structure, so that we can use
    e.g., in-process hash tables, shared memory, or network streams to provide
    lookups. *)
module type Handler = sig
  val register_dependency: handle: File.Handle.t -> dependency: Reference.t -> unit
  val register_global
    :  handle: File.Handle.t
    -> reference: Reference.t
    -> global: Resolution.global
    -> unit
  val set_class_definition: primitive: Type.t -> definition: Class.t Node.t -> unit
  val refine_class_definition: Type.t -> unit
  val register_alias: handle: File.Handle.t -> key: Type.t -> data: Type.t -> unit
  val purge: ?debug: bool -> File.Handle.t list -> unit

  val class_definition: Type.t -> Resolution.class_representation option

  val register_protocol: Type.t -> unit
  val protocols: unit -> Type.t list

  val register_module
    :  qualifier: Access.t
    -> local_mode: Source.mode
    -> handle: File.Handle.t option
    -> stub: bool
    -> statements: Statement.t list
    -> unit

  val is_module: Access.t -> bool
  val module_definition: Access.t -> Module.t option

  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Reference.t -> Resolution.global option
  val dependencies: Reference.t -> File.Handle.Set.Tree.t option

  val local_mode: File.Handle.t -> Source.mode option

  module DependencyHandler: Dependencies.Handler
  module TypeOrderHandler: TypeOrder.Handler
end

(** Provides a default in-process environment handler constructed from an
    [Environment.t]. Use [Environment_service.handler] if interfacing from outside
    [Analysis]. *)
val handler: t -> (module Handler)

val dependencies: (module Handler) -> Reference.t -> File.Handle.Set.Tree.t option

val connect_definition
  :  resolution: Resolution.t
  -> definition: Class.t Node.t
  -> unit

val register_module
  :  (module Handler)
  -> Source.t
  -> unit

val register_class_definitions
  :  (module Handler)
  -> Source.t
  -> Type.Set.t

val register_aliases
  :  (module Handler)
  -> Source.t list
  -> unit

val register_globals: (module Handler) -> Resolution.t -> Source.t -> unit

val connect_type_order
  :  (module Handler)
  -> Resolution.t
  -> Source.t
  -> unit

val register_dependencies
  :  (module Handler)
  -> Source.t
  -> unit

val register_functions
  :  (module Handler)
  -> Resolution.t
  -> Source.t
  -> unit

(* Exposed for testing. *)
val infer_implementations
  :  (module Handler)
  -> Resolution.t
  -> implementing_classes: (method_name: Identifier.t -> (Ast.Reference.t list) option)
  -> protocol: Type.t
  -> TypeOrder.Edge.Set.t
(* Exposed for testing. *)
val infer_protocol_edges
  :  handler: (module Handler)
  -> Resolution.t
  -> classes_to_infer: int list
  -> TypeOrder.Edge.Set.t
(* If classes_to_infer is not None, only infers protocols for the specified classes. *)
val infer_protocols
  :  ?classes_to_infer: Type.t list
  -> handler: (module Handler)
  -> Resolution.t
  -> unit
  -> unit

val propagate_nested_classes: (module Handler) -> Resolution.t -> Type.t -> unit

val built_in_annotations: Type.Set.t

module Builder : sig
  val create: unit -> t
  val copy: t -> t

  val statistics: t -> string

  val pp: Format.formatter -> t -> unit
  val show: t -> string
end
