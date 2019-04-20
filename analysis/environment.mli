(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Ast
open Statement


type t = {
  class_definitions: (Class.t Node.t) Identifier.Table.t;
  class_metadata: Resolution.class_metadata Identifier.Table.t;
  protocols: Identifier.Hash_set.t;
  modules: Module.t Reference.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Reference.Table.t;
  dependencies: Dependencies.t;
  undecorated_functions: Type.t Type.Callable.overload Reference.Table.t;
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
  val register_undecorated_function
    :  reference: Reference.t
    -> annotation: Type.t Type.Callable.overload
    -> unit
  val set_class_definition: name: Identifier.t -> definition: Class.t Node.t -> unit
  val register_class_metadata: Identifier.t -> unit
  val register_alias: handle: File.Handle.t -> key: Type.t -> data: Type.t -> unit
  val purge: ?debug: bool -> File.Handle.t list -> unit

  val class_definition: Identifier.t -> Class.t Node.t option
  val class_metadata: Identifier.t -> Resolution.class_metadata option

  val register_protocol: handle: File.Handle.t -> Identifier.t -> unit
  val protocols: unit -> Identifier.t list

  val register_module
    :  qualifier: Reference.t
    -> local_mode: Source.mode
    -> handle: File.Handle.t option
    -> stub: bool
    -> statements: Statement.t list
    -> unit

  val is_module: Reference.t -> bool
  val module_definition: Reference.t -> Module.t option

  val in_class_definition_keys: Identifier.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Reference.t -> Resolution.global option
  val undecorated_signature: Reference.t -> Type.t Type.Callable.overload option
  val dependencies: Reference.t -> Reference.Set.Tree.t option

  val local_mode: File.Handle.t -> Source.mode option

  (* Instead of writing values to shared memory, changes to shared memory are cached locally in this
     block. Forking off via Scheduler.map_reduce will ignore these caches.
     is called. *)
  val transaction: f:(unit -> 'a) -> unit -> 'a

  module DependencyHandler: Dependencies.Handler
  module TypeOrderHandler: TypeOrder.Handler
end

(** Provides a default in-process environment handler constructed from an
    [Environment.t]. Use [Environment_service.handler] if interfacing from outside
    [Analysis]. *)
val handler: t -> (module Handler)

val dependencies: (module Handler) -> Reference.t -> Reference.Set.Tree.t option

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

val register_undecorated_functions: (module Handler) -> Resolution.t -> Source.t -> unit

val register_values: (module Handler) -> Resolution.t -> Source.t -> unit

val connect_type_order
  :  (module Handler)
  -> Resolution.t
  -> Source.t
  -> unit

val register_dependencies
  :  (module Handler)
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
  :  ?classes_to_infer: Identifier.t list
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
