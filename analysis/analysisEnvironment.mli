(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Dependencies = AnalysisDependencies
module Resolution = AnalysisResolution
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type class_representation = {
  class_definition: Class.t Node.t;
  explicit_attributes: Attribute.t Access.SerializableMap.t;
  implicit_attributes: Attribute.t Access.SerializableMap.t;
  is_test: bool;
  methods: Type.t list;
}

type t = {
  function_definitions: ((Define.t Node.t) list) Access.Table.t;
  class_definitions: class_representation Type.Table.t;
  protocols: Type.Hash_set.t;
  modules: Module.t Access.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Access.Table.t;
  dependencies: Dependencies.t;
}

(** The handler module is an interface for performing lookups on the type
    environment. It abstracts the underlying data structure, so that we can use
    e.g., in-process hash tables, shared memory, or network streams to provide
    lookups. *)
module type Handler = sig
  val register_definition
    :  path: string
    -> ?name_override: Access.t
    -> (Define.t Node.t)
    -> unit
  val register_dependency: path: string -> dependency: Access.t -> unit
  val register_global: path: string -> access: Access.t -> global: Resolution.global -> unit
  val update_class_definition: primitive: Type.t -> definition: Class.t -> unit
  val connect_definition
    :  path: string
    -> resolution: Resolution.t
    -> predecessor: Type.t
    -> name: Access.t
    -> definition: (Class.t Node.t) option
    -> (Type.t * Type.t list)
  val refine_class_definition: Type.t -> unit
  val register_alias: path: string -> key: Type.t -> data: Type.t -> unit
  val purge: File.Handle.t list -> unit

  val function_definitions: Access.t -> (Define.t Node.t) list option
  val class_definition: Type.t -> class_representation option
  val protocols: unit -> Type.t list

  val register_module
    :  qualifier: Access.t
    -> local_mode: Source.mode
    -> path: string option
    -> stub: bool
    -> statements: Statement.t list
    -> unit

  val is_module: Access.t -> bool
  val module_definition: Access.t -> Module.t option

  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Access.t -> Resolution.global option
  val dependencies: Access.t -> string list option

  val mode: string -> Source.mode option

  module DependencyHandler: Dependencies.Handler
  module TypeOrderHandler: TypeOrder.Handler
end

(** Provides a default in-process environment handler constructed from an
    [Environment.t]. Use [Environment_service.handler] if interfacing from outside
    [Analysis]. *)
val handler: t -> configuration: Configuration.t -> (module Handler)

val resolution
  :  (module Handler)
  -> ?annotations: Annotation.t Access.Map.t
  -> unit
  -> Resolution.t

val dependencies: (module Handler) -> Access.t -> string list option

val connect_definition
  :  order: (module TypeOrder.Handler)
  -> aliases: (Type.t -> Type.t option)
  -> add_class_definition: (primitive: Type.t -> definition: Class.t Node.t -> unit)
  -> add_class_key: (path: string -> Type.t -> unit)
  -> add_protocol: (Type.t -> unit)
  -> (path: string
      -> resolution: Resolution.t
      -> predecessor: Type.t
      -> name: Access.t
      -> definition: (Class.t Node.t) option
      -> (Type.t * Type.t list))

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

val register_globals: (module Handler) -> Source.t -> unit

val connect_type_order
  :  (module Handler)
  -> Source.t
  -> unit

val register_dependencies
  :  ?source_root: Path.t
  -> ?check_dependency_exists: bool
  -> (module Handler)
  -> Source.t
  -> unit

val register_functions
  :  (module Handler)
  -> Source.t
  -> unit

(* Exposed for testing. *)
val infer_implementations
  :  (module Handler)
  -> implementing_classes: (method_name: Access.t -> (Ast.Statement.Access.t list) option)
  -> protocol: Type.t
  -> TypeOrder.Edge.Set.t
(* Exposed for testing. *)
val infer_protocol_edges
  :  handler: (module Handler)
  -> classes_to_infer: int list
  -> TypeOrder.Edge.Set.t
(* If classes_to_infer is not None, only infers protocols for the specified classes. *)
val infer_protocols: ?classes_to_infer: Type.t list -> handler: (module Handler) -> unit -> unit

module Builder : sig
  val create: unit -> t
  val copy: t -> t

  val statistics: t -> string

  val pp: Format.formatter -> t -> unit
  val show: t -> string
end
