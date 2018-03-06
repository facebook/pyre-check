(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Dependencies = AnalysisDependencies
module Resolution = AnalysisResolution
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder

type t = {
  function_definitions: ((Define.t Node.t) list) Access.Table.t;
  class_definitions: (Class.t Node.t) Type.Table.t;
  protocols: Type.Hash_set.t;
  modules: unit Access.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Access.Table.t;
  dependencies: Dependencies.t;
  ignore_lines: Source.Ignore.t Location.Table.t;
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
  val register_dependency: path: string -> dependency: string -> unit
  val register_ignore_line: path: string -> ignore:Source.Ignore.t -> unit
  val register_global: path: string -> key: Access.t -> data:Resolution.global -> unit
  val register_type
    :  path: string
    -> Type.t
    -> Access.t
    -> (Class.t Node.t) option
    -> (Type.t * Type.t list)
  val register_alias: path: string -> key: Type.t -> data: Type.t -> unit
  val purge: File.Handle.t -> unit

  val function_definitions: Access.t -> (Define.t Node.t) list option
  val class_definition: Type.t -> (Class.t Node.t) option
  val protocols: unit -> Type.t list

  val register_module: Access.t -> unit
  val is_module: Access.t -> bool

  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Access.t -> Resolution.global option
  val dependencies: string -> string list option
  val ignore_lines: Location.t -> Source.Ignore.t option

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

val dependencies: (module Handler) -> string -> string list option

val register_type
  :  order: (module TypeOrder.Handler)
  -> configuration: Configuration.t
  -> aliases: (Type.t -> Type.t option)
  -> add_class_definition: (primitive: Type.t -> definition: Class.t Node.t -> unit)
  -> add_class_key: (path: string -> Type.t -> unit)
  -> add_protocol: (Type.t -> unit)
  -> register_global: (path: string -> key: Access.t -> data: Resolution.global -> unit)
  -> (path: string -> Type.t -> Access.t -> (Class.t Node.t) option -> (Type.t * Type.t list))

val register_class_definitions
  :  (module Handler)
  -> Source.t
  -> unit

val register_aliases
  :  (module Handler)
  -> Source.t list
  -> unit

val connect_type_order
  :  (module Handler)
  -> ?source_root: Path.t
  -> ?check_dependency_exists: bool
  -> Source.t
  -> unit

val populate
  :  (module Handler)
  -> configuration: Configuration.t
  -> ?source_root: Path.t
  -> ?check_dependency_exists: bool
  -> Source.t list
  -> unit

val infer_implementations: (module Handler) -> protocol: Type.t -> TypeOrder.Edge.Set.t

module Builder : sig
  val create: configuration: Configuration.t -> unit -> t
  val copy: t -> t

  val statistics: t -> string

  val pp: Format.formatter -> t -> unit
  val show: t -> string
end
