(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Pyre
open Statement

type t = {
  function_definitions: ((Define.t Node.t) list) Access.Table.t;
  class_definitions: (Class.t Node.t) Type.Table.t;
  protocols: Type.Hash_set.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Access.Table.t;
  dependencies: Dependencies.t;
}

(** The reader module is an interface for performing lookups on the type
    environment. It abstracts the underlying data structure, so that we can use
    e.g., in-process hash tables, shared memory, or network streams to provide
    lookups. *)
module type Reader = sig
  val register_definition
    :  path: string
    -> ?name_override: Access.t
    -> (Define.t Node.t)
    -> unit
  val register_dependency: path: string -> dependency: string -> unit
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
  val protocols: Type.Hash_set.t
  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Access.t -> Resolution.global option
  val dependencies: string -> string list option

  module DependencyReader: Dependencies.Reader
  module TypeOrderReader: TypeOrder.Reader
end

(** Provides a default in-process environment reader constructed from an
    [Environment.t]. Use [Environment_service.reader] if interfacing from outside
    [Analysis]. *)
val reader: t -> (module Reader)

val resolution
  :  (module Reader)
  -> ?annotations: Annotation.t Access.Map.t
  -> unit
  -> Resolution.t

val dependencies: (module Reader) -> string -> string list option

val register_type
  :  order: (module TypeOrder.Reader)
  -> aliases: (Type.t -> Type.t option)
  -> add_class_definition: (key: Type.t -> data: Class.t Node.t -> unit)
  -> add_class_key: (path: string -> Type.t -> unit)
  -> add_protocol: (Type.t -> unit)
  -> register_global: (path: string -> key: Access.t -> data: Resolution.global -> unit)
  -> (path: string -> Type.t -> Access.t -> (Class.t Node.t) option -> (Type.t * Type.t list))

val register_class_definitions
  :  (module Reader)
  -> Source.t
  -> unit

val register_aliases
  :  (module Reader)
  -> Source.t list
  -> unit

val connect_type_order
  :  (module Reader)
  -> ?project_root: Path.t
  -> ?check_dependency_exists: bool
  -> Source.t
  -> unit

val populate
  :  (module Reader)
  -> ?project_root: Path.t
  -> ?check_dependency_exists: bool
  -> Source.t list
  -> unit

val infer_implementations: (module Reader) -> protocol: Type.t -> TypeOrder.Edge.Set.t

module Builder : sig
  val create: unit -> t
  val copy: t -> t

  val statistics: t -> string

  val pp: Format.formatter -> t -> unit
  val show: t -> string
end
