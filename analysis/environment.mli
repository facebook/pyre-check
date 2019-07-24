(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

(** The handler module is an interface for performing lookups on the type environment. It abstracts
    the underlying data structure, so that we can use e.g., in-process hash tables, shared memory,
    or network streams to provide lookups. *)
module type Handler = sig
  val register_dependency : qualifier:Reference.t -> dependency:Reference.t -> unit

  val register_global
    :  ?qualifier:Reference.t ->
    reference:Reference.t ->
    global:GlobalResolution.global ->
    unit

  val register_undecorated_function
    :  reference:Reference.t ->
    annotation:Type.t Type.Callable.overload ->
    unit

  val set_class_definition : name:Identifier.t -> definition:Class.t Node.t -> unit

  val register_class_metadata : Identifier.t -> unit

  val register_alias : qualifier:Reference.t -> key:Identifier.t -> data:Type.alias -> unit

  val purge : ?debug:bool -> Reference.t list -> unit

  val class_definition : Identifier.t -> Class.t Node.t option

  val class_metadata : Identifier.t -> GlobalResolution.class_metadata option

  val register_module : Reference.t -> Module.t -> unit

  val register_implicit_submodule : Reference.t -> unit

  val is_module : Reference.t -> bool

  val module_definition : Reference.t -> Module.t option

  val in_class_definition_keys : Identifier.t -> bool

  val aliases : Identifier.t -> Type.alias option

  val globals : Reference.t -> GlobalResolution.global option

  val undecorated_signature : Reference.t -> Type.t Type.Callable.overload option

  val dependencies : Reference.t -> Reference.Set.Tree.t option

  val local_mode : string -> Source.mode option

  (* Instead of writing values to shared memory, changes to shared memory are cached locally in
     this block. Forking off via Scheduler.map_reduce will ignore these caches. is called. *)
  val transaction : f:(unit -> 'a) -> unit -> 'a

  module DependencyHandler : Dependencies.Handler

  module TypeOrderHandler : ClassHierarchy.Handler
end

val add_special_classes : (module Handler) -> unit

val add_dummy_modules : (module Handler) -> unit

val add_special_globals : (module Handler) -> unit

val in_process_handler : ?dependencies:Dependencies.t -> unit -> (module Handler)
(** Provides a default in-process environment handler, only for tests *)

val resolution : (module Handler) -> unit -> GlobalResolution.t

val dependencies : (module Handler) -> Reference.t -> Reference.Set.Tree.t option

val connect_definition
  :  (module Handler) ->
  resolution:GlobalResolution.t ->
  definition:Class.t Node.t ->
  unit

val register_module : (module Handler) -> Source.t -> unit

val register_implicit_submodules : (module Handler) -> Reference.t -> unit

val register_class_definitions : (module Handler) -> Source.t -> Type.Primitive.Set.t

val register_aliases : (module Handler) -> Source.t list -> unit

val register_undecorated_functions : (module Handler) -> GlobalResolution.t -> Source.t -> unit

val register_values : (module Handler) -> GlobalResolution.t -> Source.t -> unit

val connect_type_order : (module Handler) -> GlobalResolution.t -> Source.t -> unit

val register_dependencies : (module Handler) -> Source.t -> unit

val propagate_nested_classes : (module Handler) -> Source.t -> unit

val built_in_annotations : Type.Primitive.Set.t
