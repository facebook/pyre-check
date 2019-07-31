(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement

type t

val add_special_classes : t -> unit

val add_dummy_modules : t -> unit

val add_special_globals : t -> unit

val resolution : t -> unit -> GlobalResolution.t

val dependencies : t -> Reference.t -> Reference.Set.Tree.t option

val connect_definition : t -> resolution:GlobalResolution.t -> definition:Class.t Node.t -> unit

val register_module : t -> Source.t -> unit

val register_implicit_submodules : t -> Reference.t -> unit

val register_class_definitions : t -> Source.t -> Type.Primitive.Set.t

val register_aliases : t -> Source.t list -> unit

val register_undecorated_functions : t -> GlobalResolution.t -> Source.t -> unit

val register_values : t -> GlobalResolution.t -> Source.t -> unit

val connect_type_order : t -> GlobalResolution.t -> Source.t -> unit

val register_dependencies : t -> Source.t -> unit

val propagate_nested_classes : t -> Source.t -> unit

val built_in_annotations : Type.Primitive.Set.t

val is_module : t -> Reference.t -> bool

val purge : t -> ?debug:bool -> Reference.t list -> unit

val class_hierarchy : t -> (module ClassHierarchy.Handler)

val dependency_handler : t -> (module Dependencies.Handler)

val set_class_definition : t -> name:Identifier.t -> definition:Class.t Node.t -> unit

val register_class_metadata : t -> Identifier.t -> unit

val transaction : t -> ?only_global_keys:bool -> f:(unit -> 'a) -> unit -> 'a

val shared_memory_handler : unit -> t

val normalize_shared_memory : Reference.t list -> unit

val fill_shared_memory_with_default_typeorder : unit -> unit

val shared_memory_hash_to_key_map : qualifiers:Ast.Reference.t list -> unit -> string String.Map.t

val serialize_decoded : Memory.decodable -> (string * string * string sexp_option) sexp_option

val decoded_equal : Memory.decodable -> Memory.decodable -> bool
