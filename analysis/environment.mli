(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type t

val add_special_globals : t -> unit

val ast_environment : t -> AstEnvironment.ReadOnly.t

val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t

val resolution : t -> unit -> GlobalResolution.t

(* Currently experimental *)
val dependency_tracked_resolution : t -> dependency:Reference.t -> unit -> GlobalResolution.t

val register_undecorated_functions : t -> GlobalResolution.t -> Reference.t -> unit

val register_values : t -> GlobalResolution.t -> Reference.t -> unit

val is_module : t -> Reference.t -> bool

val check_class_hierarchy_integrity : t -> unit

val purge
  :  t ->
  ?debug:bool ->
  Reference.t list ->
  update_result:ClassMetadataEnvironment.UpdateResult.t ->
  unit

val update_and_compute_dependencies
  :  t ->
  Reference.t list ->
  update:(unit -> 'a) ->
  update_result:ClassMetadataEnvironment.UpdateResult.t ->
  'a * SharedMemoryKeys.ReferenceDependencyKey.KeySet.t

val transaction : t -> ?only_global_keys:bool -> f:(unit -> 'a) -> unit -> 'a

val shared_memory_handler : ClassMetadataEnvironment.ReadOnly.t -> t

val shared_memory_hash_to_key_map : qualifiers:Ast.Reference.t list -> unit -> string String.Map.t

val serialize_decoded : Memory.decodable -> (string * string * string sexp_option) sexp_option

val decoded_equal : Memory.decodable -> Memory.decodable -> bool option

val class_hierarchy_json : t -> Yojson.Safe.t

val class_hierarchy_dot : t -> string
