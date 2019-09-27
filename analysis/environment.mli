(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type t

val ast_environment : t -> AstEnvironment.ReadOnly.t

val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t

val resolution : t -> unit -> GlobalResolution.t

(* Currently experimental *)
val dependency_tracked_resolution : t -> dependency:Reference.t -> unit -> GlobalResolution.t

val is_module : t -> Reference.t -> bool

val update_and_compute_dependencies
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ClassMetadataEnvironment.UpdateResult.t ->
  SharedMemoryKeys.ReferenceDependencyKey.KeySet.t

val shared_memory_handler : ClassMetadataEnvironment.ReadOnly.t -> t

val hash_to_key_map : t -> string String.Map.t

val serialize_decoded : t -> Memory.decodable -> (string * string * string sexp_option) sexp_option

val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option
