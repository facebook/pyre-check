(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

type dependency =
  | TypeCheckSource of Reference.t
  | AliasRegister of Reference.t
  | ClassConnect of Type.Primitive.t
  | RegisterClassMetadata of Type.Primitive.t
  | UndecoratedFunction of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey : Memory.DependencyKey.S with type t = dependency

module FromEmptyStubCache : sig
  val clear : unit -> unit
end

module ReadOnly : sig
  type t

  val create
    :  ?get_source:(Reference.t -> Source.t option) ->
    ?get_wildcard_exports:(Reference.t -> Reference.t list option) ->
    ?get_source_path:(Reference.t -> SourcePath.t option) ->
    ?is_module:(Reference.t -> bool) ->
    ?all_explicit_modules:(unit -> Reference.t list) ->
    ?get_module_metadata:(?dependency:dependency -> Reference.t -> Module.t option) ->
    unit ->
    t

  val get_source : t -> Reference.t -> Source.t option

  val get_wildcard_exports : t -> Reference.t -> Reference.t list option

  val get_source_path : t -> Reference.t -> SourcePath.t option

  val is_module : t -> Reference.t -> bool

  val get_relative : t -> Reference.t -> string option

  val get_real_path_relative
    :  configuration:Configuration.Analysis.t ->
    t ->
    Reference.t ->
    string option

  val all_explicit_modules : t -> Reference.t list

  val get_module_metadata : t -> ?dependency:dependency -> Reference.t -> Module.t option

  val from_empty_stub : t -> ?dependency:dependency -> Reference.t -> bool
end

(* Store the environment to saved-state *)
val store : t -> unit

(* Load the environment from saved-state. Taking a `ModuleTracker` parameter just to signal that
   loading an `AstEnvironment` must be done after loading a `ModuleTracker` *)
val load : ModuleTracker.t -> t

val shared_memory_hash_to_key_map : Reference.t list -> string Core.String.Map.t

val serialize_decoded : Memory.decodable -> (string * string * string option) option

val decoded_equal : Memory.decodable -> Memory.decodable -> bool option

val create : ModuleTracker.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> DependencyKey.KeySet.t

  val reparsed : t -> Reference.t list

  val syntax_errors : t -> SourcePath.t list

  val system_errors : t -> SourcePath.t list

  val create_for_testing : unit -> t
end

type trigger =
  | Update of ModuleTracker.IncrementalUpdate.t list
  | ColdStart

val update
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  t ->
  trigger ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
