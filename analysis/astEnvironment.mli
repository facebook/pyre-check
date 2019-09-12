(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val create : ModuleTracker.t -> t

val get_source : t -> ?dependency:Reference.t -> Reference.t -> Source.t option

val get_wildcard_exports : t -> ?dependency:Reference.t -> Reference.t -> Reference.t list option

val update
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  ast_environment:t ->
  ModuleTracker.IncrementalUpdate.t list ->
  Reference.t list

val add_source : t -> Source.t -> unit

val remove_sources : t -> Reference.t list -> unit

val get_source_path : t -> Reference.t -> SourcePath.t option

(* Store the environment to saved-state *)
val store : t -> unit

(* Load the environment from saved-state. Taking a `ModuleTracker` parameter just to signal that
   loading an `AstEnvironment` must be done after loading a `ModuleTracker` *)
val load : ModuleTracker.t -> t

val shared_memory_hash_to_key_map : Reference.t list -> string Core.String.Map.t

val serialize_decoded : Memory.decodable -> (string * string * string option) option

val decoded_equal : Memory.decodable -> Memory.decodable -> bool option

type environment_t = t

module ReadOnly : sig
  type t

  val create
    :  ?get_source:(Reference.t -> Source.t option) ->
    ?get_wildcard_exports:(Reference.t -> Reference.t list option) ->
    ?get_source_path:(Reference.t -> SourcePath.t option) ->
    ?is_module:(Reference.t -> bool) ->
    ?all_explicit_modules:(unit -> Reference.t list) ->
    ?get_module_metadata:(?dependency:Reference.t -> Reference.t -> Module.t option) ->
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

  val get_module_metadata : t -> ?dependency:Reference.t -> Reference.t -> Module.t option
end

val read_only : t -> ReadOnly.t

type parse_result =
  | Success of Source.t
  | SyntaxError of string
  | SystemError of string

val parse_source : configuration:Configuration.Analysis.t -> SourcePath.t -> parse_result

type parse_sources_result = {
  parsed: Reference.t list;
  syntax_error: SourcePath.t list;
  system_error: SourcePath.t list;
}

val parse_sources
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  preprocessing_state:ProjectSpecificPreprocessing.state option ->
  ast_environment:t ->
  SourcePath.t list ->
  parse_sources_result

val parse_all
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ModuleTracker.t ->
  Source.t list * t
