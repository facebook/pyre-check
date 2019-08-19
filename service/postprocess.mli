(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis

val register_ignores
  :  configuration:Configuration.Analysis.t ->
  Scheduler.t ->
  Ast.Source.t list ->
  unit

val ignore
  :  configuration:Configuration.Analysis.t ->
  Scheduler.t ->
  Ast.Source.t list ->
  Error.t list ->
  Error.t list

val shared_memory_hash_to_key_map : Analysis.ModuleTracker.t -> string String.Map.t

val serialize_decoded : Memory.decodable -> (string * string * string sexp_option) sexp_option

val decoded_equal : Memory.decodable -> Memory.decodable -> bool option
