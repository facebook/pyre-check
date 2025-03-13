(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Kind : sig
  type t =
    | Model
    | Issue
    | CallGraph
    | HigherOrderCallGraph
    | Module
    | Function

  val show : t -> string
end

module Line : sig
  type t = {
    kind: Kind.t;
    data: Yojson.Safe.t;
  }

  val to_json : t -> Yojson.Safe.t
end

val write_file
  :  path:PyrePath.t ->
  configuration:Yojson.Safe.t ->
  to_json_lines:('a -> Line.t list) ->
  'a list ->
  unit

val remove_sharded_files : directory:PyrePath.t -> filename_prefix:string -> unit

val write_sharded_files
  :  scheduler:Scheduler.t ->
  directory:PyrePath.t ->
  filename_prefix:string ->
  configuration:Yojson.Safe.t ->
  to_json_lines:('a -> Line.t list) ->
  'a list ->
  unit
