(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre

exception IncompatibleState of string

module ServerErrors : sig
  val load : unit -> Analysis.TypeEnvironment.Error.t list Ast.Reference.Table.t
end

(* Exposed for testing. *)
val restore_symbolic_links
  :  changed_paths:Path.t list ->
  source_path:Path.t list ->
  get_old_link_path:(Path.t -> Path.t option) ->
  Path.t list

val load : server_configuration:Configuration.Server.t -> connections:State.connections -> State.t

val save : configuration:Configuration.Analysis.t -> saved_state_path:string -> State.t -> unit
