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

  val store : Analysis.TypeEnvironment.Error.t list Ast.Reference.Table.t -> unit
end

module StoredConfiguration : sig
  val load : unit -> Configuration.Analysis.t

  val store : Configuration.Analysis.t -> unit
end

(* Exposed for testing. *)
val restore_symbolic_links
  :  changed_paths:Path.t list ->
  source_path:Path.t list ->
  get_old_link_path:(Path.t -> Path.t option) ->
  Path.t list
