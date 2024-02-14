(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

(* Add the files that contain any of the given callables. *)
val add_files_from_callables
  :  resolution:Analysis.GlobalResolution.t ->
  resolve_module_path:(Ast.Reference.t -> Interprocedural.RepositoryPath.t option) ->
  callables:Interprocedural.Target.t list ->
  t ->
  t

val empty : t

val write_to_file : path:PyrePath.t -> t -> unit
