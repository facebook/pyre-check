(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Expression = AstExpression
module Module = AstModule
module Source = AstSource

module SharedMemory = Hack_parallel.Std.SharedMem


module HandleKey = struct
  type t = File.Handle.t
  let to_string = File.Handle.show
  let compare = File.Handle.compare
end

module IntKey = struct
  type t = int
  let to_string = Int.to_string
  let compare = Int.compare
end


module SourceValue = struct
  type t = Source.t
  let prefix = Prefix.make ()
  let description = "AST"
end


module PathValue = struct
  type t = string
  let prefix = Prefix.make ()
  let description = "Path"
end

module Sources = SharedMemory.NoCache (HandleKey) (SourceValue)

module Paths = SharedMemory.WithCache (IntKey) (PathValue)


let get_source path =
  Sources.get path


(* The sources must be removed by remove_paths beforehand. *)
let add_source path source =
  Sources.add path source


(* The way hack_parallel works, only the master thread is allowed to remove items from shared
   memory. *)
let remove_paths paths =
  let paths = List.filter ~f:Sources.mem paths in
  Sources.remove_batch (Sources.KeySet.of_list paths)


module AccessKey = struct
  type t = Expression.Access.t
  let to_string = Expression.Access.show
  let compare = Expression.Access.compare
end


module ModuleValue = struct
  type t = Module.t
  let prefix = Prefix.make ()
  let description = "Module"
end


module Modules = SharedMemory.WithCache (AccessKey) (ModuleValue)


let add_module access ast_module =
  Modules.add access ast_module


let remove_modules accesses =
  let accesses = List.filter ~f:Modules.mem accesses in
  Modules.remove_batch (Modules.KeySet.of_list accesses)


let get_module access =
  Modules.get access


let get_module_exports access =
  Modules.get access
  >>| Module.wildcard_exports


let in_modules access =
  Modules.mem access


let get_path ~hash =
  Paths.get hash


let add_path_hash ~path =
  Paths.write_through (String.hash path) path
