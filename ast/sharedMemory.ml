(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module SharedMemory = Memory


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

module HandleKeysValue = struct
  type t = File.Handle.t list
  let prefix = Prefix.make ()
  let description = "All handles"
end

module Sources = struct
  include SharedMemory.NoCache (HandleKey) (SourceValue)

  let remove ~handles =
    List.filter ~f:mem handles
    |> KeySet.of_list
    |> remove_batch
end


module Handles = struct
  include SharedMemory.WithCache (IntKey) (PathValue)

  let get ~hash =
    get hash

  let add_handle_hash ~handle =
    write_through (String.hash handle) handle
end


module HandleKeys = struct
  include SharedMemory.WithCache (IntKey) (HandleKeysValue)

  let get () =
    get 0
    |> Option.value ~default:[]

  let clear () =
    remove_batch (KeySet.singleton 0)


  let add ~handles:new_keys =
    let handles = get () in
    clear ();
    add 0 (new_keys @ handles)
end


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
  Modules.write_through access ast_module


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
