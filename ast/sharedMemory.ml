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


module AccessKey = struct
  type t = Expression.Access.t
  let to_string = Expression.Access.show
  let compare = Expression.Access.compare
end


module SymlinksToPaths = struct
  module SymlinkTarget = struct
    type t = string
    let to_string = ident
    let compare = String.compare
  end
  module SymlinkSource = struct
    type t = PyrePath.t
    let prefix = Prefix.make ()
    let description = "SymlinkSource"
  end
  module SymlinksToPaths = SharedMemory.NoCache (SymlinkTarget) (SymlinkSource)

  let get target =
    SymlinksToPaths.get target

  let add target =
    SymlinksToPaths.add target

  let remove ~targets =
    List.filter ~f:SymlinksToPaths.mem targets
    |> SymlinksToPaths.KeySet.of_list
    |> SymlinksToPaths.remove_batch
end


module Sources = struct
  module SourceValue = struct
    type t = Source.t
    let prefix = Prefix.make ()
    let description = "AST"
  end
  module Sources = SharedMemory.NoCache (HandleKey) (SourceValue)

  let get handle =
    Sources.get handle

  let add handle =
    Sources.add handle

  let remove ~handles =
    List.filter ~f:Sources.mem handles
    |> Sources.KeySet.of_list
    |> Sources.remove_batch
end


module Handles = struct
  module PathValue = struct
    type t = string
    let prefix = Prefix.make ()
    let description = "Path"
  end

  module Paths = SharedMemory.WithCache (IntKey) (PathValue)

  let get ~hash =
    Paths.get hash

  let add_handle_hash ~handle =
    Paths.write_through (String.hash handle) handle
end


module HandleKeys = struct
  module HandleKeysValue = struct
    type t = File.Handle.t list
    let prefix = Prefix.make ()
    let description = "All handles"
  end

  module HandleKeys = SharedMemory.WithCache (IntKey) (HandleKeysValue)

  let get () =
    HandleKeys.get 0
    |> Option.value ~default:[]

  let clear () =
    HandleKeys.remove_batch (HandleKeys.KeySet.singleton 0)

  let add ~handles:new_keys =
    let handles = get () in
    clear ();
    HandleKeys.add 0 (new_keys @ handles)
end


module Modules = struct
  module ModuleValue = struct
    type t = Module.t
    let prefix = Prefix.make ()
    let description = "Module"
  end

  module Modules = SharedMemory.WithCache (AccessKey) (ModuleValue)

  let add ~qualifier ~ast_module =
    Modules.write_through qualifier ast_module

  let remove ~qualifiers =
    let accesses = List.filter ~f:Modules.mem qualifiers in
    Modules.remove_batch (Modules.KeySet.of_list accesses)

  let get ~qualifier =
    Modules.get qualifier

  let get_exports ~qualifier =
    get ~qualifier
    >>| Module.wildcard_exports

  let exists ~qualifier =
    Modules.mem qualifier
end
