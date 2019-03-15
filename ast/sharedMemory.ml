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

  type out = File.Handle.t
  let from_string = File.Handle.create
end

module HandleValue = struct
  type t = File.Handle.t
  let prefix = Prefix.make ()
  let description = "File handle"
end

module IntKey = struct
  type t = int
  let to_string = Int.to_string
  let compare = Int.compare

  type out = int
  let from_string = Int.of_string
end


module AccessKey = struct
  type t = Expression.Access.t
  let to_string = Expression.Access.show
  let compare = Expression.Access.compare

  type out = t
  let from_string = Expression.Access.create
end


module SymlinksToPaths = struct
  module SymlinkTarget = struct
    type t = string
    let to_string = ident
    let compare = String.compare

    type out = string
    let from_string = ident
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

  let hash_of_key =
    SymlinksToPaths.hash_of_key

  let serialize_key key =
    SymlinksToPaths.serialize_key key
    |> Base64.encode_exn

  let compute_hashes_to_keys ~links =
    let add map link =
      Map.add_exn
        map
        ~key:(hash_of_key link)
        ~data:(serialize_key link)
    in
    List.fold links ~init:String.Map.empty ~f:add
end


module Sources = struct
  module SourceValue = struct
    type t = Source.t
    let prefix = Prefix.make ()
    let description = "AST"

    type out = string
    let from_string = ident
  end

  module Sources = SharedMemory.NoCache (HandleKey) (SourceValue)
  module QualifiersToHandles = SharedMemory.NoCache (AccessKey) (HandleValue)

  let get handle =
    Sources.get handle

  let get_for_qualifier qualifier =
    QualifiersToHandles.get qualifier
    >>= Sources.get

  let add handle ({ Source.qualifier; _ } as source) =
    Sources.add handle source;
    QualifiersToHandles.add qualifier handle

  let remove ~handles =
    List.filter ~f:Sources.mem handles
    |> Sources.KeySet.of_list
    |> Sources.remove_batch

  let hash_of_handle =
    Sources.hash_of_key

  let serialize_handle handle =
    Sources.serialize_key handle
    |> Base64.encode_exn

  let hash_of_qualifier =
    QualifiersToHandles.hash_of_key

  let serialize_qualifier qualifier =
    QualifiersToHandles.serialize_key qualifier
    |> Base64.encode_exn

  let compute_hashes_to_keys ~handles =
    let add map handle =
      let map =
        Map.add_exn
          map
          ~key:(hash_of_handle handle)
          ~data:(serialize_handle handle)
      in
      (* This is an approximation that assumes Source.qualifier = Source.qualifier handle for all
         cases. *)
      let qualifier = Source.qualifier ~handle in
      Map.add_exn
        map
        ~key:(hash_of_qualifier qualifier)
        ~data:(serialize_qualifier qualifier)
    in
    List.fold handles ~init:String.Map.empty ~f:add
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


  let hash_of_key =
    Paths.hash_of_key

  let serialize_key key =
    Paths.serialize_key key
    |> Base64.encode_exn

  let compute_hashes_to_keys ~handles =
    let add map handle =
      let key = String.hash handle in
      Map.add_exn
        map
        ~key:(hash_of_key key)
        ~data:(serialize_key key)
    in
    List.fold handles ~init:String.Map.empty ~f:add
end


module HandleKeys = struct
  module HandleKeysValue = struct
    type t = File.Handle.Set.Tree.t
    let prefix = Prefix.make ()
    let description = "All handles"
  end

  module HandleKeys = SharedMemory.WithCache (IntKey) (HandleKeysValue)

  let get () =
    HandleKeys.get 0
    |> Option.value ~default:File.Handle.Set.Tree.empty

  let clear () =
    HandleKeys.remove_batch (HandleKeys.KeySet.singleton 0)

  let add ~handles:new_keys =
    let handles = get () in
    clear ();
    let handles = File.Handle.Set.Tree.union handles new_keys in
    HandleKeys.add 0 handles

  let normalize () =
    let handles = get () in
    clear ();
    handles
    |> File.Handle.Set.Tree.to_list
    |> List.sort ~compare:File.Handle.compare
    |> File.Handle.Set.Tree.of_list
    |> HandleKeys.add 0

  let hash_of_key =
    HandleKeys.hash_of_key

  let serialize_key key =
    HandleKeys.serialize_key key
    |> Base64.encode_exn

  let compute_hashes_to_keys () =
    String.Map.singleton (hash_of_key 0) (serialize_key 0)
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

  let hash_of_key = Modules.hash_of_key
  let serialize_key key =
    Modules.serialize_key key
    |> Base64.encode_exn

  let compute_hashes_to_keys ~qualifiers =
    let add map qualifier =
      Map.add_exn
        map
        ~key:(hash_of_key qualifier)
        ~data:(serialize_key qualifier)
    in
    List.fold qualifiers ~init:String.Map.empty ~f:add
end
