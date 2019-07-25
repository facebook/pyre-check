(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core

type t = {
  add_source: Source.t -> unit;
  remove_sources: Reference.t list -> unit;
  get_source: Reference.t -> Source.t option;
  get_source_path: Reference.t -> SourcePath.t option;
}

let create module_tracker =
  {
    add_source = Ast.SharedMemory.Sources.add;
    remove_sources = Ast.SharedMemory.Sources.remove;
    get_source = Ast.SharedMemory.Sources.get;
    get_source_path = ModuleTracker.lookup module_tracker;
  }


let add_source { add_source; _ } = add_source

let remove_sources { remove_sources; _ } = remove_sources

let get_source { get_source; _ } = get_source

let get_source_path { get_source_path; _ } = get_source_path

(* Both `load` and `store` are no-ops here since `Ast.SharedMemory.Sources` is in shared memory,
   and `Memory.load_shared_memory`/`Memory.save_shared_memory` will take care of the
   (de-)serialization for us. *)
let store _ = ()

let load = create

type environment_t = t

module ReadOnly = struct
  type t = environment_t

  let create = Fn.id

  let get_source = get_source

  let get_source_path = get_source_path
end
