(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Data_structures
open Pyre

module File = struct
  module T = struct
    type t = { name: string } [@@deriving compare, sexp, hash, show]
  end

  include T
  module Set = SerializableSet.Make (T)

  let from_callable ~resolve_module_path ~resolution callable =
    Interprocedural.Target.get_module_and_definition callable ~resolution
    >>| fst
    >>= resolve_module_path
    >>= function
    | { Interprocedural.RepositoryPath.filename = Some filename; _ } ->
        (* Omitting absolute paths, since they are less useful than relative paths, which are
           machine independent. *)
        Some { name = filename }
    | _ -> None
end

type t = { (* Any file that contains a callable that is analyzed. *)
           files: File.Set.t }

let empty = { files = File.Set.empty }

let add_file_from_callable ~resolution ~resolve_module_path ~callable ({ files } as coverage) =
  match File.from_callable ~resolve_module_path ~resolution callable with
  | Some file -> { files = File.Set.add file files }
  | None -> coverage


(* Add the files that contain any of the given callables. *)
let add_files_from_callables ~resolution ~resolve_module_path ~callables coverage =
  let callables = Interprocedural.Target.Set.of_list callables in
  Interprocedural.Target.Set.fold
    (fun callable so_far ->
      add_file_from_callable ~resolution ~resolve_module_path ~callable so_far)
    callables
    coverage


let write_to_file ~path { files; _ } =
  let out_channel = Out_channel.create (PyrePath.absolute path) in
  File.Set.iter (fun { File.name } -> Printf.fprintf out_channel "%s\n" name) files
