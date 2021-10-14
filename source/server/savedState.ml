(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast

module StoredConfiguration = Memory.Serializer (struct
  type t = Configuration.Analysis.t

  module Serialized = struct
    type t = Configuration.Analysis.t

    let prefix = Prefix.make ()

    let description = "Configuration"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

module ServerErrors = Memory.Serializer (struct
  type t = Analysis.AnalysisError.t list Reference.Table.t

  module Serialized = struct
    type t = (Reference.t * Analysis.AnalysisError.t list) list

    let prefix = Prefix.make ()

    let description = "All errors"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Hashtbl.to_alist

  let deserialize data = Reference.Table.of_alist_exn data
end)

exception IncompatibleState of string

(* Return symlinks for files for a pyre project that are links to a separate project root. *)
let restore_symbolic_links ~changed_paths ~source_path ~get_old_link_path =
  let new_paths, removed_paths = List.partition_tf ~f:Path.file_exists changed_paths in
  (* We need to get the deleted paths from shared memory, as the version of the server launched from
     the saved state will have references to this files, whereas they won't be present in the new
     project root, meaning that we need to clean up the environment from them. *)
  let removed_paths =
    List.map removed_paths ~f:(fun path -> get_old_link_path path |> Option.value ~default:path)
  in
  (* Any member of new_paths might not have existed when the saved state was being created. *)
  let new_paths =
    let local_root_links =
      let file_filter file =
        Filename.check_suffix file ".py" || Filename.check_suffix file ".pyi"
      in
      List.concat_map source_path ~f:(fun root -> Path.list ~file_filter ~root ())
      |> fun links -> Path.build_symlink_map ~links
    in
    List.filter_map new_paths ~f:(Map.find local_root_links)
  in
  new_paths @ removed_paths
