(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Path = PyrePath

type t =
  | Root of Path.t
  | Subdirectory of { root: Path.t; subdirectory: string }
[@@deriving sexp, compare, hash]

let equal = [%compare.equal: t]

let get_root path =
  match path with
  | Root root -> root
  | Subdirectory { root; _ } -> root


let to_path path =
  match path with
  | Root root -> root
  | Subdirectory { root; subdirectory } -> Path.create_relative ~root ~relative:subdirectory


let pp formatter path = Path.pp formatter (to_path path)

let show path = Format.asprintf "%a" pp path

let create serialized =
  match String.split serialized ~on:'$' with
  | [root] -> Root (Path.create_absolute root)
  | [root; subdirectory] -> Subdirectory { root = Path.create_absolute root; subdirectory }
  | _ -> failwith (Format.asprintf "Unable to create search path from %s" serialized)


let search_for_path ~search_path ~path =
  let under_root ~path root =
    if Path.directory_contains ~directory:(to_path root) path then
      let root = get_root root in
      Path.get_relative_to_root ~root ~path
      |> Option.map ~f:(fun relative -> Path.create_relative ~root ~relative)
    else
      None
  in
  search_path |> List.find_map ~f:(under_root ~path)
