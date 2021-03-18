(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = PyrePath

type t =
  | Root of Path.t
  | Subdirectory of {
      root: Path.t;
      subdirectory: string;
    }
[@@deriving sexp, compare, hash]

type search_result = {
  relative_path: Path.RelativePath.t;
  priority: int;
}

let equal = [%compare.equal: t]

let get_root path =
  match path with
  | Root root -> root
  | Subdirectory { root; _ } -> root


let to_path path =
  match path with
  | Root root -> root
  | Subdirectory { root; subdirectory } -> Path.create_relative ~root ~relative:subdirectory


let pp formatter = function
  | Root root -> Path.pp formatter root
  | Subdirectory { root; subdirectory } ->
      Format.fprintf formatter "%a$%s" Path.pp root subdirectory


let show path = Format.asprintf "%a" pp path

let create serialized =
  match String.split serialized ~on:'$' with
  | [root] -> Root (Path.create_absolute root)
  | [root; subdirectory] -> Subdirectory { root = Path.create_absolute root; subdirectory }
  | _ -> failwith (Format.asprintf "Unable to create search path from %s" serialized)


let normalize = function
  | Root root -> Root (Path.create_absolute ~follow_symbolic_links:true (Path.absolute root))
  | Subdirectory { root; subdirectory } ->
      Subdirectory
        {
          root = Path.create_absolute ~follow_symbolic_links:true (Path.absolute root);
          subdirectory;
        }


let create_normalized serialized = create serialized |> normalize

let search_for_path ~search_paths path =
  let under_root search_path =
    let open Option in
    if Path.directory_contains ~directory:(to_path search_path) path then
      let root = get_root search_path in
      Path.get_relative_to_root ~root ~path
      >>| (fun relative -> Path.create_relative ~root ~relative)
      >>= function
      | Path.Absolute _ -> None
      | Path.Relative relative -> Some relative
    else
      None
  in
  let search_under_root index search_path =
    under_root search_path
    |> Option.map ~f:(fun relative_path -> { relative_path; priority = index })
  in
  List.find_mapi search_paths ~f:search_under_root
