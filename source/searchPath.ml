(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The SearchPath.t datatype represents somewhere Pyre will look for
 * source code. There are three varieties:
 * - A Root, which corresponds to a top-level package root like site-packages
 * - A Subdirectory, which is a directory of source files where the module
 *   paths should be interpreted as relative to some possibly-higher root.
 *   This is useful, for example, when only including a couple of packages
 *   as search roots from a much bigger site-packages directory; we need a way
 *   to only include relative packages, but set
 * - A Submodule, which is like a Subdirectory but for a single source file.
 *   This is needed to handle single-file packages in the same scenarios where
 *   we might otherwise have used Subdirectory.
 *)

open Core

type t =
  | Root of PyrePath.t
  | Subdirectory of {
      root: PyrePath.t;
      subdirectory: string;
    }
  | Submodule of {
      root: PyrePath.t;
      submodule: string;
    }
[@@deriving sexp, compare, hash]

type search_result = {
  relative_path: string;
  priority: int;
}

let equal = [%compare.equal: t]

let get_root path =
  match path with
  | Root root -> root
  | Subdirectory { root; _ }
  | Submodule { root; _ } ->
      root


let to_path path =
  match path with
  | Root root -> root
  | Subdirectory { root; subdirectory = relative }
  | Submodule { root; submodule = relative } ->
      PyrePath.create_relative ~root ~relative


let pp formatter = function
  | Root root -> PyrePath.pp formatter root
  | Subdirectory { root; subdirectory = relative }
  | Submodule { root; submodule = relative } ->
      Format.fprintf formatter "%a$%s" PyrePath.pp root relative


let show path = Format.asprintf "%a" pp path

let create serialized =
  match String.split serialized ~on:'$' with
  | [root] -> Root (PyrePath.create_absolute root)
  | [root; path] -> (
      match String.split path ~on:'.' with
      | [subdirectory] -> Subdirectory { root = PyrePath.create_absolute root; subdirectory }
      | _ -> Submodule { root = PyrePath.create_absolute root; submodule = path })
  | _ -> failwith (Format.asprintf "Unable to create search path from %s" serialized)


let normalize = function
  | Root root ->
      Root (PyrePath.create_absolute ~follow_symbolic_links:true (PyrePath.absolute root))
  | Subdirectory { root; subdirectory } ->
      Subdirectory
        {
          root = PyrePath.create_absolute ~follow_symbolic_links:true (PyrePath.absolute root);
          subdirectory;
        }
  | Submodule { root; submodule } ->
      Submodule
        {
          root = PyrePath.create_absolute ~follow_symbolic_links:true (PyrePath.absolute root);
          submodule;
        }


let create_normalized serialized = create serialized |> normalize

let search_for_path ~search_paths analysis_path =
  let raw_path = ArtifactPath.raw analysis_path in
  let under_root search_path =
    let found =
      match search_path with
      | Submodule _ -> PyrePath.equal (to_path search_path) raw_path
      | _ -> PyrePath.directory_contains ~directory:(to_path search_path) raw_path
    in
    if found then
      let root = get_root search_path in
      PyrePath.get_relative_to_root ~root ~path:raw_path
    else
      None
  in
  let search_under_root index search_path =
    under_root search_path
    |> Option.map ~f:(fun relative_path -> { relative_path; priority = index })
  in
  List.find_mapi search_paths ~f:search_under_root
