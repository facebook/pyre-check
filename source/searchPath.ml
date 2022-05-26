(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  relative_path: PyrePath.RelativePath.t;
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
    let open Option in
    let found =
      match search_path with
      | Submodule _ -> PyrePath.equal (to_path search_path) raw_path
      | _ -> PyrePath.directory_contains ~directory:(to_path search_path) raw_path
    in
    if found then
      let root = get_root search_path in
      PyrePath.get_relative_to_root ~root ~path:raw_path
      >>| (fun relative -> PyrePath.create_relative ~root ~relative)
      >>= function
      | PyrePath.Absolute _ -> None
      | PyrePath.Relative relative -> Some relative
    else
      None
  in
  let search_under_root index search_path =
    under_root search_path
    |> Option.map ~f:(fun relative_path -> { relative_path; priority = index })
  in
  List.find_mapi search_paths ~f:search_under_root
