(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = {
  lookup_source: string -> string option;
  lookup_dependency: string -> string option;
  all_sources: unit -> string list;
  all_dependencies: unit -> string list;
}

let create_for_testing
    ?(lookup_source = fun _ -> None)
    ?(lookup_dependency = fun _ -> None)
    ?(all_sources = fun () -> [])
    ?(all_dependencies = fun () -> [])
    ()
  =
  { lookup_source; lookup_dependency; all_sources; all_dependencies }


let create_from_alists ~source_alists ~dependency_alists ~typeshed_alists () =
  let create_map_from_alist sofar alist =
    let f sofar (artifact_path, source_path) =
      (* Mapping that come later takes priority *)
      Map.set sofar ~key:artifact_path ~data:source_path
    in
    List.fold alist ~init:sofar ~f
  in
  let empty_map = Map.empty (module String) in
  let source_map = List.fold source_alists ~init:empty_map ~f:create_map_from_alist in
  let typeshed_map = List.fold typeshed_alists ~init:empty_map ~f:create_map_from_alist in
  (* Mappings in dependency manifests take priorities over the ones in typeshed on conflicts *)
  let dependency_map = List.fold dependency_alists ~init:typeshed_map ~f:create_map_from_alist in
  {
    lookup_source = Map.find source_map;
    lookup_dependency = Map.find dependency_map;
    all_sources = (fun () -> Map.keys source_map);
    all_dependencies = (fun () -> Map.keys dependency_map);
  }


let create_from_manifests ~source_manifests ~dependency_manifests ~typeshed_manifests () =
  create_from_alists
    ~source_alists:(List.map source_manifests ~f:Manifest.to_alist)
    ~dependency_alists:(List.map dependency_manifests ~f:Manifest.to_alist)
    ~typeshed_alists:(List.map typeshed_manifests ~f:Manifest.to_alist)
    ()
