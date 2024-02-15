(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module Lookup = struct
  type t = {
    get_source: string -> string option;
    get_dependency: string -> string option;
  }

  let create ?(get_source = fun _ -> None) ?(get_dependency = fun _ -> None) () =
    { get_source; get_dependency }


  let create_for_testing ?(sources = []) ?(dependencies = []) () =
    {
      get_source = List.Assoc.find ~equal:String.equal sources;
      get_dependency = List.Assoc.find ~equal:String.equal dependencies;
    }
end

module Listing = struct
  type t = {
    all_sources: unit -> string list;
    all_dependencies: unit -> string list;
  }

  let create ?(all_sources = fun () -> []) ?(all_dependencies = fun () -> []) () =
    { all_sources; all_dependencies }


  let create_for_testing ?(sources = []) ?(dependencies = []) () =
    { all_sources = (fun () -> sources); all_dependencies = (fun () -> dependencies) }
end

type t = {
  lookup: Lookup.t;
  listing: Listing.t;
}

let create ?(lookup = Lookup.create ()) ?(listing = Listing.create ()) () = { lookup; listing }

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
    lookup =
      Lookup.create ~get_source:(Map.find source_map) ~get_dependency:(Map.find dependency_map) ();
    listing =
      Listing.create
        ~all_sources:(fun () -> Map.keys source_map)
        ~all_dependencies:(fun () -> Map.keys dependency_map)
        ();
  }


let create_from_manifests ~source_manifests ~dependency_manifests ~typeshed_manifests () =
  create_from_alists
    ~source_alists:(List.map source_manifests ~f:Manifest.to_alist)
    ~dependency_alists:(List.map dependency_manifests ~f:Manifest.to_alist)
    ~typeshed_alists:(List.map typeshed_manifests ~f:Manifest.to_alist)
    ()
