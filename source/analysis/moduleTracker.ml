(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

module PathLookup = struct
  type t =
    | Found of Ast.SourcePath.t
    | ShadowedBy of Ast.SourcePath.t
    | NotFound
  [@@deriving show, sexp, compare]
end

module IncrementalUpdate = struct
  type t =
    | NewExplicit of Ast.SourcePath.t
    | NewImplicit of Ast.Reference.t
    | Delete of Reference.t
  [@@deriving show, sexp, compare]

  let equal = [%compare.equal: t]
end

module Layouts = struct
  type t = {
    module_to_files: SourcePath.t list Reference.Table.t;
    submodule_refcounts: int Reference.Table.t;
  }

  let insert_source_path ~configuration ~inserted existing_files =
    let rec insert sofar = function
      | [] -> List.rev_append sofar [inserted]
      | current_file :: rest as existing -> (
          match SourcePath.same_module_compare ~configuration inserted current_file with
          | 0 ->
              (* We have the following precondition for files that are in the same module: *)
              (* `same_module_compare a b = 0` implies `equal a b` *)
              assert (SourcePath.equal inserted current_file);

              (* Duplicate entry detected. Do nothing *)
              existing_files
          | x when x > 0 -> List.rev_append sofar (inserted :: existing)
          | _ -> insert (current_file :: sofar) rest)
    in
    insert [] existing_files


  let remove_source_path ~configuration ~removed existing_files =
    let rec remove sofar = function
      | [] -> existing_files
      | current_file :: rest -> (
          match SourcePath.same_module_compare ~configuration removed current_file with
          | 0 ->
              let () =
                (* For removed files, we only check for equality on relative path & priority. *)
                (* There's a corner case (where symlink is involved) that may cause `removed` to
                   have a different `is_external` flag. *)
                let partially_equal
                    { SourcePath.relative = left_relative; priority = left_priority; _ }
                    { SourcePath.relative = right_relative; priority = right_priority; _ }
                  =
                  String.equal left_relative right_relative
                  && Int.equal left_priority right_priority
                in
                assert (partially_equal removed current_file)
              in
              List.rev_append sofar rest
          | x when x > 0 -> existing_files
          | _ -> remove (current_file :: sofar) rest)
    in
    remove [] existing_files


  let find_files
      ({ Configuration.Analysis.source_paths; search_paths; excludes; _ } as configuration)
    =
    let visited_directories = String.Hash_set.create () in
    let visited_files = String.Hash_set.create () in
    let valid_suffixes =
      ".py" :: ".pyi" :: Configuration.Analysis.extension_suffixes configuration
    in
    let mark_visited set path =
      match Hash_set.strict_add set path with
      | Result.Ok () -> false
      | _ -> true
    in
    let search_roots =
      List.append
        (List.map ~f:SearchPath.to_path source_paths)
        (List.map ~f:SearchPath.to_path search_paths)
    in
    List.map search_roots ~f:(fun root ->
        let root_path = PyrePath.absolute root in
        let directory_filter path =
          (* Don't bother with hidden directories (except in the case where the root itself is
             hidden) as they are non-importable in Python by default *)
          ((not (String.is_prefix (Filename.basename path) ~prefix:"."))
          || String.equal path root_path)
          (* Do not scan excluding directories to speed up the traversal *)
          && (not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0)))
          && not (mark_visited visited_directories path)
        in
        let file_filter path =
          let extension =
            Filename.split_extension path
            |> snd
            >>| (fun extension -> "." ^ extension)
            |> Option.value ~default:""
          in
          (* Don't bother with hidden files as they are non-importable in Python by default *)
          (not (String.is_prefix (Filename.basename path) ~prefix:"."))
          (* Only consider files with valid suffix *)
          && List.exists ~f:(String.equal extension) valid_suffixes
          && not (mark_visited visited_files path)
        in
        PyrePath.list ~file_filter ~directory_filter ~root ())
    |> List.concat


  let create_module_to_files configuration =
    let module_to_files = Reference.Table.create () in
    let process_file path =
      match SourcePath.create ~configuration path with
      | None -> ()
      | Some ({ SourcePath.qualifier; _ } as source_path) ->
          let update_table = function
            | None -> [source_path]
            | Some source_paths ->
                insert_source_path ~configuration ~inserted:source_path source_paths
          in
          Hashtbl.update module_to_files qualifier ~f:update_table
    in
    let files = find_files configuration in
    List.iter files ~f:process_file;
    module_to_files


  let create_submodule_refcounts module_to_files =
    let submodule_refcounts = Reference.Table.create () in
    let process_module qualifier =
      let rec process_submodule = function
        | None -> ()
        | Some qualifier when Reference.is_empty qualifier -> ()
        | Some qualifier ->
            Reference.Table.update submodule_refcounts qualifier ~f:(function
                | None -> 1
                | Some count -> count + 1);
            process_submodule (Reference.prefix qualifier)
      in
      process_submodule (Some qualifier)
    in
    Hashtbl.keys module_to_files |> List.iter ~f:process_module;
    submodule_refcounts


  let create configuration =
    let timer = Timer.start () in
    Log.info "Building module tracker...";
    let module_to_files = create_module_to_files configuration in
    let submodule_refcounts = create_submodule_refcounts module_to_files in
    Statistics.performance ~name:"module tracker built" ~timer ~phase_name:"Module tracking" ();
    { module_to_files; submodule_refcounts }


  module FileSystemEvent = struct
    type t =
      | Update of PyrePath.t
      | Remove of PyrePath.t

    let create path = if PyrePath.file_exists path then Update path else Remove path
  end

  module IncrementalExplicitUpdate = struct
    type t =
      | New of SourcePath.t
      | Changed of SourcePath.t
      | Delete of Reference.t
    [@@deriving sexp, compare]
  end

  module IncrementalImplicitUpdate = struct
    type t =
      | New of Reference.t
      | Delete of Reference.t
    [@@deriving sexp, compare]
  end

  let update_explicit_modules ~configuration ~paths module_to_files =
    (* Process a single filesystem event *)
    let process_filesystem_event ~configuration = function
      | FileSystemEvent.Update path -> (
          match SourcePath.create ~configuration path with
          | None ->
              Log.warning "`%a` not found in search path." PyrePath.pp path;
              None
          | Some ({ SourcePath.qualifier; _ } as source_path) -> (
              match Hashtbl.find module_to_files qualifier with
              | None ->
                  (* New file for a new module *)
                  Hashtbl.set module_to_files ~key:qualifier ~data:[source_path];
                  Some (IncrementalExplicitUpdate.New source_path)
              | Some source_paths ->
                  let new_source_paths =
                    insert_source_path ~configuration ~inserted:source_path source_paths
                  in
                  let new_source_path = List.hd_exn new_source_paths in
                  Hashtbl.set module_to_files ~key:qualifier ~data:new_source_paths;
                  if SourcePath.equal new_source_path source_path then
                    (* Updating a shadowing file means the module gets changed *)
                    Some (IncrementalExplicitUpdate.Changed source_path)
                  else (* Updating a shadowed file should not trigger any reanalysis *)
                    None))
      | FileSystemEvent.Remove path -> (
          match SourcePath.create ~configuration path with
          | None ->
              Log.warning "`%a` not found in search path." PyrePath.pp path;
              None
          | Some ({ SourcePath.qualifier; _ } as source_path) -> (
              Hashtbl.find module_to_files qualifier
              >>= fun source_paths ->
              match source_paths with
              | [] ->
                  (* This should never happen but handle it just in case *)
                  Hashtbl.remove module_to_files qualifier;
                  None
              | old_source_path :: _ -> (
                  match remove_source_path ~configuration ~removed:source_path source_paths with
                  | [] ->
                      (* Last remaining file for the module gets removed. *)
                      Hashtbl.remove module_to_files qualifier;
                      Some (IncrementalExplicitUpdate.Delete qualifier)
                  | new_source_path :: _ as new_source_paths ->
                      Hashtbl.set module_to_files ~key:qualifier ~data:new_source_paths;
                      if SourcePath.equal old_source_path new_source_path then
                        (* Removing a shadowed file should not trigger any reanalysis *)
                        None
                      else (* Removing source_path un-shadows another source file. *)
                        Some (IncrementalExplicitUpdate.Changed new_source_path))))
    in
    (* Make sure we have only one update per module *)
    let merge_updates updates =
      let table = Reference.Table.create () in
      let process_update update =
        match update with
        | IncrementalExplicitUpdate.New ({ SourcePath.qualifier; _ } as source_path) ->
            let update = function
              | None -> update
              | Some (IncrementalExplicitUpdate.Delete _) ->
                  IncrementalExplicitUpdate.Changed source_path
              | Some (IncrementalExplicitUpdate.New _) ->
                  let message =
                    Format.asprintf "Illegal state: double new module %a" Reference.pp qualifier
                  in
                  failwith message
              | Some (IncrementalExplicitUpdate.Changed _) ->
                  let message =
                    Format.asprintf
                      "Illegal state: new after changed module %a"
                      Reference.pp
                      qualifier
                  in
                  failwith message
            in
            Hashtbl.update table qualifier ~f:update
        | IncrementalExplicitUpdate.Changed ({ SourcePath.qualifier; _ } as source_path) ->
            let update = function
              | None
              | Some (IncrementalExplicitUpdate.Changed _) ->
                  update
              | Some (IncrementalExplicitUpdate.New _) -> IncrementalExplicitUpdate.New source_path
              | Some (IncrementalExplicitUpdate.Delete _) ->
                  let message =
                    Format.asprintf
                      "Illegal state: changing a deleted module %a"
                      Reference.pp
                      qualifier
                  in
                  failwith message
            in
            Hashtbl.update table qualifier ~f:update
        | IncrementalExplicitUpdate.Delete qualifier ->
            let update = function
              | None
              | Some (IncrementalExplicitUpdate.Changed _) ->
                  Some update
              | Some (IncrementalExplicitUpdate.New _) ->
                  let message =
                    Format.asprintf
                      "Illegal state: delete after new module %a"
                      Reference.pp
                      qualifier
                  in
                  failwith message
              | Some (IncrementalExplicitUpdate.Delete _) ->
                  let message =
                    Format.asprintf "Illegal state: double delete module %a" Reference.pp qualifier
                  in
                  failwith message
            in
            Hashtbl.change table qualifier ~f:update
      in
      List.iter updates ~f:process_update;
      Hashtbl.data table
    in
    (* Since `process_filesystem_event` is not idempotent, we don't want duplicated filesystem
       events *)
    List.dedup_and_sort ~compare:PyrePath.compare paths
    |> List.map ~f:FileSystemEvent.create
    |> List.filter_map ~f:(process_filesystem_event ~configuration)
    |> merge_updates


  let update_submodules ~events submodule_refcounts =
    let aggregate_updates events =
      let aggregated_refcounts = Reference.Table.create () in
      let process_event event =
        let rec do_update ~f = function
          | None -> ()
          | Some qualifier when Reference.is_empty qualifier -> ()
          | Some qualifier ->
              Hashtbl.update aggregated_refcounts qualifier ~f:(function
                  | None -> f 0
                  | Some count -> f count);
              do_update (Reference.prefix qualifier) ~f
        in
        match event with
        | IncrementalExplicitUpdate.Changed _ -> ()
        | IncrementalExplicitUpdate.New { SourcePath.qualifier; _ } ->
            do_update (Some qualifier) ~f:(fun count -> count + 1)
        | IncrementalExplicitUpdate.Delete qualifier ->
            do_update (Some qualifier) ~f:(fun count -> count - 1)
      in
      List.iter events ~f:process_event;
      aggregated_refcounts
    in
    let commit_updates update_table =
      let commit_update ~key ~data sofar =
        match data with
        | 0 -> List.rev sofar
        | delta -> (
            let original_refcount =
              Hashtbl.find submodule_refcounts key |> Option.value ~default:0
            in
            let new_refcount = original_refcount + delta in
            match new_refcount with
            | 0 ->
                Hashtbl.remove submodule_refcounts key;
                IncrementalImplicitUpdate.Delete key :: sofar
            | count when count < 0 ->
                let message =
                  Format.asprintf
                    "Illegal state: negative refcount (%d) for module %a"
                    count
                    Reference.pp
                    key
                in
                failwith message
            | _ ->
                Hashtbl.set submodule_refcounts ~key ~data:new_refcount;
                if Int.equal original_refcount 0 then
                  IncrementalImplicitUpdate.New key :: sofar
                else
                  sofar)
      in
      Hashtbl.fold update_table ~init:[] ~f:commit_update
    in
    aggregate_updates events |> commit_updates


  let update ~configuration ~paths { module_to_files; submodule_refcounts } =
    let explicit_updates = update_explicit_modules module_to_files ~configuration ~paths in
    let implicit_updates = update_submodules submodule_refcounts ~events:explicit_updates in
    (* Explicit updates should shadow implicit updates *)
    let updates =
      let new_qualifiers = Reference.Hash_set.create () in
      let deleted_qualifiers = Reference.Hash_set.create () in
      let explicits =
        let process_explicit_update = function
          | IncrementalExplicitUpdate.New ({ SourcePath.qualifier; _ } as source_path)
          | IncrementalExplicitUpdate.Changed ({ SourcePath.qualifier; _ } as source_path) ->
              Hash_set.add new_qualifiers qualifier;
              IncrementalUpdate.NewExplicit source_path
          | IncrementalExplicitUpdate.Delete qualifier ->
              Hash_set.add deleted_qualifiers qualifier;
              IncrementalUpdate.Delete qualifier
        in
        List.map explicit_updates ~f:process_explicit_update
      in
      let implicits =
        let process_implicit_update = function
          | IncrementalImplicitUpdate.New qualifier ->
              if Hash_set.mem new_qualifiers qualifier then
                None
              else
                Some (IncrementalUpdate.NewImplicit qualifier)
          | IncrementalImplicitUpdate.Delete qualifier ->
              if Hash_set.mem deleted_qualifiers qualifier then
                None
              else
                Some (IncrementalUpdate.Delete qualifier)
        in
        List.filter_map implicit_updates ~f:process_implicit_update
      in
      List.append explicits implicits
    in
    Log.log
      ~section:`Server
      "Explicit Module Update: %a"
      Sexp.pp
      [%message (explicit_updates : IncrementalExplicitUpdate.t list)];
    Log.log
      ~section:`Server
      "Implicit Module Update: %a"
      Sexp.pp
      [%message (implicit_updates : IncrementalImplicitUpdate.t list)];
    updates
end

type t = {
  layouts: Layouts.t;
  configuration: Configuration.Analysis.t;
}

let create configuration =
  let layouts = Layouts.create configuration in
  { layouts; configuration }


let all_source_paths { layouts = { module_to_files; _ }; _ } =
  Hashtbl.data module_to_files |> List.concat


let source_paths { layouts = { module_to_files; _ }; _ } =
  Hashtbl.data module_to_files |> List.filter_map ~f:List.hd


let configuration { configuration; _ } = configuration

let update ~paths { layouts; configuration } =
  let timer = Timer.start () in
  let result = Layouts.update ~configuration ~paths layouts in
  Statistics.performance ~name:"module tracker updated" ~timer ~phase_name:"Module tracking" ();
  result


module Serializer = struct
  module Layouts = Memory.Serializer (struct
    type nonrec t = Layouts.t

    module Serialized = struct
      type t = (Reference.t * SourcePath.t list) list * (Reference.t * int) list

      let prefix = Prefix.make ()

      let description = "Module tracker"
    end

    let serialize { Layouts.module_to_files; submodule_refcounts } =
      Hashtbl.to_alist module_to_files, Hashtbl.to_alist submodule_refcounts


    let deserialize (module_data, submodule_data) =
      {
        Layouts.module_to_files = Reference.Table.of_alist_exn module_data;
        submodule_refcounts = Reference.Table.of_alist_exn submodule_data;
      }
  end)

  let store_layouts { layouts; _ } = Layouts.store layouts

  let from_stored_layouts ~configuration () =
    let layouts = Layouts.load () in
    { layouts; configuration }
end

module ReadOnly = struct
  type t = {
    lookup_source_path: Reference.t -> SourcePath.t option;
    is_module_tracked: Reference.t -> bool;
    source_paths: unit -> SourcePath.t list;
    configuration: unit -> Configuration.Analysis.t;
  }

  let configuration { configuration; _ } = configuration ()

  let lookup_source_path { lookup_source_path; _ } = lookup_source_path

  let source_paths { source_paths; _ } = source_paths ()

  let is_module_tracked { is_module_tracked; _ } = is_module_tracked

  let lookup_path tracker path =
    let configuration = configuration tracker in
    match SourcePath.create ~configuration path with
    | None -> PathLookup.NotFound
    | Some { SourcePath.relative; priority; qualifier; _ } -> (
        match lookup_source_path tracker qualifier with
        | None -> PathLookup.NotFound
        | Some
            ({ SourcePath.relative = tracked_relative; priority = tracked_priority; _ } as
            source_path) ->
            if String.equal relative tracked_relative && Int.equal priority tracked_priority then
              PathLookup.Found source_path
            else
              PathLookup.ShadowedBy source_path)


  let tracked_explicit_modules tracker = source_paths tracker |> List.map ~f:SourcePath.qualifier

  let get_raw_code tracker source_path =
    let path = SourcePath.full_path ~configuration:(configuration tracker) source_path in
    try Ok (File.content_exn (File.create path)) with
    | Sys_error error ->
        Error (Format.asprintf "Cannot open file `%a` due to: %s" PyrePath.pp path error)
end

let read_only ({ layouts = { module_to_files; submodule_refcounts }; configuration } as tracker) =
  let lookup_source_path module_name =
    match Hashtbl.find module_to_files module_name with
    | Some (source_path :: _) -> Some source_path
    | _ -> None
  in
  let is_module_tracked qualifier =
    Hashtbl.mem module_to_files qualifier || Hashtbl.mem submodule_refcounts qualifier
  in
  {
    ReadOnly.lookup_source_path;
    is_module_tracked;
    source_paths = (fun () -> source_paths tracker);
    configuration = (fun () -> configuration);
  }
