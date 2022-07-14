(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

type raw_code = string

type message = string

module PathLookup = struct
  type t =
    | Found of Ast.ModulePath.t
    | ShadowedBy of Ast.ModulePath.t
    | NotFound
  [@@deriving show, sexp, compare]
end

module IncrementalUpdate = struct
  type t =
    | NewExplicit of Ast.ModulePath.t
    | NewImplicit of Ast.Reference.t
    | Delete of Reference.t
  [@@deriving show, sexp, compare]

  let equal = [%compare.equal: t]
end

module ReadOnly = struct
  type t = {
    lookup_module_path: Reference.t -> ModulePath.t option;
    is_module_tracked: Reference.t -> bool;
    get_raw_code: ModulePath.t -> (raw_code, message) Result.t;
    module_paths: unit -> ModulePath.t list;
    controls: unit -> EnvironmentControls.t;
  }

  let controls { controls; _ } = controls ()

  let lookup_module_path { lookup_module_path; _ } = lookup_module_path

  let module_paths { module_paths; _ } = module_paths ()

  let is_module_tracked { is_module_tracked; _ } = is_module_tracked

  let lookup_path tracker path =
    let configuration = controls tracker |> EnvironmentControls.configuration in
    match ModulePath.create ~configuration path with
    | None -> PathLookup.NotFound
    | Some { ModulePath.relative; priority; qualifier; _ } -> (
        match lookup_module_path tracker qualifier with
        | None -> PathLookup.NotFound
        | Some
            ({ ModulePath.relative = tracked_relative; priority = tracked_priority; _ } as
            module_path) ->
            if String.equal relative tracked_relative && Int.equal priority tracked_priority then
              PathLookup.Found module_path
            else
              PathLookup.ShadowedBy module_path)


  let tracked_explicit_modules tracker = module_paths tracker |> List.map ~f:ModulePath.qualifier

  let get_raw_code { get_raw_code; _ } = get_raw_code

  let project_qualifiers tracker =
    module_paths tracker
    |> List.filter ~f:ModulePath.is_in_project
    |> List.map ~f:ModulePath.qualifier
end

module Base = struct
  module Layouts = struct
    type t = {
      module_to_files: ModulePath.t list Reference.Table.t;
      submodule_refcounts: int Reference.Table.t;
    }

    let insert_module_path ~configuration ~inserted existing_files =
      let rec insert sofar = function
        | [] -> List.rev_append sofar [inserted]
        | current_file :: rest as existing -> (
            match ModulePath.same_module_compare ~configuration inserted current_file with
            | 0 ->
                (* We have the following precondition for files that are in the same module: *)
                (* `same_module_compare a b = 0` implies `equal a b` *)
                assert (ModulePath.equal inserted current_file);

                (* Duplicate entry detected. Do nothing *)
                existing_files
            | x when x > 0 -> List.rev_append sofar (inserted :: existing)
            | _ -> insert (current_file :: sofar) rest)
      in
      insert [] existing_files


    let remove_module_path ~configuration ~removed existing_files =
      let rec remove sofar = function
        | [] -> existing_files
        | current_file :: rest -> (
            match ModulePath.same_module_compare ~configuration removed current_file with
            | 0 ->
                let () =
                  (* For removed files, we only check for equality on relative path & priority. *)
                  (* There's a corner case (where symlink is involved) that may cause `removed` to
                     have a different `is_external` flag. *)
                  let partially_equal
                      { ModulePath.relative = left_relative; priority = left_priority; _ }
                      { ModulePath.relative = right_relative; priority = right_priority; _ }
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


    let create_module_to_files ~configuration module_paths =
      let module_to_files = Reference.Table.create () in
      let process_module_path ({ ModulePath.qualifier; _ } as module_path) =
        let update_table = function
          | None -> [module_path]
          | Some module_paths ->
              insert_module_path ~configuration ~inserted:module_path module_paths
        in
        Hashtbl.update module_to_files qualifier ~f:update_table
      in
      List.iter module_paths ~f:process_module_path;
      module_to_files


    let create_submodule_refcounts module_to_files =
      let submodule_refcounts = Reference.Table.create () in
      let process_module qualifier =
        let rec process_submodule ?(process_parent = true) maybe_qualifier =
          match maybe_qualifier with
          | None -> ()
          | Some qualifier when Reference.is_empty qualifier -> ()
          | Some qualifier ->
              Reference.Table.update submodule_refcounts qualifier ~f:(function
                  | None -> 1
                  | Some count -> count + 1);
              if process_parent then
                process_submodule ~process_parent:false (Reference.prefix qualifier)
        in
        process_submodule (Some qualifier)
      in
      Hashtbl.keys module_to_files |> List.iter ~f:process_module;
      submodule_refcounts


    let create ~configuration module_paths =
      let module_to_files = create_module_to_files ~configuration module_paths in
      let submodule_refcounts = create_submodule_refcounts module_to_files in
      { module_to_files; submodule_refcounts }


    module FileSystemEvent = struct
      type t =
        | Update of ArtifactPath.t
        | Remove of ArtifactPath.t

      let create path =
        if PyrePath.file_exists (ArtifactPath.raw path) then Update path else Remove path
    end

    module IncrementalExplicitUpdate = struct
      type t =
        | New of ModulePath.t
        | Changed of ModulePath.t
        | Delete of Reference.t
      [@@deriving sexp, compare]
    end

    module IncrementalImplicitUpdate = struct
      type t =
        | New of Reference.t
        | Delete of Reference.t
      [@@deriving sexp, compare]
    end

    let update_explicit_modules ~configuration ~artifact_paths module_to_files =
      (* Process a single filesystem event *)
      let process_filesystem_event ~configuration = function
        | FileSystemEvent.Update path -> (
            match ModulePath.create ~configuration path with
            | None ->
                Log.log ~section:`Server "`%a` not found in search path." ArtifactPath.pp path;
                None
            | Some ({ ModulePath.qualifier; _ } as module_path) -> (
                match Hashtbl.find module_to_files qualifier with
                | None ->
                    (* New file for a new module *)
                    Hashtbl.set module_to_files ~key:qualifier ~data:[module_path];
                    Some (IncrementalExplicitUpdate.New module_path)
                | Some module_paths ->
                    let new_module_paths =
                      insert_module_path ~configuration ~inserted:module_path module_paths
                    in
                    let new_module_path = List.hd_exn new_module_paths in
                    Hashtbl.set module_to_files ~key:qualifier ~data:new_module_paths;
                    if ModulePath.equal new_module_path module_path then
                      (* Updating a shadowing file means the module gets changed *)
                      Some (IncrementalExplicitUpdate.Changed module_path)
                    else (* Updating a shadowed file should not trigger any reanalysis *)
                      None))
        | FileSystemEvent.Remove path -> (
            match ModulePath.create ~configuration path with
            | None ->
                Log.log ~section:`Server "`%a` not found in search path." ArtifactPath.pp path;
                None
            | Some ({ ModulePath.qualifier; _ } as module_path) -> (
                Hashtbl.find module_to_files qualifier
                >>= fun module_paths ->
                match module_paths with
                | [] ->
                    (* This should never happen but handle it just in case *)
                    Hashtbl.remove module_to_files qualifier;
                    None
                | old_module_path :: _ -> (
                    match remove_module_path ~configuration ~removed:module_path module_paths with
                    | [] ->
                        (* Last remaining file for the module gets removed. *)
                        Hashtbl.remove module_to_files qualifier;
                        Some (IncrementalExplicitUpdate.Delete qualifier)
                    | new_module_path :: _ as new_module_paths ->
                        Hashtbl.set module_to_files ~key:qualifier ~data:new_module_paths;
                        if ModulePath.equal old_module_path new_module_path then
                          (* Removing a shadowed file should not trigger any reanalysis *)
                          None
                        else (* Removing module_path un-shadows another source file. *)
                          Some (IncrementalExplicitUpdate.Changed new_module_path))))
      in
      (* Make sure we have only one update per module *)
      let merge_updates updates =
        let table = Reference.Table.create () in
        let process_update update =
          match update with
          | IncrementalExplicitUpdate.New ({ ModulePath.qualifier; _ } as module_path) ->
              let update = function
                | None -> update
                | Some (IncrementalExplicitUpdate.Delete _) ->
                    IncrementalExplicitUpdate.Changed module_path
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
          | IncrementalExplicitUpdate.Changed ({ ModulePath.qualifier; _ } as module_path) ->
              let update = function
                | None
                | Some (IncrementalExplicitUpdate.Changed _) ->
                    update
                | Some (IncrementalExplicitUpdate.New _) ->
                    IncrementalExplicitUpdate.New module_path
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
                      Format.asprintf
                        "Illegal state: double delete module %a"
                        Reference.pp
                        qualifier
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
      List.dedup_and_sort ~compare:ArtifactPath.compare artifact_paths
      |> List.map ~f:FileSystemEvent.create
      |> List.filter_map ~f:(process_filesystem_event ~configuration)
      |> merge_updates


    let update_submodules ~events submodule_refcounts =
      let aggregate_updates events =
        let aggregated_refcounts = Reference.Table.create () in
        let process_event event =
          let rec do_update ?(parent = false) ~f = function
            | None -> ()
            | Some qualifier when Reference.is_empty qualifier -> ()
            | Some qualifier ->
                Hashtbl.update aggregated_refcounts qualifier ~f:(function
                    | None -> f 0
                    | Some count -> f count);
                if not parent then
                  do_update (Reference.prefix qualifier) ~parent:true ~f
          in
          match event with
          | IncrementalExplicitUpdate.Changed _ -> ()
          | IncrementalExplicitUpdate.New { ModulePath.qualifier; _ } ->
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


    let update ~configuration ~artifact_paths { module_to_files; submodule_refcounts } =
      let explicit_updates =
        update_explicit_modules module_to_files ~configuration ~artifact_paths
      in
      let implicit_updates = update_submodules submodule_refcounts ~events:explicit_updates in
      (* Explicit updates should shadow implicit updates *)
      let updates =
        let new_qualifiers = Reference.Hash_set.create () in
        let deleted_qualifiers = Reference.Hash_set.create () in
        let explicits =
          let process_explicit_update = function
            | IncrementalExplicitUpdate.New ({ ModulePath.qualifier; _ } as module_path)
            | IncrementalExplicitUpdate.Changed ({ ModulePath.qualifier; _ } as module_path) ->
                Hash_set.add new_qualifiers qualifier;
                IncrementalUpdate.NewExplicit module_path
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
    controls: EnvironmentControls.t;
    is_updatable: bool;
    get_raw_code: ModulePath.t -> (raw_code, message) Result.t;
  }

  let find_module_paths
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
        PyrePath.list ~file_filter ~directory_filter ~root () |> List.map ~f:ArtifactPath.create)
    |> List.concat
    |> List.filter_map ~f:(ModulePath.create ~configuration)


  let load_raw_code ~configuration module_path =
    let path = ModulePath.full_path ~configuration module_path in
    try Ok (ArtifactPath.raw path |> File.create |> File.content_exn) with
    | Sys_error error ->
        Error (Format.asprintf "Cannot open file `%a` due to: %s" ArtifactPath.pp path error)


  let configuration_allows_update { Configuration.Analysis.incremental_style; _ } =
    match incremental_style with
    | Configuration.Analysis.Shallow -> false
    | Configuration.Analysis.FineGrained -> true


  let create controls =
    Log.info "Building module tracker...";
    let timer = Timer.start () in
    let configuration = EnvironmentControls.configuration controls in
    let source_paths = find_module_paths configuration in
    let layouts = Layouts.create ~configuration source_paths in
    let get_raw_code = load_raw_code ~configuration in
    let is_updatable = configuration_allows_update configuration in
    Statistics.performance ~name:"module tracker built" ~timer ~phase_name:"Module tracking" ();
    { layouts; controls; is_updatable; get_raw_code }


  let create_for_testing controls module_path_code_pairs =
    let configuration = EnvironmentControls.configuration controls in
    let layouts =
      let module_paths = List.map ~f:fst module_path_code_pairs in
      Layouts.create ~configuration module_paths
    in
    let in_memory_sources =
      let table = Reference.Table.create () in
      let add_pair (module_path, code) =
        Reference.Table.set table ~key:(ModulePath.qualifier module_path) ~data:code
      in
      List.iter module_path_code_pairs ~f:add_pair;
      table
    in
    let get_raw_code ({ ModulePath.qualifier; _ } as module_path) =
      match Reference.Table.find in_memory_sources qualifier with
      | Some code -> Ok code
      | None -> load_raw_code ~configuration module_path
    in
    { layouts; controls; is_updatable = false; get_raw_code }


  let all_module_paths { layouts = { module_to_files; _ }; _ } =
    Hashtbl.data module_to_files |> List.concat


  let module_paths { layouts = { module_to_files; _ }; _ } =
    Hashtbl.data module_to_files |> List.filter_map ~f:List.hd


  let controls { controls; _ } = controls

  let assert_can_update { controls; is_updatable; _ } =
    if not (EnvironmentControls.track_dependencies controls) then
      failwith "Environments without dependency tracking cannot be updated";
    if not is_updatable then
      failwith
        "Environments created via create_for_testing with in-memory sources cannot be updated";
    ()


  let update ({ layouts; controls; _ } as tracker) ~artifact_paths =
    let timer = Timer.start () in
    assert_can_update tracker;
    let result =
      let configuration = EnvironmentControls.configuration controls in
      Layouts.update ~configuration ~artifact_paths layouts
    in
    Statistics.performance ~name:"module tracker updated" ~timer ~phase_name:"Module tracking" ();
    result


  module Serializer = struct
    module Layouts = Memory.Serializer (struct
      type nonrec t = Layouts.t

      module Serialized = struct
        type t = (Reference.t * ModulePath.t list) list * (Reference.t * int) list

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

    let from_stored_layouts ~controls () =
      let configuration = EnvironmentControls.configuration controls in
      let layouts = Layouts.load () in
      {
        layouts;
        controls;
        is_updatable = configuration_allows_update configuration;
        get_raw_code = load_raw_code ~configuration;
      }
  end

  let read_only
      ({ layouts = { module_to_files; submodule_refcounts }; controls; get_raw_code; _ } as tracker)
    =
    let lookup_module_path module_name =
      match Hashtbl.find module_to_files module_name with
      | Some (module_path :: _) -> Some module_path
      | _ -> None
    in
    let is_module_tracked qualifier =
      Hashtbl.mem module_to_files qualifier || Hashtbl.mem submodule_refcounts qualifier
    in
    {
      ReadOnly.lookup_module_path;
      is_module_tracked;
      get_raw_code;
      module_paths = (fun () -> module_paths tracker);
      controls = (fun () -> controls);
    }
end

module Overlay = struct
  module CodeUpdate = struct
    type t =
      | NewCode of raw_code
      | ResetCode
  end

  type t = {
    parent: ReadOnly.t;
    controls: EnvironmentControls.t;
    overlaid_code: string ModulePath.Table.t;
    overlaid_qualifiers: Reference.Hash_set.t;
  }

  let owns_qualifier { overlaid_qualifiers; _ } qualifier =
    Hash_set.mem overlaid_qualifiers qualifier


  let owns_reference environment reference =
    Reference.possible_qualifiers reference |> List.exists ~f:(owns_qualifier environment)


  let owns_identifier environment name = Reference.create name |> owns_reference environment

  let create parent =
    {
      parent;
      controls = ReadOnly.controls parent |> EnvironmentControls.create_for_overlay;
      overlaid_code = ModulePath.Table.create ();
      overlaid_qualifiers = Reference.Hash_set.create ();
    }


  let update_overlaid_code { parent; overlaid_code; overlaid_qualifiers; _ } ~code_updates =
    let add_or_update_code ~code_update module_path =
      let qualifier = ModulePath.qualifier module_path in
      let () =
        match code_update with
        | CodeUpdate.NewCode new_code ->
            ModulePath.Table.set overlaid_code ~key:module_path ~data:new_code
        | CodeUpdate.ResetCode -> ModulePath.Table.remove overlaid_code module_path
      in
      let () = Hash_set.add overlaid_qualifiers qualifier in
      IncrementalUpdate.NewExplicit module_path
    in
    let process_code_update (artifact_path, code_update) =
      let configuration = ReadOnly.controls parent |> EnvironmentControls.configuration in
      ModulePath.create ~configuration artifact_path >>| add_or_update_code ~code_update
    in
    List.filter_map code_updates ~f:process_code_update


  let read_only { parent; overlaid_code; _ } =
    let get_raw_code module_path =
      match ModulePath.Table.find overlaid_code module_path with
      | Some code -> Result.Ok code
      | _ -> ReadOnly.get_raw_code parent module_path
    in
    { parent with get_raw_code }
end

include Base
