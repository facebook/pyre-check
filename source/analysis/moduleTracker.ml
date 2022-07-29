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
    | Some { ModulePath.raw; qualifier; _ } -> (
        match lookup_module_path tracker qualifier with
        | None -> PathLookup.NotFound
        | Some ({ ModulePath.raw = tracked_raw; _ } as module_path) ->
            if ModulePath.Raw.equal raw tracked_raw then
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

module ModuleFinder = struct
  type t = {
    valid_suffixes: String.Set.t;
    excludes: Str.regexp list;
    configuration: Configuration.Analysis.t;
  }

  let create ({ Configuration.Analysis.excludes; _ } as configuration) =
    {
      valid_suffixes =
        ".py" :: ".pyi" :: Configuration.Analysis.extension_suffixes configuration
        |> String.Set.of_list;
      excludes;
      configuration;
    }


  let is_valid_filename_raw { valid_suffixes; _ } raw_path =
    let extension =
      Filename.split_extension raw_path
      |> snd
      >>| (fun extension -> "." ^ extension)
      |> Option.value ~default:""
    in
    (* Don't bother with hidden files as they are non-importable in Python by default *)
    (not (String.is_prefix (Filename.basename raw_path) ~prefix:"."))
    (* Only consider files with valid suffix *)
    && Set.mem valid_suffixes extension


  let is_valid_filename finder artifact_path =
    ArtifactPath.raw artifact_path |> PyrePath.absolute |> is_valid_filename_raw finder


  let mark_visited visited_paths path =
    match Hash_set.strict_add visited_paths path with
    | Result.Ok () -> false
    | _ -> true


  let python_file_filter finder ~visited_paths path =
    is_valid_filename_raw finder path && not (mark_visited visited_paths path)


  let package_directory_filter { excludes; _ } ~visited_paths ~root_path path =
    (* Don't bother with hidden directories (except in the case where the root itself is hidden) as
       they are non-importable in Python by default *)
    ((not (String.is_prefix (Filename.basename path) ~prefix:".")) || String.equal path root_path)
    (* Do not scan excluding directories to speed up the traversal *)
    && (not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0)))
    && not (mark_visited visited_paths path)


  module Implicits = struct
    module Existence = struct
      type t = {
        existed_before: bool;
        exists_after: bool;
      }

      type change =
        | Add
        | Remove
        | Unchanged

      let to_change { existed_before; exists_after } =
        match existed_before, exists_after with
        | false, true -> Add
        | true, false -> Remove
        | true, true
        | false, false ->
            Unchanged
    end

    let parent_qualifier_and_raw { ModulePath.raw; qualifier; _ } =
      Reference.prefix qualifier >>| fun parent_qualifier -> parent_qualifier, raw
  end
end

let find_module_paths ({ Configuration.Analysis.source_paths; search_paths; _ } as configuration) =
  let module_finder = ModuleFinder.create configuration in
  let visited_paths = String.Hash_set.create () in
  let search_roots =
    List.append
      (List.map ~f:SearchPath.to_path source_paths)
      (List.map ~f:SearchPath.to_path search_paths)
  in
  List.map search_roots ~f:(fun root ->
      let root_path = PyrePath.absolute root in
      let directory_filter =
        ModuleFinder.package_directory_filter module_finder ~visited_paths ~root_path
      in
      let file_filter = ModuleFinder.python_file_filter module_finder ~visited_paths in
      PyrePath.list ~file_filter ~directory_filter ~root () |> List.map ~f:ArtifactPath.create)
  |> List.concat
  |> List.filter_map ~f:(ModulePath.create ~configuration)


module Base = struct
  module Layouts = struct
    module ExplicitModules = struct
      module Value = struct
        type t = ModulePath.t list
      end

      module Update = struct
        type t =
          | New of ModulePath.t
          | Changed of ModulePath.t
          | Delete of Reference.t
        [@@deriving sexp, compare]
      end

      module Table = struct
        type t = Value.t Reference.Table.t
      end
    end

    module ImplicitModules = struct
      module Value = struct
        type t = ModulePath.Raw.Set.t
      end

      module Update = struct
        type t =
          | New of Reference.t
          | Delete of Reference.t
        [@@deriving sexp, compare]
      end

      module Table = struct
        type t = Value.t Reference.Table.t
      end
    end

    type t = {
      qualifier_to_modules: ExplicitModules.Table.t;
      qualifier_to_implicits: ImplicitModules.Table.t;
    }

    let insert_module_path ~configuration ~to_insert existing_paths =
      let rec insert sofar = function
        | [] -> List.rev_append sofar [to_insert]
        | current_path :: rest as existing -> (
            match ModulePath.same_module_compare ~configuration to_insert current_path with
            | 0 ->
                (* We have the following precondition for files that are in the same module: *)
                (* `same_module_compare a b = 0` implies `equal a b` *)
                assert (ModulePath.equal to_insert current_path);

                (* Duplicate entry detected. Do nothing *)
                existing_paths
            | x when x > 0 -> List.rev_append sofar (to_insert :: existing)
            | _ -> insert (current_path :: sofar) rest)
      in
      insert [] existing_paths


    let remove_module_path ~configuration ~to_remove existing_paths =
      let rec remove sofar = function
        | [] -> existing_paths
        | current_path :: rest -> (
            match ModulePath.same_module_compare ~configuration to_remove current_path with
            | 0 ->
                let () =
                  (* For removed files, we only check for equality on relative path & priority. *)
                  (* There's a corner case (where symlink is involved) that may cause `removed` to
                     have a different `is_external` flag. *)
                  let partially_equal { ModulePath.raw = left; _ } { ModulePath.raw = right; _ } =
                    ModulePath.Raw.equal left right
                  in
                  assert (partially_equal to_remove current_path)
                in
                List.rev_append sofar rest
            | x when x > 0 -> existing_paths
            | _ -> remove (current_path :: sofar) rest)
      in
      remove [] existing_paths


    let create_qualifier_to_modules ~configuration module_paths =
      let qualifier_to_modules = Reference.Table.create () in
      let process_module_path ({ ModulePath.qualifier; _ } as module_path) =
        let update_table = function
          | None -> [module_path]
          | Some module_paths ->
              insert_module_path ~configuration ~to_insert:module_path module_paths
        in
        Hashtbl.update qualifier_to_modules qualifier ~f:update_table
      in
      List.iter module_paths ~f:process_module_path;
      qualifier_to_modules


    let create_qualifier_to_implicits qualifier_to_modules =
      let qualifier_to_implicits = Reference.Table.create () in
      let process_module_path module_path =
        match ModuleFinder.Implicits.parent_qualifier_and_raw module_path with
        | None -> ()
        | Some (parent_qualifier, raw) ->
            Reference.Table.update qualifier_to_implicits parent_qualifier ~f:(function
                | None -> ModulePath.Raw.Set.singleton raw
                | Some paths -> ModulePath.Raw.Set.add raw paths)
      in
      Hashtbl.iter qualifier_to_modules ~f:(List.iter ~f:process_module_path);
      qualifier_to_implicits


    let create ~configuration module_paths =
      let qualifier_to_modules = create_qualifier_to_modules ~configuration module_paths in
      let qualifier_to_implicits = create_qualifier_to_implicits qualifier_to_modules in
      { qualifier_to_modules; qualifier_to_implicits }


    module FileSystemEvent = struct
      type t =
        | NewOrChanged of ModulePath.t
        | Remove of ModulePath.t

      let create ~configuration path =
        match ModulePath.create ~configuration path with
        | None ->
            Log.log ~section:`Server "`%a` not found in search path." ArtifactPath.pp path;
            None
        | Some module_path ->
            if PyrePath.file_exists (ArtifactPath.raw path) then
              Some (NewOrChanged module_path)
            else
              Some (Remove module_path)


      let module_path = function
        | NewOrChanged module_path
        | Remove module_path ->
            module_path
    end

    let create_filesystem_events ~configuration artifact_paths =
      (* Since the logic in `process_filesystem_event` is not idempotent, we don't want any
         duplicate ArtifactPaths in our filesystem events. *)
      List.dedup_and_sort ~compare:ArtifactPath.compare artifact_paths
      |> List.filter ~f:(ModuleFinder.is_valid_filename (ModuleFinder.create configuration))
      |> List.filter_map ~f:(FileSystemEvent.create ~configuration)


    let update_explicit_modules ~configuration ~filesystem_events qualifier_to_modules =
      (* Process a single filesystem event *)
      let process_filesystem_event ~configuration = function
        | FileSystemEvent.NewOrChanged ({ ModulePath.qualifier; _ } as module_path) -> (
            match Hashtbl.find qualifier_to_modules qualifier with
            | None ->
                (* New file for a new module *)
                Hashtbl.set qualifier_to_modules ~key:qualifier ~data:[module_path];
                Some (ExplicitModules.Update.New module_path)
            | Some module_paths ->
                let new_module_paths =
                  insert_module_path ~configuration ~to_insert:module_path module_paths
                in
                let new_module_path = List.hd_exn new_module_paths in
                Hashtbl.set qualifier_to_modules ~key:qualifier ~data:new_module_paths;
                if ModulePath.equal new_module_path module_path then
                  (* Updating a shadowing file means the module gets changed *)
                  Some (ExplicitModules.Update.Changed module_path)
                else (* Updating a shadowed file should not trigger any reanalysis *)
                  None)
        | FileSystemEvent.Remove ({ ModulePath.qualifier; _ } as module_path) -> (
            Hashtbl.find qualifier_to_modules qualifier
            >>= fun module_paths ->
            match module_paths with
            | [] ->
                (* This should never happen but handle it just in case *)
                Hashtbl.remove qualifier_to_modules qualifier;
                None
            | old_module_path :: _ -> (
                match remove_module_path ~configuration ~to_remove:module_path module_paths with
                | [] ->
                    (* Last remaining file for the module gets removed. *)
                    Hashtbl.remove qualifier_to_modules qualifier;
                    Some (ExplicitModules.Update.Delete qualifier)
                | new_module_path :: _ as new_module_paths ->
                    Hashtbl.set qualifier_to_modules ~key:qualifier ~data:new_module_paths;
                    if ModulePath.equal old_module_path new_module_path then
                      (* Removing a shadowed file should not trigger any reanalysis *)
                      None
                    else (* Removing module_path un-shadows another source file. *)
                      Some (ExplicitModules.Update.Changed new_module_path)))
      in
      (* Make sure we have only one update per module *)
      let merge_updates updates =
        let table = Reference.Table.create () in
        let process_update update =
          match update with
          | ExplicitModules.Update.New ({ ModulePath.qualifier; _ } as module_path) ->
              let update = function
                | None -> update
                | Some (ExplicitModules.Update.Delete _) ->
                    ExplicitModules.Update.Changed module_path
                | Some (ExplicitModules.Update.New _) ->
                    let message =
                      Format.asprintf "Illegal state: double new module %a" Reference.pp qualifier
                    in
                    failwith message
                | Some (ExplicitModules.Update.Changed _) ->
                    let message =
                      Format.asprintf
                        "Illegal state: new after changed module %a"
                        Reference.pp
                        qualifier
                    in
                    failwith message
              in
              Hashtbl.update table qualifier ~f:update
          | ExplicitModules.Update.Changed ({ ModulePath.qualifier; _ } as module_path) ->
              let update = function
                | None
                | Some (ExplicitModules.Update.Changed _) ->
                    update
                | Some (ExplicitModules.Update.New _) -> ExplicitModules.Update.New module_path
                | Some (ExplicitModules.Update.Delete _) ->
                    let message =
                      Format.asprintf
                        "Illegal state: changing a deleted module %a"
                        Reference.pp
                        qualifier
                    in
                    failwith message
              in
              Hashtbl.update table qualifier ~f:update
          | ExplicitModules.Update.Delete qualifier ->
              let update = function
                | None
                | Some (ExplicitModules.Update.Changed _) ->
                    Some update
                | Some (ExplicitModules.Update.New _) ->
                    let message =
                      Format.asprintf
                        "Illegal state: delete after new module %a"
                        Reference.pp
                        qualifier
                    in
                    failwith message
                | Some (ExplicitModules.Update.Delete _) ->
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
      List.filter_map filesystem_events ~f:(process_filesystem_event ~configuration)
      |> merge_updates


    let update_implicits ~filesystem_events qualifier_to_implicits =
      let treat_as_importable explicit_children =
        not (ModulePath.Raw.Set.is_empty explicit_children)
      in
      let process_filesystem_event previous_existence filesystem_event =
        let module_path = FileSystemEvent.module_path filesystem_event in
        match ModuleFinder.Implicits.parent_qualifier_and_raw module_path with
        | None -> previous_existence
        | Some (parent_qualifier, raw) ->
            (* Get the previous state and new state *)
            let previous_explicit_children =
              Reference.Table.find qualifier_to_implicits parent_qualifier
              |> Option.value ~default:ModulePath.Raw.Set.empty
            in
            let next_explicit_children =
              match filesystem_event with
              | FileSystemEvent.NewOrChanged _ ->
                  ModulePath.Raw.Set.add raw previous_explicit_children
              | FileSystemEvent.Remove _ -> ModulePath.Raw.Set.remove raw previous_explicit_children
            in
            (* update qualifier_to_implicits as a side effect *)
            let () =
              Reference.Table.update qualifier_to_implicits parent_qualifier ~f:(fun _ ->
                  next_explicit_children)
            in
            (* As we fold the updates, track existince before-and-after *)
            let next_existence =
              let open ModuleFinder.Implicits.Existence in
              Reference.Map.update previous_existence parent_qualifier ~f:(function
                  | None ->
                      {
                        existed_before = treat_as_importable previous_explicit_children;
                        exists_after = treat_as_importable next_explicit_children;
                      }
                  | Some { existed_before; _ } ->
                      { existed_before; exists_after = treat_as_importable next_explicit_children })
            in
            next_existence
      in
      let existence =
        List.fold filesystem_events ~init:Reference.Map.empty ~f:process_filesystem_event
      in
      let to_implicit_update ~key ~data =
        match ModuleFinder.Implicits.Existence.to_change data with
        | Add -> Some (ImplicitModules.Update.New key)
        | Remove -> Some (ImplicitModules.Update.Delete key)
        | Unchanged -> None
      in
      Reference.Map.filter_mapi existence ~f:to_implicit_update |> Reference.Map.data


    let update ~configuration ~artifact_paths { qualifier_to_modules; qualifier_to_implicits } =
      let filesystem_events = create_filesystem_events ~configuration artifact_paths in
      let explicit_updates =
        update_explicit_modules ~configuration ~filesystem_events qualifier_to_modules
      in
      let implicit_updates = update_implicits ~filesystem_events qualifier_to_implicits in
      (* Explicit updates should shadow implicit updates *)
      let updates =
        let explicitly_modified_qualifiers = Reference.Hash_set.create () in
        let explicits =
          let process_explicit_update = function
            | ExplicitModules.Update.New ({ ModulePath.qualifier; _ } as module_path)
            | ExplicitModules.Update.Changed ({ ModulePath.qualifier; _ } as module_path) ->
                Hash_set.add explicitly_modified_qualifiers qualifier;
                IncrementalUpdate.NewExplicit module_path
            | ExplicitModules.Update.Delete qualifier ->
                Hash_set.add explicitly_modified_qualifiers qualifier;
                IncrementalUpdate.Delete qualifier
          in
          List.map explicit_updates ~f:process_explicit_update
        in
        let implicits =
          let process_implicit_update = function
            | ImplicitModules.Update.New qualifier ->
                if Hash_set.mem explicitly_modified_qualifiers qualifier then
                  None
                else
                  Some (IncrementalUpdate.NewImplicit qualifier)
            | ImplicitModules.Update.Delete qualifier ->
                if Hash_set.mem explicitly_modified_qualifiers qualifier then
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
        [%message (explicit_updates : ExplicitModules.Update.t list)];
      Log.log
        ~section:`Server
        "Implicit Module Update: %a"
        Sexp.pp
        [%message (implicit_updates : ImplicitModules.Update.t list)];
      updates
  end

  type t = {
    layouts: Layouts.t;
    controls: EnvironmentControls.t;
    is_updatable: bool;
    get_raw_code: ModulePath.t -> (raw_code, message) Result.t;
  }

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


  let all_module_paths { layouts = { qualifier_to_modules; _ }; _ } =
    Hashtbl.data qualifier_to_modules |> List.concat


  let module_paths { layouts = { qualifier_to_modules; _ }; _ } =
    Hashtbl.data qualifier_to_modules |> List.filter_map ~f:List.hd


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
        type t = (Reference.t * ModulePath.t list) list * (Reference.t * ModulePath.Raw.Set.t) list

        let prefix = Prefix.make ()

        let description = "Module tracker"
      end

      let serialize { Layouts.qualifier_to_modules; qualifier_to_implicits } =
        Hashtbl.to_alist qualifier_to_modules, Hashtbl.to_alist qualifier_to_implicits


      let deserialize (module_data, implicits_data) =
        {
          Layouts.qualifier_to_modules = Reference.Table.of_alist_exn module_data;
          qualifier_to_implicits = Reference.Table.of_alist_exn implicits_data;
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
      ({ layouts = { qualifier_to_modules; qualifier_to_implicits }; controls; get_raw_code; _ } as
      tracker)
    =
    let lookup_module_path module_name =
      match Hashtbl.find qualifier_to_modules module_name with
      | Some (module_path :: _) -> Some module_path
      | _ -> None
    in
    let is_module_tracked qualifier =
      Hashtbl.mem qualifier_to_modules qualifier
      || not
           (Hashtbl.find qualifier_to_implicits qualifier
           |> Option.value ~default:ModulePath.Raw.Set.empty
           |> ModulePath.Raw.Set.is_empty)
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
