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

module ReadOnly = struct
  type t = {
    lookup_module_path: Reference.t -> ModulePath.t option;
    is_module_tracked: Reference.t -> bool;
    get_raw_code: ModulePath.t -> (raw_code, message) Result.t;
    module_paths: unit -> ModulePath.t list;
    all_module_paths: unit -> ModulePath.t list;
    controls: unit -> EnvironmentControls.t;
  }

  let controls { controls; _ } = controls ()

  let lookup_module_path { lookup_module_path; _ } = lookup_module_path

  let module_paths { module_paths; _ } = module_paths ()

  let all_module_paths { all_module_paths; _ } = all_module_paths ()

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

module ModulePaths = struct
  module Finder = struct
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
      (* Don't bother with hidden directories (except in the case where the root itself is hidden)
         as they are non-importable in Python by default *)
      ((not (String.is_prefix (Filename.basename path) ~prefix:".")) || String.equal path root_path)
      (* Do not scan excluding directories to speed up the traversal *)
      && (not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0)))
      && not (mark_visited visited_paths path)


    let find_all ({ Configuration.Analysis.source_paths; search_paths; _ } as configuration) =
      let finder = create configuration in
      let visited_paths = String.Hash_set.create () in
      let search_roots =
        List.append
          (List.map ~f:SearchPath.to_path source_paths)
          (List.map ~f:SearchPath.to_path search_paths)
      in
      List.map search_roots ~f:(fun root ->
          let root_path = PyrePath.absolute root in
          let directory_filter = package_directory_filter finder ~visited_paths ~root_path in
          let file_filter = python_file_filter finder ~visited_paths in
          PyrePath.list ~file_filter ~directory_filter ~root () |> List.map ~f:ArtifactPath.create)
      |> List.concat
      |> List.filter_map ~f:(ModulePath.create ~configuration)
  end

  module Update = struct
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


    let from_artifact_paths ~configuration artifact_paths =
      (* Since the logic in `process_module_path_update` is not idempotent, we don't want any
         duplicate ArtifactPaths in our module_path updates *)
      List.dedup_and_sort ~compare:ArtifactPath.compare artifact_paths
      |> List.filter ~f:(Finder.is_valid_filename (Finder.create configuration))
      |> List.filter_map ~f:(create ~configuration)
  end
end

module ExplicitModules = struct
  module Value = struct
    type t = ModulePath.t list

    let module_path = function
      | module_path :: _ -> Some module_path
      | [] -> None


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
  end

  module Update = struct
    type t =
      | New of ModulePath.t
      | Changed of ModulePath.t
      | Delete of Reference.t
    [@@deriving sexp, compare]

    let merge_updates updates =
      let table = Reference.Table.create () in
      let process_update update =
        match update with
        | New ({ ModulePath.qualifier; _ } as module_path) ->
            let update = function
              | None -> update
              | Some (Delete _) -> Changed module_path
              | Some (New _) ->
                  let message =
                    Format.asprintf "Illegal state: double new module %a" Reference.pp qualifier
                  in
                  failwith message
              | Some (Changed _) ->
                  let message =
                    Format.asprintf
                      "Illegal state: new after changed module %a"
                      Reference.pp
                      qualifier
                  in
                  failwith message
            in
            Hashtbl.update table qualifier ~f:update
        | Changed ({ ModulePath.qualifier; _ } as module_path) ->
            let update = function
              | None
              | Some (Changed _) ->
                  update
              | Some (New _) -> New module_path
              | Some (Delete _) ->
                  let message =
                    Format.asprintf
                      "Illegal state: changing a deleted module %a"
                      Reference.pp
                      qualifier
                  in
                  failwith message
            in
            Hashtbl.update table qualifier ~f:update
        | Delete qualifier ->
            let update = function
              | None
              | Some (Changed _) ->
                  Some update
              | Some (New _) ->
                  let message =
                    Format.asprintf
                      "Illegal state: delete after new module %a"
                      Reference.pp
                      qualifier
                  in
                  failwith message
              | Some (Delete _) ->
                  let message =
                    Format.asprintf "Illegal state: double delete module %a" Reference.pp qualifier
                  in
                  failwith message
            in
            Hashtbl.change table qualifier ~f:update
      in
      List.iter updates ~f:process_update;
      Hashtbl.data table
  end

  module Table = struct
    module Api = struct
      type t = {
        find: qualifier:Reference.t -> Value.t option;
        set: qualifier:Reference.t -> Value.t -> unit;
        remove: qualifier:Reference.t -> unit;
        data: unit -> Value.t list;
      }

      let update_module_paths ~configuration ~module_path_updates { find; set; remove; _ } =
        (* Process a single module_path update *)
        let process_module_path_update ~configuration = function
          | ModulePaths.Update.NewOrChanged ({ ModulePath.qualifier; _ } as module_path) -> (
              match find ~qualifier with
              | None ->
                  (* New file for a new module *)
                  set ~qualifier [module_path];
                  Some (Update.New module_path)
              | Some module_paths ->
                  let new_module_paths =
                    Value.insert_module_path ~configuration ~to_insert:module_path module_paths
                  in
                  let new_module_path = List.hd_exn new_module_paths in
                  set ~qualifier new_module_paths;
                  if ModulePath.equal new_module_path module_path then
                    (* Updating a shadowing file means the module gets changed *)
                    Some (Update.Changed module_path)
                  else (* Updating a shadowed file should not trigger any reanalysis *)
                    None)
          | ModulePaths.Update.Remove ({ ModulePath.qualifier; _ } as module_path) -> (
              find ~qualifier
              >>= fun module_paths ->
              match module_paths with
              | [] ->
                  (* This should never happen but handle it just in case *)
                  remove ~qualifier;
                  None
              | old_module_path :: _ -> (
                  match
                    Value.remove_module_path ~configuration ~to_remove:module_path module_paths
                  with
                  | [] ->
                      (* Last remaining file for the module gets removed. *)
                      remove ~qualifier;
                      Some (Update.Delete qualifier)
                  | new_module_path :: _ as new_module_paths ->
                      set ~qualifier new_module_paths;
                      if ModulePath.equal old_module_path new_module_path then
                        (* Removing a shadowed file should not trigger any reanalysis *)
                        None
                      else (* Removing module_path un-shadows another source file. *)
                        Some (Update.Changed new_module_path)))
        in
        (* Make sure we have only one update per module *)
        List.filter_map module_path_updates ~f:(process_module_path_update ~configuration)
        |> Update.merge_updates
    end

    module Eager = struct
      type t = Value.t Reference.Table.t

      let create ~configuration module_paths =
        let explicit_modules = Reference.Table.create () in
        let process_module_path ({ ModulePath.qualifier; _ } as module_path) =
          let update_table = function
            | None -> [module_path]
            | Some module_paths ->
                Value.insert_module_path ~configuration ~to_insert:module_path module_paths
          in
          Hashtbl.update explicit_modules qualifier ~f:update_table
        in
        List.iter module_paths ~f:process_module_path;
        explicit_modules


      let to_api eager =
        let find ~qualifier = Reference.Table.find eager qualifier in
        let set ~qualifier value = Reference.Table.set eager ~key:qualifier ~data:value in
        let remove ~qualifier = Reference.Table.remove eager qualifier in
        let data () = Hashtbl.data eager in
        Api.{ find; set; remove; data }
    end
  end
end

module ImplicitModules = struct
  (* Given a ModulePath.t, determine the qualifier of the parent (the key in our
     ImplicitModules.Table) and the ModulePath.Raw.t representing this child *)
  let parent_qualifier_and_raw { ModulePath.raw; qualifier; _ } =
    Reference.prefix qualifier >>| fun parent_qualifier -> parent_qualifier, raw


  module Value = struct
    (* This represents the raw paths of all *explicit* children. We treat a namespace package as
       importable only when it has regular python files as children, i.e. when this set is nonempty. *)
    type t = ModulePath.Raw.Set.t

    let should_treat_as_importable explicit_children =
      not (ModulePath.Raw.Set.is_empty explicit_children)
  end

  module Update = struct
    type t =
      | New of Reference.t
      | Delete of Reference.t
    [@@deriving sexp, compare]
  end

  module Table = struct
    module Api = struct
      type t = {
        find: qualifier:Reference.t -> Value.t option;
        set: qualifier:Reference.t -> Value.t -> unit;
      }

      module Existence = struct
        type t = {
          existed_before: bool;
          exists_after: bool;
        }

        let to_update ~qualifier { existed_before; exists_after } =
          match existed_before, exists_after with
          | false, true -> Some (Update.New qualifier)
          | true, false -> Some (Update.Delete qualifier)
          | true, true
          | false, false ->
              None
      end

      let update_module_paths ~module_path_updates { find; set } =
        let process_module_path_update previous_existence module_path_update =
          let module_path = ModulePaths.Update.module_path module_path_update in
          match parent_qualifier_and_raw module_path with
          | None -> previous_existence
          | Some (qualifier, raw_child) ->
              (* Get the previous state and new state *)
              let previous_explicit_children =
                find ~qualifier |> Option.value ~default:ModulePath.Raw.Set.empty
              in
              let next_explicit_children =
                match module_path_update with
                | ModulePaths.Update.NewOrChanged _ ->
                    ModulePath.Raw.Set.add raw_child previous_explicit_children
                | ModulePaths.Update.Remove _ ->
                    ModulePath.Raw.Set.remove raw_child previous_explicit_children
              in
              (* update implicit_modules as a side effect *)
              let () = set ~qualifier next_explicit_children in
              (* As we fold the updates, track existince before-and-after *)
              let next_existence =
                let open Existence in
                Reference.Map.update previous_existence qualifier ~f:(function
                    | None ->
                        {
                          existed_before =
                            Value.should_treat_as_importable previous_explicit_children;
                          exists_after = Value.should_treat_as_importable next_explicit_children;
                        }
                    | Some { existed_before; _ } ->
                        {
                          existed_before;
                          exists_after = Value.should_treat_as_importable next_explicit_children;
                        })
              in
              next_existence
        in
        List.fold module_path_updates ~init:Reference.Map.empty ~f:process_module_path_update
        |> Reference.Map.filter_mapi ~f:(fun ~key ~data -> Existence.to_update ~qualifier:key data)
        |> Reference.Map.data
    end

    module Eager = struct
      type t = Value.t Reference.Table.t

      let create explicit_modules =
        let implicit_modules = Reference.Table.create () in
        let process_module_path module_path =
          match parent_qualifier_and_raw module_path with
          | None -> ()
          | Some (parent_qualifier, raw) ->
              Reference.Table.update implicit_modules parent_qualifier ~f:(function
                  | None -> ModulePath.Raw.Set.singleton raw
                  | Some paths -> ModulePath.Raw.Set.add raw paths)
        in
        Reference.Table.iter explicit_modules ~f:(List.iter ~f:process_module_path);
        implicit_modules


      let to_api eager =
        let find ~qualifier = Reference.Table.find eager qualifier in
        let set ~qualifier data = Reference.Table.set eager ~key:qualifier ~data in
        Api.{ find; set }
    end
  end
end

module IncrementalUpdate = struct
  type t =
    | NewExplicit of Ast.ModulePath.t
    | NewImplicit of Ast.Reference.t
    | Delete of Reference.t
  [@@deriving show, sexp, compare]

  let equal = [%compare.equal: t]

  let combine_explicits_and_implicits explicit_updates implicit_updates =
    (* Explicit updates should shadow implicit updates *)
    let explicitly_modified_qualifiers = Reference.Hash_set.create () in
    let explicits =
      let process_explicit_update = function
        | ExplicitModules.Update.New ({ ModulePath.qualifier; _ } as module_path)
        | ExplicitModules.Update.Changed ({ ModulePath.qualifier; _ } as module_path) ->
            Hash_set.add explicitly_modified_qualifiers qualifier;
            NewExplicit module_path
        | ExplicitModules.Update.Delete qualifier ->
            Hash_set.add explicitly_modified_qualifiers qualifier;
            Delete qualifier
      in
      List.map explicit_updates ~f:process_explicit_update
    in
    let implicits =
      let process_implicit_update = function
        | ImplicitModules.Update.New qualifier ->
            if Hash_set.mem explicitly_modified_qualifiers qualifier then
              None
            else
              Some (NewImplicit qualifier)
        | ImplicitModules.Update.Delete qualifier ->
            if Hash_set.mem explicitly_modified_qualifiers qualifier then
              None
            else
              Some (Delete qualifier)
      in
      List.filter_map implicit_updates ~f:process_implicit_update
    in
    List.append explicits implicits
end

module Layouts = struct
  module Api = struct
    type t = {
      explicit_modules: ExplicitModules.Table.Api.t;
      implicit_modules: ImplicitModules.Table.Api.t;
      store: unit -> unit;
    }

    let update ~configuration ~artifact_paths { explicit_modules; implicit_modules; _ } =
      let module_path_updates =
        ModulePaths.Update.from_artifact_paths ~configuration artifact_paths
      in
      let explicit_updates =
        ExplicitModules.Table.Api.update_module_paths
          ~configuration
          ~module_path_updates
          explicit_modules
      in
      let implicit_updates =
        ImplicitModules.Table.Api.update_module_paths ~module_path_updates implicit_modules
      in
      let updates =
        IncrementalUpdate.combine_explicits_and_implicits explicit_updates implicit_updates
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


    let lookup_module_path { explicit_modules = { find; _ }; _ } ~qualifier =
      find ~qualifier >>= ExplicitModules.Value.module_path


    let is_explicit_module layouts ~qualifier =
      lookup_module_path layouts ~qualifier |> Option.is_some


    let is_implicit_module { implicit_modules = { find; _ }; _ } ~qualifier =
      find ~qualifier
      >>| ImplicitModules.Value.should_treat_as_importable
      |> Option.value ~default:false


    let is_module_tracked layouts ~qualifier =
      is_explicit_module layouts ~qualifier || is_implicit_module layouts ~qualifier


    let all_module_paths { explicit_modules = { data; _ }; _ } = data () |> List.concat

    let module_paths { explicit_modules = { data; _ }; _ } =
      data () |> List.filter_map ~f:ExplicitModules.Value.module_path


    let store { store; _ } = store ()
  end

  module Eager = struct
    type t = {
      explicit_modules: ExplicitModules.Table.Eager.t;
      implicit_modules: ImplicitModules.Table.Eager.t;
    }

    let create ~controls =
      let configuration = EnvironmentControls.configuration controls in
      let module_paths =
        match EnvironmentControls.in_memory_sources controls with
        | None -> ModulePaths.Finder.find_all configuration
        | Some in_memory_sources -> List.map in_memory_sources ~f:fst
      in
      let explicit_modules = ExplicitModules.Table.Eager.create ~configuration module_paths in
      let implicit_modules = ImplicitModules.Table.Eager.create explicit_modules in
      { explicit_modules; implicit_modules }


    module Serializer = Memory.Serializer (struct
      type nonrec t = t

      module Serialized = struct
        type t = (Reference.t * ModulePath.t list) list * (Reference.t * ModulePath.Raw.Set.t) list

        let prefix = Prefix.make ()

        let description = "Module tracker"
      end

      let serialize { explicit_modules; implicit_modules } =
        Hashtbl.to_alist explicit_modules, Hashtbl.to_alist implicit_modules


      let deserialize (module_data, implicits_data) =
        {
          explicit_modules = Reference.Table.of_alist_exn module_data;
          implicit_modules = Reference.Table.of_alist_exn implicits_data;
        }
    end)

    let to_api ({ explicit_modules; implicit_modules } as layouts) =
      Api.
        {
          explicit_modules = ExplicitModules.Table.Eager.to_api explicit_modules;
          implicit_modules = ImplicitModules.Table.Eager.to_api implicit_modules;
          store = (fun () -> Serializer.store layouts);
        }
  end

  let create ~controls = Eager.create ~controls |> Eager.to_api

  let load ~controls:_ = Eager.Serializer.load () |> Eager.to_api
end

module Base = struct
  type t = {
    layouts: Layouts.Api.t;
    controls: EnvironmentControls.t;
    get_raw_code: ModulePath.t -> (raw_code, message) Result.t;
  }

  let load_raw_code ~configuration module_path =
    let path = ModulePath.full_path ~configuration module_path in
    try Ok (ArtifactPath.raw path |> File.create |> File.content_exn) with
    | Sys_error error ->
        Error (Format.asprintf "Cannot open file `%a` due to: %s" ArtifactPath.pp path error)


  let make_get_raw_code ~controls =
    let configuration = EnvironmentControls.configuration controls in
    match EnvironmentControls.in_memory_sources controls with
    | None -> load_raw_code ~configuration
    | Some in_memory_sources ->
        let in_memory_sources =
          let table = Reference.Table.create () in
          let add_pair (module_path, code) =
            Reference.Table.set table ~key:(ModulePath.qualifier module_path) ~data:code
          in
          List.iter in_memory_sources ~f:add_pair;
          table
        in
        let get_raw_code ({ ModulePath.qualifier; _ } as module_path) =
          match Reference.Table.find in_memory_sources qualifier with
          | Some code -> Ok code
          | None -> load_raw_code ~configuration module_path
        in
        get_raw_code


  let create controls =
    Log.info "Building module tracker...";
    let timer = Timer.start () in
    let layouts = Layouts.create ~controls in
    let get_raw_code = make_get_raw_code ~controls in
    Statistics.performance ~name:"module tracker built" ~timer ~phase_name:"Module tracking" ();
    { layouts; controls; get_raw_code }


  let controls { controls; _ } = controls

  let update { layouts; controls; _ } ~artifact_paths =
    let timer = Timer.start () in
    EnvironmentControls.assert_allow_updates controls;
    let result =
      let configuration = EnvironmentControls.configuration controls in
      Layouts.Api.update layouts ~configuration ~artifact_paths
    in
    Statistics.performance ~name:"module tracker updated" ~timer ~phase_name:"Module tracking" ();
    result


  module Serializer = struct
    let store_layouts { layouts; _ } = Layouts.Api.store layouts

    let from_stored_layouts ~controls () =
      let configuration = EnvironmentControls.configuration controls in
      let layouts = Layouts.load ~controls in
      { layouts; controls; get_raw_code = load_raw_code ~configuration }
  end

  let read_only { layouts; controls; get_raw_code; _ } =
    let lookup_module_path qualifier = Layouts.Api.lookup_module_path layouts ~qualifier in
    let is_module_tracked qualifier = Layouts.Api.is_module_tracked layouts ~qualifier in
    let module_paths () = Layouts.Api.module_paths layouts in
    let all_module_paths () = Layouts.Api.all_module_paths layouts in
    {
      ReadOnly.lookup_module_path;
      is_module_tracked;
      get_raw_code;
      module_paths;
      all_module_paths;
      controls = (fun () -> controls);
    }


  let module_paths tracker = read_only tracker |> ReadOnly.module_paths

  let all_module_paths tracker = read_only tracker |> ReadOnly.all_module_paths
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
