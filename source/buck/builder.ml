(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Path = Pyre.Path

exception JsonError of string

exception LinkTreeConstructionError of string

module BuildResult = struct
  type t = {
    build_map: BuildMap.t;
    targets: Target.t list;
  }
end

module IncrementalBuildResult = struct
  type t = {
    build_map: BuildMap.t;
    targets: Target.t list;
    changed_artifacts: PyrePath.t list;
  }
end

let source_database_suffix = "#source-db"

module BuckOptions = struct
  type t = {
    raw: Raw.t;
    mode: string option;
    isolation_prefix: string option;
  }
end

type t = {
  buck_options: BuckOptions.t;
  source_root: Path.t;
  artifact_root: Path.t;
}

let create ?mode ?isolation_prefix ~source_root ~artifact_root raw =
  { buck_options = { BuckOptions.raw; mode; isolation_prefix }; source_root; artifact_root }


let query_buck_for_normalized_targets
    { BuckOptions.raw; mode; isolation_prefix }
    target_specifications
  =
  match target_specifications with
  | [] -> Lwt.return "{}"
  | _ ->
      List.concat
        [
          (* Force `buck` to hand back structured JSON output instead of plain text. *)
          ["--json"];
          (* Mark the query as coming from `pyre` for `buck`, to make troubleshooting easier. *)
          ["--config"; "client.id=pyre"];
          Option.value_map mode ~default:[] ~f:(fun mode -> [mode]);
          [
            "kind(\"python_binary|python_library|python_test\", %s)"
            (* Don't bother with generated rules. *)
            ^ " - attrfilter(labels, generated, %s)"
            (* `python_unittest()` sources are separated into a macro-generated library, so make
               sure we include those. *)
            ^ " + attrfilter(labels, unittest-library, %s)"
            ^ (* Provide an opt-out label so that rules can avoid type-checking (e.g. some libraries
                 wrap generated sources which are expensive to build and therefore typecheck). *)
            " - attrfilter(labels, no_pyre, %s)";
          ];
          target_specifications;
        ]
      |> Raw.query ?isolation_prefix raw


let query_buck_for_changed_targets ~targets { BuckOptions.raw; mode; isolation_prefix } source_paths
  =
  match targets with
  | [] -> Lwt.return "{}"
  | targets -> (
      match source_paths with
      | [] -> Lwt.return "{}"
      | source_paths ->
          let target_string =
            (* Targets need to be quoted since `buck query` can fail with syntax errors if target
               name contains special characters like `=`. *)
            let quote_string value = Format.sprintf "\"%s\"" value in
            let quote_target target = Target.show target |> quote_string in
            List.map targets ~f:quote_target |> String.concat ~sep:" "
          in
          List.concat
            [
              ["--json"];
              ["--config"; "client.id=pyre"];
              Option.value_map mode ~default:[] ~f:(fun mode -> [mode]);
              [
                (* This will get only those owner targets that are beneath our targets or the
                   dependencies of our targets. *)
                Format.sprintf "owner(%%s) ^ deps(set(%s))" target_string;
              ];
              List.map source_paths ~f:Path.show;
              (* These attributes are all we need to locate the source and artifact relative paths. *)
              ["--output-attributes"; "srcs"; "buck.base_path"; "buck.base_module"; "base_module"];
            ]
          |> Raw.query ?isolation_prefix raw)


let run_buck_build_for_targets { BuckOptions.raw; mode; isolation_prefix } targets =
  match targets with
  | [] -> Lwt.return "{}"
  | _ ->
      List.concat
        [
          (* Force `buck` to hand back structured JSON output instead of plain text. *)
          ["--show-full-json-output"];
          (* Mark the query as coming from `pyre` for `buck`, to make troubleshooting easier. *)
          ["--config"; "client.id=pyre"];
          Option.value_map mode ~default:[] ~f:(fun mode -> [mode]);
          List.map targets ~f:(fun target ->
              Format.sprintf "%s%s" (Target.show target) source_database_suffix);
        ]
      |> Raw.build ?isolation_prefix raw


let parse_buck_normalized_targets_query_output query_output =
  let is_ignored_target target =
    (* We should probably tag these targets as `no_pyre` in the long run. *)
    String.is_suffix target ~suffix:"-mypy_ini"
    || String.is_suffix target ~suffix:"-testmodules-lib"
  in
  let open Yojson.Safe in
  try
    from_string ~fname:"buck query output" query_output
    |> Util.to_assoc
    |> List.map ~f:(fun (_, targets_json) ->
           Util.to_list targets_json
           |> List.map ~f:Util.to_string
           |> List.filter ~f:(Fn.non is_ignored_target))
    |> List.concat_no_order
    |> List.dedup_and_sort ~compare:String.compare
  with
  | Yojson.Json_error message
  | Util.Type_error (message, _) ->
      raise (JsonError message)


let parse_buck_build_output query_output =
  let open Yojson.Safe in
  try
    from_string ~fname:"buck build output" query_output
    |> Util.to_assoc
    |> List.map ~f:(fun (target, path_json) -> target, Util.to_string path_json)
  with
  | Yojson.Json_error message
  | Util.Type_error (message, _) ->
      raise (JsonError message)


let load_partial_build_map_from_json json =
  let filter_mapping ~key ~data:_ =
    match key with
    | "__manifest__.py"
    | "__test_main__.py"
    | "__test_modules__.py" ->
        (* These files are not useful for type checking but create many conflicts when merging
           different targets. *)
        false
    | _ -> true
  in
  BuildMap.Partial.of_json_exn_ignoring_duplicates json |> BuildMap.Partial.filter ~f:filter_mapping


let load_partial_build_map path =
  let open Lwt.Infix in
  let path = Path.absolute path in
  Lwt_io.(with_file ~mode:Input path read)
  >>= fun content ->
  try
    Yojson.Safe.from_string ~fname:path content |> load_partial_build_map_from_json |> Lwt.return
  with
  | Yojson.Safe.Util.Type_error (message, _)
  | Yojson.Safe.Util.Undefined (message, _) ->
      raise (JsonError message)
  | Yojson.Json_error message -> raise (JsonError message)


(* Given a list of buck target specifications (which may contain `...` or filter expression), query
   `buck` and return the set of individual targets which will be built. May raise
   [Buck.Raw.BuckError] when `buck` invocation fails, or [Buck.Builder.JsonError] when `buck` itself
   succeeds but its output cannot be parsed. *)
let normalize_targets buck_options target_specifications =
  let open Lwt.Infix in
  Log.info "Collecting buck targets to build...";
  query_buck_for_normalized_targets buck_options target_specifications
  >>= fun query_output ->
  let targets =
    parse_buck_normalized_targets_query_output query_output |> List.map ~f:Target.of_string
  in
  Log.info "Collected %d targets" (List.length targets);
  Lwt.return targets


(* Run `buck build` on the given target with the `#source-db` flavor. This will make `buck`
   construct its link tree and for each target, dump a source-db JSON file containing how files in
   the link tree corresponds to the final Python artifacts. Return a list containing the input
   targets as well as the corresponding location of the source-db JSON file. Note that targets in
   the returned list is not guaranteed to be in the same order as the input list.

   May raise [Buck.Raw.BuckError] when `buck` invocation fails, or [Buck.Builder.JsonError] when
   `buck` itself succeeds but its output cannot be parsed. *)
let build_source_databases buck_options targets =
  let open Lwt.Infix in
  Log.info "Building Buck source databases...";
  run_buck_build_for_targets buck_options targets
  >>= fun build_output ->
  let source_database_suffix_length = String.length source_database_suffix in
  parse_buck_build_output build_output
  |> List.map ~f:(fun (target, path) ->
         ( String.drop_suffix target source_database_suffix_length |> Target.of_string,
           Path.create_absolute path ))
  |> Lwt.return


let load_build_maps target_and_source_database_paths =
  Log.info "Loading Buck source databases from files...";
  let load_build_map (target, source_database_path) =
    let open Lwt.Infix in
    load_partial_build_map source_database_path >>= fun build_map -> Lwt.return (target, build_map)
  in
  (* Make sure the targets are in a determinstic order. This is important to make the merging
     process deterministic later. *)
  List.sort target_and_source_database_paths ~compare:(fun (left_target, _) (right_target, _) ->
      Target.compare left_target right_target)
  |> List.map ~f:load_build_map
  |> Lwt.all


let merge_build_maps target_and_build_maps =
  Log.info "Merging source databases...";
  let merge (target_and_build_maps_sofar, build_map_sofar) (next_target, next_build_map) =
    let open BuildMap.Partial in
    match merge build_map_sofar next_build_map with
    | MergeResult.Incompatible { MergeResult.IncompatibleItem.key; left_value; right_value } ->
        Log.warning "Cannot include target for type checking: %s" (Target.show next_target);
        (* For better error message, try to figure out which target casued the conflict. *)
        let conflicting_target =
          let match_target ~key (target, build_map) =
            if contains ~key build_map then Some target else None
          in
          List.find_map target_and_build_maps_sofar ~f:(match_target ~key)
        in
        Log.info
          "... file `%s` has already been mapped to `%s`%s but the target maps it to `%s` instead. "
          key
          left_value
          (Option.value_map conflicting_target ~default:"" ~f:(Format.sprintf " by `%s`"))
          right_value;
        target_and_build_maps_sofar, build_map_sofar
    | MergeResult.Ok merged_build_map ->
        (next_target, next_build_map) :: target_and_build_maps_sofar, merged_build_map
  in
  let reversed_target_and_build_maps, merged_build_map =
    List.fold target_and_build_maps ~init:([], BuildMap.Partial.empty) ~f:merge
  in
  let targets = List.rev_map reversed_target_and_build_maps ~f:fst in
  if List.length targets < List.length target_and_build_maps then
    Log.warning
      "One or more targets get dropped by Pyre due to potential conflicts. For more details, see \
       https://fburl.com/pyre-target-conflict";
  targets, BuildMap.create merged_build_map


(* Given a list of (target, path) obtained from [build_source_databases], load the source-db JSON
   file for each target, and merge the source-db for all targets into one single build map. May
   raise [Buck.Builder.JsonError] if the JSON loading fails.

   Source-db merging may not always succeed (see {!val:Buck.BuildMap.Partial.merge}). If it is
   deteced that the source-db for one target cannot be merged into the build map due to confliction,
   a warning will be printed and the target will be dropped. If a target is dropped, it will not
   show up in the final target list returned from this API (alongside with the build map). *)
let load_and_merge_source_databases target_and_source_database_paths =
  let open Lwt.Infix in
  load_build_maps target_and_source_database_paths
  >>= fun target_and_build_maps -> Lwt.return (merge_build_maps target_and_build_maps)


(* A convenient wrapper that stitches together [normalize_targets], [build_source_databases], and
   [load_and_merge_source_databases]. *)
let construct_build_map buck_options target_specifications =
  let open Lwt.Infix in
  normalize_targets buck_options target_specifications
  >>= fun normalized_targets ->
  build_source_databases buck_options normalized_targets
  >>= fun target_and_source_database_paths ->
  load_and_merge_source_databases target_and_source_database_paths


let build ~targets { buck_options; source_root; artifact_root } =
  let open Lwt.Infix in
  construct_build_map buck_options targets
  >>= fun (targets, build_map) ->
  Log.info "Constructing Python link-tree for type checking...";
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () -> Lwt.return { BuildResult.targets; build_map }


let restore ~build_map { source_root; artifact_root; _ } =
  let open Lwt.Infix in
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () -> Lwt.return_unit


let update_artifacts ~source_root ~artifact_root difference =
  let open Lwt.Infix in
  Log.info "Incrementally updating Python link-tree for type checking...";
  Artifacts.update ~source_root ~artifact_root difference
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () ->
      let to_artifact_path (relative, _) = Path.create_relative ~root:artifact_root ~relative in
      BuildMap.Difference.to_alist difference |> List.map ~f:to_artifact_path |> Lwt.return


let do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map () =
  let difference =
    Log.info "Calculating the scope of the re-build...";
    BuildMap.difference ~original:old_build_map new_build_map
  in
  update_artifacts ~source_root ~artifact_root difference


let full_incremental_build ~old_build_map ~targets { buck_options; source_root; artifact_root } =
  let open Lwt.Infix in
  construct_build_map buck_options targets
  >>= fun (targets, build_map) ->
  do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
  >>= fun changed_artifacts ->
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let incremental_build_with_normalized_targets
    ~old_build_map
    ~targets
    { buck_options; source_root; artifact_root }
  =
  let open Lwt.Infix in
  build_source_databases buck_options targets
  >>= fun target_and_source_database_paths ->
  load_and_merge_source_databases target_and_source_database_paths
  >>= fun (targets, build_map) ->
  do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
  >>= fun changed_artifacts ->
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


module BuckChangedTargetsQueryOutput = struct
  type t = {
    source_base_path: string;
    artifact_base_path: string;
    artifacts_to_sources: (string * string) list;
  }
  [@@deriving sexp, compare]

  let to_partial_build_map { source_base_path; artifact_base_path; artifacts_to_sources } =
    let to_build_mapping (artifact, source) =
      Filename.concat artifact_base_path artifact, Filename.concat source_base_path source
    in
    match BuildMap.Partial.of_alist (List.map artifacts_to_sources ~f:to_build_mapping) with
    | `Duplicate_key artifact ->
        let message = Format.sprintf "Overlapping artifact file detected: %s" artifact in
        Result.Error message
    | `Ok partial_build_map -> Result.Ok partial_build_map


  let to_build_map_batch outputs =
    let rec merge ~sofar = function
      | [] -> Result.Ok (BuildMap.create sofar)
      | output :: rest -> (
          match to_partial_build_map output with
          | Result.Error _ as error -> error
          | Result.Ok next_build_map -> (
              match BuildMap.Partial.merge sofar next_build_map with
              | BuildMap.Partial.MergeResult.Incompatible
                  { BuildMap.Partial.MergeResult.IncompatibleItem.key; _ } ->
                  let message = Format.sprintf "Overlapping artifact file detected: %s" key in
                  Result.Error message
              | BuildMap.Partial.MergeResult.Ok sofar -> merge ~sofar rest))
    in
    merge ~sofar:BuildMap.Partial.empty outputs
end

let parse_buck_changed_targets_query_output query_output =
  let open Yojson.Safe in
  try
    let parse_target_json target_json =
      let source_base_path = Util.member "buck.base_path" target_json |> Util.to_string in
      let artifact_base_path =
        match Util.member "buck.base_module" target_json with
        | `String base_module -> String.tr ~target:'.' ~replacement:'/' base_module
        | _ -> source_base_path
      in
      let artifact_base_path =
        match Util.member "base_module" target_json with
        | `String base_module -> String.tr ~target:'.' ~replacement:'/' base_module
        | _ -> artifact_base_path
      in
      let artifacts_to_sources =
        match Util.member "srcs" target_json with
        | `Assoc targets_to_sources ->
            List.map targets_to_sources ~f:(fun (target, source_json) ->
                target, Util.to_string source_json)
            |> List.filter ~f:(function
                   | _, source when String.is_prefix ~prefix:"//" source ->
                       (* This can happen for custom rules. *)
                       false
                   | _ -> true)
        | _ -> []
      in
      { BuckChangedTargetsQueryOutput.source_base_path; artifact_base_path; artifacts_to_sources }
    in
    from_string ~fname:"buck changed paths query output" query_output
    |> Util.to_assoc
    |> List.map ~f:(fun (_, target_json) -> parse_target_json target_json)
  with
  | Yojson.Json_error message
  | Util.Type_error (message, _) ->
      raise (JsonError message)


let to_relative_path ~root path = Path.get_relative_to_root ~root ~path

let to_relative_paths ~root paths = List.filter_map paths ~f:(to_relative_path ~root)

let compute_difference_from_removed_relative_paths ~build_map_index removed_paths =
  List.concat_map removed_paths ~f:(BuildMap.Indexed.lookup_artifact build_map_index)
  |> List.map ~f:(fun artifact -> artifact, BuildMap.Difference.Kind.Deleted)
  (* This `of_alist_exn` won't raise because build map never hold duplicated artifact paths. *)
  |> BuildMap.Difference.of_alist_exn


let compute_difference_from_removed_paths ~source_root ~build_map_index removed_paths =
  to_relative_paths ~root:source_root removed_paths
  |> compute_difference_from_removed_relative_paths ~build_map_index


let compute_difference_from_changed_relative_paths ~build_map_index changed_paths =
  List.concat_map changed_paths ~f:(fun source_path ->
      BuildMap.Indexed.lookup_artifact build_map_index source_path
      |> List.map ~f:(fun artifact_path ->
             artifact_path, BuildMap.Difference.Kind.Changed source_path))
  (* This `of_alist_exn` won't raise because build map never hold duplicated artifact paths. *)
  |> BuildMap.Difference.of_alist_exn


let compute_difference_from_changed_paths ~source_root ~buck_options ~targets changed_paths =
  let open Lwt.Infix in
  try
    Log.info "Running `buck query`...";
    query_buck_for_changed_targets ~targets buck_options changed_paths
    >>= fun query_output ->
    let changed_targets = parse_buck_changed_targets_query_output query_output in
    Log.info "Constructing local build map for changed files...";
    match BuckChangedTargetsQueryOutput.to_build_map_batch changed_targets with
    | Result.Error _ as error -> Lwt.return error
    | Result.Ok build_map ->
        to_relative_paths ~root:source_root changed_paths
        |> compute_difference_from_changed_relative_paths
             ~build_map_index:(BuildMap.index build_map)
        |> Lwt.return_ok
  with
  | JsonError message -> Lwt.return_error message
  | Raw.BuckError { description; _ } ->
      let message = Format.sprintf "Buck query failed: %s" description in
      Lwt.return_error message


let build_map_and_difference_from_paths
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    { buck_options; source_root; _ }
  =
  let open Lwt.Infix in
  Log.info "Computing build map deltas from changed paths...";
  compute_difference_from_changed_paths ~source_root ~buck_options ~targets changed_paths
  >>= function
  | Result.Error _ as error -> Lwt.return error
  | Result.Ok difference_from_changed_paths -> (
      Log.info "Computing build map deltas from removed paths...";
      let difference_from_removed_paths =
        compute_difference_from_removed_paths
          ~source_root
          ~build_map_index:old_build_map_index
          removed_paths
      in
      Log.info "Merging build map deltas...";
      match
        BuildMap.Difference.merge difference_from_changed_paths difference_from_removed_paths
      with
      | Result.Error artifact_path ->
          Format.sprintf "Conflicting source updates on artifact `%s`" artifact_path
          |> Lwt.return_error
      | Result.Ok difference -> (
          Log.info "Updating old build map...";
          match BuildMap.strict_apply_difference ~difference old_build_map with
          | Result.Ok build_map -> Lwt.return_ok (build_map, difference)
          | Result.Error artifact_path ->
              Format.sprintf "Cannot determine source path for artifact `%s`" artifact_path
              |> Lwt.return_error))


let fast_incremental_build_with_normalized_targets
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    ({ source_root; artifact_root; _ } as builder)
  =
  let open Lwt.Infix in
  Log.info "Attempting to perform fast incremental rebuild...";
  build_map_and_difference_from_paths
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    builder
  >>= function
  | Result.Error message ->
      Log.info "Fast incremental rebuild failed: %s. Falling back to the slow path..." message;
      incremental_build_with_normalized_targets ~old_build_map ~targets builder
  | Result.Ok (build_map, difference) ->
      let open Lwt.Infix in
      update_artifacts ~source_root ~artifact_root difference
      >>= fun changed_artifacts ->
      Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let incremental_build_with_unchanged_build_map
    ~build_map
    ~build_map_index
    ~targets
    ~changed_sources
    { source_root; artifact_root; _ }
  =
  let changed_artifacts =
    to_relative_paths ~root:source_root changed_sources
    |> List.concat_map ~f:(BuildMap.Indexed.lookup_artifact build_map_index)
    |> List.map ~f:(fun relative -> Path.create_relative ~root:artifact_root ~relative)
  in
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let do_lookup_source ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:artifact_root path with
  | None -> None
  | Some relative_artifact_path ->
      BuildMap.Indexed.lookup_source index relative_artifact_path
      |> Option.map ~f:(fun relative -> Path.create_relative ~root:source_root ~relative)


let lookup_source ~index ~builder:{ source_root; artifact_root; _ } path =
  do_lookup_source ~index ~source_root ~artifact_root path


let do_lookup_artifact ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:source_root path with
  | None -> []
  | Some relative_source_path ->
      BuildMap.Indexed.lookup_artifact index relative_source_path
      |> List.map ~f:(fun relative -> Path.create_relative ~root:artifact_root ~relative)


let lookup_artifact ~index ~builder:{ source_root; artifact_root; _ } path =
  do_lookup_artifact ~index ~source_root ~artifact_root path


let cleanup { artifact_root; _ } =
  match Path.remove_contents_of_directory artifact_root with
  | Result.Error message -> Log.warning "Encountered error during buck builder cleanup: %s" message
  | Result.Ok () -> ()
