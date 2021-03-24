(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Path = Pyre.Path

exception JsonError of string

let source_database_suffix = "#source-db"

type t = {
  raw: Raw.t;
  mode: string option;
  isolation_prefix: string option;
}

let create ?mode ?isolation_prefix raw = { raw; mode; isolation_prefix }

let query_buck_for_targets { raw; mode; isolation_prefix } target_specifications =
  match target_specifications with
  | [] -> Lwt.return "{}"
  | _ ->
      List.concat
        [
          (* Force `buck` to hand back structured JSON output instead of plain text. *)
          ["--json"];
          (* Mark the query as coming from `pyre` for `buck`, to make troubleshooting easier. *)
          ["--config"; "client.id=pyre"];
          Option.value_map isolation_prefix ~default:[] ~f:(fun isolation_prefix ->
              ["--isolation_prefix"; isolation_prefix]);
          Option.value_map mode ~default:[] ~f:(fun mode -> ["@mode/" ^ mode]);
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
      |> Raw.query raw


let run_buck_build_for_targets { raw; isolation_prefix; _ } targets =
  match targets with
  | [] -> Lwt.return "{}"
  | _ ->
      List.concat
        [
          (* Force `buck` to hand back structured JSON output instead of plain text. *)
          ["--show-full-json-output"];
          (* Mark the query as coming from `pyre` for `buck`, to make troubleshooting easier. *)
          ["--config"; "client.id=pyre"];
          Option.value_map isolation_prefix ~default:[] ~f:(fun isolation_prefix ->
              ["--isolation_prefix"; isolation_prefix]);
          List.map targets ~f:(fun target ->
              Format.sprintf "%s%s" (Target.show target) source_database_suffix);
        ]
      |> Raw.build raw


let parse_buck_query_output query_output =
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


let load_partial_build_map path =
  let open Lwt.Infix in
  let path = Path.absolute path in
  Lwt_io.(with_file ~mode:Input path read)
  >>= fun content ->
  try
    match BuildMap.Partial.of_json (Yojson.Safe.from_string ~fname:path content) with
    | Result.Error message -> raise (JsonError message)
    | Result.Ok build_map -> Lwt.return build_map
  with
  | Yojson.Json_error message -> raise (JsonError message)


let normalize_targets builder target_specifications =
  let open Lwt.Infix in
  Log.info "Collecting buck targets to build...";
  query_buck_for_targets builder target_specifications
  >>= fun query_output ->
  let targets = parse_buck_query_output query_output |> List.map ~f:Target.of_string in
  Log.info "Collected %d targets" (List.length targets);
  Lwt.return targets


let build_source_databases builder targets =
  let open Lwt.Infix in
  Log.info "Building Buck source databases...";
  run_buck_build_for_targets builder targets
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
  let merge (targets_sofar, build_map_sofar) (next_target, next_build_map) =
    let open BuildMap.Partial in
    match merge build_map_sofar next_build_map with
    | MergeResult.Incompatible { MergeResult.IncompatibleItem.key; left_value; right_value } ->
        Log.warning
          "Cannot include target %s for type checking: file `%s` has already been mapped to `%s` \
           but the target maps it to `%s` instead. "
          (Target.show next_target)
          key
          left_value
          right_value;
        targets_sofar, build_map_sofar
    | MergeResult.Ok merged_build_map -> next_target :: targets_sofar, merged_build_map
  in
  let reversed_targets, merged_build_map =
    List.fold target_and_build_maps ~init:([], BuildMap.Partial.empty) ~f:merge
  in
  List.rev reversed_targets, BuildMap.create merged_build_map


let load_and_merge_source_databases target_and_source_database_paths =
  let open Lwt.Infix in
  load_build_maps target_and_source_database_paths
  >>= fun target_and_build_maps -> Lwt.return (merge_build_maps target_and_build_maps)
