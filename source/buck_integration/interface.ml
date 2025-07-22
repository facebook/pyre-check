(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module implements the logic to call out to buck as needed for classic Pyre daemons. The
   primary action we take is to build source-db targets, which produces a json map from source paths
   (mostly user-controlled source code, but also some buck-built generated code, for example thrift
   stubs) to artifact paths. *)

open Base

exception JsonError of string

module BuckOptions = struct
  type 'raw t = {
    raw: 'raw;
    mode: string option;
    isolation_prefix: string option;
  }
end

module BuildResult = struct
  type t = {
    build_map: BuildMap.t;
    targets: BuckTarget.t list;
  }
end

module WithMetadata = struct
  type ('data, 'metadata) t = {
    data: 'data;
    metadata: 'metadata option;
  }

  let create ?metadata data = { data; metadata }
end

module Eager = struct
  type t = { construct_build_map: string list -> (BuildMap.t, string) WithMetadata.t Lwt.t }

  let create_for_testing ~construct_build_map () = { construct_build_map }

  let build_map_key = "build_map"

  let built_targets_count_key = "built_targets_count"

  let dropped_targets_key = "dropped_targets"

  module BuckBxlBuilderOutput = struct
    module Conflict = struct
      type t = {
        conflict_with: string;
        artifact_path: string;
        preserved_source_path: string;
        dropped_source_path: string;
      }
      [@@deriving sexp, compare, of_yojson { strict = false }]
    end

    type t = {
      build_map: BuildMap.t;
      target_count: int;
      conflicts: (BuckTarget.t * Conflict.t) list;
    }
  end

  let parse_merged_sourcedb merged_sourcedb : BuckBxlBuilderOutput.t =
    let open Yojson.Safe in
    try
      let build_map =
        Util.member build_map_key merged_sourcedb
        |> BuildMap.Partial.of_json_exn_ignoring_duplicates_no_dependency
        |> BuildMap.create
      in
      let target_count = Util.member built_targets_count_key merged_sourcedb |> Util.to_int in
      let conflicts =
        let conflict_of_yojson json =
          match BuckBxlBuilderOutput.Conflict.of_yojson json with
          | Result.Ok conflict -> conflict
          | Result.Error message ->
              let message = Stdlib.Format.sprintf "Cannot parse conflict item: %s" message in
              raise (JsonError message)
        in
        Util.member dropped_targets_key merged_sourcedb
        |> Util.to_assoc
        |> List.map ~f:(fun (target, conflict_json) ->
               BuckTarget.of_string target, conflict_of_yojson conflict_json)
      in
      { BuckBxlBuilderOutput.build_map; target_count; conflicts }
    with
    | (Yojson.Json_error message | Util.Type_error (message, _)) as exn ->
        let exn = Exception.wrap exn in
        Exception.raise_with_backtrace (JsonError message) exn


  let parse_merged_sourcedb_path_json bxl_output =
    let open Yojson.Safe in
    try from_string ~fname:"buck bxl output" bxl_output |> Util.member "db" |> Util.to_string with
    | (Yojson.Json_error message | Util.Type_error (message, _)) as e ->
        let exn = Exception.wrap e in
        let message = Stdlib.Format.sprintf "Error parsing BXL output: %s" message in
        Exception.raise_with_backtrace (JsonError message) exn


  let parse_merged_sourcedb_json merged_sourcedb_path =
    let open Yojson.Safe in
    try from_file merged_sourcedb_path with
    | (Yojson.Json_error message | Util.Type_error (message, _) | Sys_error message) as e ->
        let exn = Exception.wrap e in
        let message =
          Stdlib.Format.sprintf
            "Error parsing merged sourcedb at `%s`: %s"
            merged_sourcedb_path
            message
        in
        Exception.raise_with_backtrace (JsonError message) exn


  let parse_bxl_output bxl_output =
    parse_merged_sourcedb_path_json bxl_output
    |> parse_merged_sourcedb_json
    |> parse_merged_sourcedb


  let run_bxl_for_targets
      ~bxl_builder
      ~buck_options:{ BuckOptions.raw; mode; isolation_prefix; _ }
      ~number_of_threads
      target_patterns
    =
    match target_patterns with
    | [] ->
        `Assoc
          [
            build_map_key, `Assoc [];
            built_targets_count_key, `Assoc [];
            dropped_targets_key, `Assoc [];
          ]
        |> Yojson.Safe.to_string
        |> Raw.Command.Output.create
        |> Lwt.return
    | _ ->
        let number_of_threads =
          match number_of_threads with
          | Some number_of_threads -> ["--num-threads"; Int.to_string number_of_threads]
          | None -> []
        in
        List.concat
          [
            number_of_threads;
            (* Location of the BXL builder. *)
            [bxl_builder];
            (* Force `buck` to opt-out fancy tui logging. *)
            ["--console=simple"];
            ["--"];
            List.bind target_patterns ~f:(fun target ->
                ["--target"; Stdlib.Format.sprintf "%s" target]);
          ]
        |> Raw.bxl ?mode ?isolation_prefix raw


  let optionally_kill_buck
      ~kill_buck_after_build
      ~buck_options:{ BuckOptions.raw; mode; isolation_prefix; _ }
    =
    if kill_buck_after_build then
      let () = Log.info "Killing Buck..." in
      Raw.kill ?mode ?isolation_prefix raw []
    else
      Lwt.return (Raw.Command.Output.create "Did not kill Buck")


  let warn_on_conflict
      ~target
      {
        BuckBxlBuilderOutput.Conflict.conflict_with;
        artifact_path;
        preserved_source_path;
        dropped_source_path;
      }
    =
    Log.warning "Cannot include target for type checking: %s" (BuckTarget.show target);
    Log.info
      "... file `%s` has already been mapped to `%s` by `%s` but the target maps it to `%s` \
       instead. "
      artifact_path
      preserved_source_path
      (BuckTarget.show conflict_with)
      dropped_source_path;
    ()


  let warn_on_conflicts = function
    | [] -> ()
    | conflicts ->
        List.iter conflicts ~f:(fun (target, conflict) -> warn_on_conflict ~target conflict);
        Log.warning
          "One or more targets get dropped by Pyre due to potential conflicts. For more details, \
           see https://fburl.com/pyre-target-conflict"


  let construct_build_map_with_options
      ~bxl_builder
      ~buck_options
      ~kill_buck_after_build
      ~number_of_threads
      target_patterns
    =
    let open Lwt.Infix in
    Log.info "Building Buck source databases...";
    run_bxl_for_targets ~bxl_builder ~buck_options ~number_of_threads target_patterns
    >>= fun { Raw.Command.Output.stdout; build_id } ->
    let { BuckBxlBuilderOutput.build_map; target_count; conflicts } = parse_bxl_output stdout in
    warn_on_conflicts conflicts;
    Log.info "Loaded source databases for %d targets" target_count;
    optionally_kill_buck ~kill_buck_after_build ~buck_options
    >>= fun _ -> Lwt.return (WithMetadata.create ?metadata:build_id build_map)


  let create ?mode ?isolation_prefix ?bxl_builder ~kill_buck_after_build ~number_of_threads raw =
    let buck_options = { BuckOptions.mode; isolation_prefix; raw } in
    match bxl_builder with
    | None -> failwith "BXL path is not set but it is required when using Buck2"
    | Some bxl_builder ->
        {
          construct_build_map =
            construct_build_map_with_options
              ~bxl_builder
              ~buck_options
              ~kill_buck_after_build
              ~number_of_threads;
        }


  let construct_build_map { construct_build_map } target_patterns =
    construct_build_map target_patterns
end

module Lazy = struct
  type t = { construct_build_map: string list -> BuildMap.t Lwt.t }

  let create_for_testing ~construct_build_map () = { construct_build_map }

  let parse_merged_sourcedb merged_sourcedb : BuildMap.t =
    let open Yojson.Safe in
    try
      BuildMap.Partial.of_json_exn_ignoring_duplicates_no_dependency merged_sourcedb
      |> BuildMap.create
    with
    | (Yojson.Json_error message | Util.Type_error (message, _)) as e ->
        let exn = Exception.wrap e in
        Exception.raise_with_backtrace (JsonError message) exn


  let parse_bxl_output bxl_output =
    Eager.parse_merged_sourcedb_path_json bxl_output
    |> Eager.parse_merged_sourcedb_json
    |> parse_merged_sourcedb


  let run_bxl_for_targets
      ~bxl_builder
      ~buck_options:{ BuckOptions.raw; mode; isolation_prefix; _ }
      target_patterns
    =
    match target_patterns with
    | [] -> Lwt.return BuildMap.(Partial.empty |> create)
    | _ ->
        let open Lwt.Infix in
        List.concat
          [
            (* Location of the BXL builder. *)
            [bxl_builder];
            (* Force `buck` to opt-out fancy tui logging. *)
            ["--console=simple"];
            ["--"];
            List.bind target_patterns ~f:(fun source_path -> ["--source"; source_path]);
          ]
        |> Raw.bxl ?mode ?isolation_prefix raw
        >>= fun { Raw.Command.Output.stdout; _ } -> Lwt.return (parse_bxl_output stdout)


  let construct_build_map_with_options ~bxl_builder ~buck_options source_paths =
    let open Lwt.Infix in
    Log.info "Building Buck source databases for %d sources..." (List.length source_paths);
    run_bxl_for_targets ~bxl_builder ~buck_options source_paths
    >>= fun build_map ->
    Log.info "Loaded source databases";
    Lwt.return build_map


  let create ?mode ?isolation_prefix ~bxl_builder raw =
    let buck_options = { BuckOptions.mode; isolation_prefix; raw } in
    { construct_build_map = construct_build_map_with_options ~bxl_builder ~buck_options }


  let construct_build_map { construct_build_map; _ } source_paths = construct_build_map source_paths
end
