(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Ast
open Analysis

module Output = struct
  type type_at_location = {
    location: Location.t;
    annotation: Analysis.Type.t;
  }
  [@@deriving equal, to_yojson]

  type types_at_path = {
    path: string;
    types: type_at_location list;
  }
  [@@deriving equal, to_yojson]

  type t = types_at_path list [@@deriving to_yojson]
end

let create_environment_controls ~root ~python_version ~system_platform () =
  (* TODO: Rename `log_directory` to `saved_state_directory` in Configuration.Analysis *)
  let log_directory = Stdlib.Filename.get_temp_dir_name () in
  let configuration =
    Configuration.Analysis.create
      ~parallel:false
      ~analyze_external_sources:false
      ~filter_directories:[]
      ~ignore_all_errors:[]
      ~number_of_workers:1
      ~local_root:root
      ~project_root:root
      ~search_paths:[]
      ~taint_model_paths:[]
      ~strict:false
      ~debug:false
      ~show_error_traces:false
      ~excludes:[]
      ~extensions:[]
      ~store_type_check_resolution:false
      ~store_type_errors:true
      ~track_dependencies:false
      ~log_directory
      ~python_version
      ~system_platform
      ~enable_type_comments:true
      ~enable_readonly_analysis:false
      ~enable_strict_override_check:false
      ~enable_unawaited_awaitable_analysis:true
      ~include_suppressed_errors:false
      ~source_paths:[]
      ()
  in
  (* This is needed to initiailze sharedmem *)
  Memory.initialize configuration;
  Analysis.EnvironmentControls.create ~populate_call_graph:false configuration


let compute_types_at_location ~qualifier ~type_environment =
  let lookup = LocationBasedLookup.ExpressionTypes.create_of_module type_environment qualifier in
  let nodes_and_coverage_data =
    LocationBasedLookup.ExpressionLevelCoverage.get_all_nodes_and_coverage_data lookup
  in
  List.map
    nodes_and_coverage_data
    ~f:(fun (location, { LocationBasedLookup.ExpressionTypes.type_; expression = _ }) ->
      { Output.location; annotation = type_ })


let compute_types_at_paths
    ~lookup:{ Sourcedb.Lookup.get_source; _ }
    ~type_environment
    ~buck_based_source_code_api
  =
  let qualifiers = BuckBasedSourceCodeApi.get_type_check_qualifiers buck_based_source_code_api in
  let source_code_api = BuckBasedSourceCodeApi.get_source_code_api buck_based_source_code_api in
  let get_types_at_qualifier qualifier =
    let open Option.Monad_infix in
    Analysis.SourceCodeApi.module_path_of_qualifier source_code_api qualifier
    >>= fun module_path ->
    Ast.ModulePath.relative module_path
    |> get_source
    >>= fun path ->
    Some { Output.path; types = compute_types_at_location ~qualifier ~type_environment }
  in
  List.filter_map qualifiers ~f:get_types_at_qualifier


let print_types_at_paths ~output ~types_at_paths =
  let json = Output.to_yojson types_at_paths in
  match output with
  | None -> Log.print "%s" (Yojson.Safe.to_string json)
  | Some output -> Yojson.Safe.to_file output json


let produce_types
    ~root
    ~output
    { CheckCommandInput.get_source_db; get_python_version; get_system_platform }
  =
  let python_version = get_python_version () in
  let system_platform = get_system_platform () in
  let controls = create_environment_controls ~root ~python_version ~system_platform () in
  let { Sourcedb.lookup; listing } = get_source_db () in
  let buck_based_source_code_api =
    let loader = FileLoader.create_from_sourcedb_lookup ~root lookup in
    BuckBasedSourceCodeApi.create ~controls ~loader ~listing ()
  in
  let type_environment =
    let source_code_environment =
      BuckBasedSourceCodeApi.get_source_code_incremental_api buck_based_source_code_api
      |> SourceCodeEnvironment.of_source_code_base
    in
    TypeEnvironment.create source_code_environment |> TypeEnvironment.read_only
  in
  let types_at_paths =
    compute_types_at_paths ~lookup ~type_environment ~buck_based_source_code_api
  in
  print_types_at_paths ~output ~types_at_paths


let get_error_message = function
  | CheckCommandInput.Error.FileReadError { path; message }
  | CheckCommandInput.Error.ManifestError (Manifest.Error.FileReadError { path; message }) ->
      Stdlib.Format.asprintf "Cannot read file `%a`: %s" PyrePath.pp path message
  | CheckCommandInput.Error.JsonParseError { path; message }
  | CheckCommandInput.Error.ManifestError (Manifest.Error.JsonParseError { path; message }) ->
      let filename =
        Option.value_map path ~default:"" ~f:(Stdlib.Format.asprintf "file `%a`" PyrePath.pp)
      in
      Stdlib.Format.sprintf "Cannot parse JSON %s: %s" filename message
  | CheckCommandInput.Error.JsonFormatError { path; message }
  | CheckCommandInput.Error.ManifestError (Manifest.Error.JsonFormatError { path; message }) ->
      let filename =
        Option.value_map path ~default:"" ~f:(Stdlib.Format.asprintf "file `%a`" PyrePath.pp)
      in
      Stdlib.Format.sprintf "Wrong JSON format %s: %s" filename message
  | CheckCommandInput.Error.VersionFormatError { py_version; message } ->
      Stdlib.Format.sprintf "Cannot parse py_version string `%s`: %s" py_version message


let run_glean_command input_argument_file output_file =
  Log.info "Loading argument file `%s`..." input_argument_file;
  match
    PyrePath.create_absolute input_argument_file |> CheckCommandInput.create_from_argument_file
  with
  | Result.Error error -> `Error (false, get_error_message error)
  | Result.Ok input_arguments ->
      Log.info "Input argument file loaded";
      Option.iter output_file ~f:(Log.info "Output will be written into %s");
      let root = PyrePath.current_working_directory () in
      let () = produce_types ~root ~output:output_file input_arguments in
      `Ok ()


let command () =
  let open Cmdliner in
  let filename = Arg.(required & pos 0 (some file) None & info [] ~docv:"filename") in
  let output =
    Arg.(
      value
      & opt (some string) None
      & info
          ["o"; "output"]
          ~docv:"output"
          ~doc:"If specified, write output to this file instead of stdout")
  in
  let term = Term.(const run_glean_command $ filename $ output |> ret) in
  let info =
    Cmd.info
      "glean"
      ~doc:
        "returns a map from targets sources to a list of all types for that source to be used by \
         glean."
  in
  Cmd.v info term
