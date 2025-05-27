(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

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
      ~enable_strict_any_check:false
      ~enable_unawaited_awaitable_analysis:true
      ~include_suppressed_errors:false
      ~source_paths:[]
      ()
  in
  (* This is needed to initiailze sharedmem *)
  Memory.initialize configuration;
  Analysis.EnvironmentControls.create ~populate_call_graph:false configuration


let instantiate_error ~source_code_api ~lookup:{ Sourcedb.Lookup.get_source; _ } error =
  let lookup module_name =
    let open Option.Monad_infix in
    Analysis.SourceCodeApi.module_path_of_qualifier source_code_api module_name
    >>= fun module_path ->
    (* Type errors are only expected in sources not dependencies so we only need to lookup the
       mappings for the former *)
    Ast.ModulePath.relative module_path |> get_source
  in
  Analysis.AnalysisError.instantiate ~show_error_traces:false ~lookup error


let compute_errors buck_based_source_code_api =
  let open Analysis in
  let errors_environment =
    let source_code_environment =
      BuckBasedSourceCodeApi.get_source_code_incremental_api buck_based_source_code_api
      |> SourceCodeEnvironment.of_source_code_base
    in
    ErrorsEnvironment.create source_code_environment |> ErrorsEnvironment.read_only
  in
  let type_check_qualifiers =
    BuckBasedSourceCodeApi.get_type_check_qualifiers buck_based_source_code_api
  in
  Log.info "Type checking %d modules..." (List.length type_check_qualifiers);
  let errors =
    ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers errors_environment type_check_qualifiers
  in
  Log.info "Reporting %d type errors..." (List.length errors);
  List.sort errors ~compare:AnalysisError.compare


let print_errors ~output errors =
  let json =
    `Assoc
      [
        ( "errors",
          `List
            (List.map ~f:(fun error -> Analysis.AnalysisError.Instantiated.to_yojson error) errors)
        );
      ]
  in
  match output with
  | None -> Log.print "%s" (Yojson.Safe.to_string json)
  | Some output -> Yojson.Safe.to_file output json


let run_check
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
  let errors =
    let uninstantiated_errors = compute_errors buck_based_source_code_api in
    let source_code_api = BuckBasedSourceCodeApi.get_source_code_api buck_based_source_code_api in
    List.map uninstantiated_errors ~f:(instantiate_error ~source_code_api ~lookup)
  in
  print_errors ~output errors


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


let run_check_command input_argument_file output_file =
  Log.info "Loading argument file `%s`..." input_argument_file;
  match
    PyrePath.create_absolute input_argument_file |> CheckCommandInput.create_from_argument_file
  with
  | Result.Error error -> `Error (false, get_error_message error)
  | Result.Ok input_arguments ->
      Log.info "Input argument file loaded";
      Option.iter output_file ~f:(Log.info "Output will be written into %s");
      let root = PyrePath.current_working_directory () in
      let () = run_check ~root ~output:output_file input_arguments in
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
  let term = Term.(const run_check_command $ filename $ output |> ret) in
  let info = Cmd.info "check" ~doc:"runs a full check" in
  Cmd.v info term
