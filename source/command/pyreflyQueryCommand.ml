(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let output_query_response ~output_file ~content =
  match output_file with
  | Some path ->
      let out_channel = Out_channel.create ~append:false path in
      Out_channel.output_string out_channel content;
      Out_channel.output_string out_channel "\n";
      Out_channel.close out_channel
  | None -> Log.info "%s\n%!" content


let run_pyrefly_query ~pyrefly_results ~query ~configuration ~scheduler =
  let create_pyre_api () =
    pyrefly_results
    |> PyrePath.create_absolute
    |> Interprocedural.PyreflyApi.ReadWrite.create_from_directory
         ~scheduler
         ~scheduler_policies:Configuration.SchedulerPolicies.empty
         ~configuration
    |> Interprocedural.PyreflyApi.ReadOnly.of_read_write_api
    |> Interprocedural.PyrePysaApi.ReadOnly.from_pyrefly_api
  in
  match Server.Query.parse_request query with
  | Result.Error message -> Server.Query.Response.Error message
  | Result.Ok (ModelQuery { path; query_name }) ->
      Server.Query.process_model_query
        ~pyre_api:(create_pyre_api ())
        ~scheduler
        ~configuration
        ~path
        ~query_name
  | Result.Ok (ValidateTaintModels { path; verify_dsl }) ->
      Server.Query.process_validate_taint_models
        ~pyre_api:(create_pyre_api ())
        ~scheduler
        ~configuration
        ~path
        ~verify_dsl
  | Result.Ok _ -> Server.Query.Response.Error (Format.asprintf "Unsupported query: `%s`" query)


let run_command query output_file configuration_file =
  match
    CommandStartup.read_and_parse_json
      configuration_file
      ~f:AnalyzeCommand.AnalyzeConfiguration.of_yojson
  with
  | Result.Error message ->
      Log.error "%s" message;
      (* Be consistent with the exit code of pyre server when running `pyre query` *)
      ServerCommand.ExitStatus.Error |> ServerCommand.ExitStatus.exit_code |> exit
  | Result.Ok { base; taint_model_paths; strict; pyrefly_results; _ } ->
      AnalyzeCommand.setup_global_states base;
      let configuration =
        AnalyzeCommand.analysis_configuration_of
          ~taint_model_paths
          ~strict
          ~use_pyrefly_results:true
          base
      in
      let pyrefly_results =
        match pyrefly_results with
        | Some path -> PyrePath.absolute path
        | None -> failwith "`pyrefly_results` must be set in the configuration file"
      in
      let query_response =
        Scheduler.with_scheduler
          ~configuration
          ~should_log_exception:(fun _ -> false)
          ~f:(fun scheduler -> run_pyrefly_query ~pyrefly_results ~query ~configuration ~scheduler)
      in
      let json = Server.Query.Response.to_yojson query_response in
      output_query_response ~output_file ~content:(Yojson.Safe.pretty_to_string json);
      (* Be consistent with the exit code of pyre server when running `pyre query` *)
      ServerCommand.ExitStatus.Ok |> ServerCommand.ExitStatus.exit_code |> exit


let doc = "Run a query using Pyrefly results"

let command () =
  let open Cmdliner in
  let query = Arg.(required & opt (some string) None & info ["query"] ~doc:"Query to run") in
  let configuration_file =
    Arg.(
      required
      & opt (some string) None
      & info ["configuration-file"] ~doc:"Path to the configuration file")
  in
  let output_file =
    Arg.(
      value & opt (some string) None & info ["output-file"] ~doc:"Optional path to write output to")
  in
  let term = Term.(const run_command $ query $ output_file $ configuration_file) in
  let info = Cmd.info "pyrefly-query" ~doc in
  Cmd.v info term
