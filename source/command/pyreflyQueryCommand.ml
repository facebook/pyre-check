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
  match Server.Query.parse_request query with
  | Result.Error message -> Server.Query.Response.Error message
  | Result.Ok (ModelQuery { path; query_name }) ->
      let pyre_api =
        pyrefly_results
        |> PyrePath.create_absolute
        |> Interprocedural.PyreflyApi.ReadWrite.create_from_directory
             ~scheduler
             ~scheduler_policies:Configuration.SchedulerPolicies.empty
             ~configuration
        |> Interprocedural.PyreflyApi.ReadOnly.of_read_write_api
        |> Interprocedural.PyrePysaApi.ReadOnly.from_pyrefly_api
      in
      Server.Query.process_model_query ~pyre_api ~scheduler ~configuration ~path ~query_name
  | Result.Ok _ -> Server.Query.Response.Error (Format.asprintf "Unsupported query: `%s`" query)


let run_command pyrefly_results query output_file configuration_file =
  let query_response =
    match
      CommandStartup.read_and_parse_json
        configuration_file
        ~f:CommandStartup.BaseConfiguration.of_yojson
    with
    | Result.Error message -> Server.Query.Response.Error message
    | Result.Ok base_configuration ->
        AnalyzeCommand.setup_global_states base_configuration;
        let configuration =
          AnalyzeCommand.analysis_configuration_of
            ~taint_model_paths:[]
            ~strict:false
            ~use_pyrefly_results:true
            base_configuration
        in
        Scheduler.with_scheduler
          ~configuration
          ~should_log_exception:(fun _ -> false)
          ~f:(fun scheduler -> run_pyrefly_query ~pyrefly_results ~query ~configuration ~scheduler)
  in
  let json = Server.Query.Response.to_yojson query_response in
  output_query_response ~output_file ~content:(Yojson.Safe.pretty_to_string json);
  exit 0


let doc = "Run a query using Pyrefly results"

let command () =
  let open Cmdliner in
  let pyrefly_results =
    Arg.(
      required & opt (some string) None & info ["pyrefly-results"] ~doc:"Path to Pyrefly results")
  in
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
  let term =
    Term.(const run_command $ pyrefly_results $ query $ output_file $ configuration_file)
  in
  let info = Cmd.info "pyrefly-query" ~doc in
  Cmd.v info term
