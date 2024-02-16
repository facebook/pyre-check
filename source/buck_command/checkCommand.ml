(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

let doc = "Runs a full check"

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
  | Result.Ok _input_arguments ->
      Log.info "Input argument file loaded.";
      Option.iter output_file ~f:(Log.info "Output will be written into %s");
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
  let info = Cmd.info "check" ~doc in
  Cmd.v info term
