(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception
  BuckError of {
    arguments: string list;
    description: string;
  }

type t = {
  query: string list -> string Lwt.t;
  build: string list -> string Lwt.t;
}

let create_for_testing ~query ~build () = { query; build }

let create () =
  let open Lwt.Infix in
  let invoke_buck arguments =
    let consume_stderr stderr_channel =
      (* Forward the buck progress message from subprocess stderr to our users, so they get a sense
         of what is being done under the hood. *)
      let rec consume_line channel =
        Lwt_io.read_line_opt channel
        >>= function
        | None ->
            (* We don't care what's returned here since we'll ignore `Completed.stderr` a few lines
               later. *)
            Lwt.return ""
        | Some line ->
            Log.info "[Buck] %s" line;
            consume_line channel
      in
      consume_line stderr_channel
    in
    (* Sometimes the total length of buck cli arguments can go beyond the limit of the underlying
       operating system. Pass all the arguments via a temporary file instead. *)
    Lwt_io.with_temp_file ~prefix:"buck_arguments_" (fun (filename, output_channel) ->
        Lwt_io.write_lines output_channel (Lwt_stream.of_list arguments)
        >>= fun () ->
        Lwt_io.flush output_channel
        >>= fun () ->
        LwtSubprocess.run "buck" ~arguments:[Format.sprintf "@%s" filename] ~consume_stderr)
    >>= function
    | { LwtSubprocess.Completed.status; stdout; _ } -> (
        let fail_with_error description = Lwt.fail (BuckError { arguments; description }) in
        match status with
        | Unix.WEXITED 0 -> Lwt.return stdout
        | WEXITED 127 ->
            let description = Format.sprintf "Cannot find buck exectuable under PATH." in
            fail_with_error description
        | WEXITED code ->
            let description = Format.sprintf "Buck exited with code %d" code in
            fail_with_error description
        | WSIGNALED signal ->
            let description =
              Format.sprintf "Buck signaled with %s signal" (PrintSignal.string_of_signal signal)
            in
            fail_with_error description
        | WSTOPPED signal ->
            let description =
              Format.sprintf "Buck stopped with %s signal" (PrintSignal.string_of_signal signal)
            in
            fail_with_error description )
  in
  let query arguments = invoke_buck ("query" :: arguments) in
  let build arguments = invoke_buck ("build" :: arguments) in
  { query; build }


let query { query; _ } = query

let build { build; _ } = build
