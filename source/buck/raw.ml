(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module ArgumentList = struct
  type t = string list [@@deriving sexp_of]

  let to_buck_command arguments =
    let open Core in
    let escaped_arguments =
      let quote argument =
        if String.contains argument ' ' then
          (* This makes sure that the buck command gets properly escaped by the shell. *)
          Format.sprintf "'%s'" argument
        else
          argument
      in
      List.map arguments ~f:quote
    in
    String.concat ~sep:" " ("buck" :: escaped_arguments)


  let length = List.length
end

module BoundedQueue = struct
  type 'a t = {
    queue: 'a Queue.t;
    size: int;
  }

  let create size =
    if size < 0 then
      failwith "cannot have queue with negative size bound"
    else
      { queue = Queue.create ~capacity:size (); size }


  let add ~item { queue; size } =
    if size > 0 then (
      Queue.enqueue queue item;
      while Queue.length queue > size do
        Queue.dequeue queue |> ignore
      done)


  let collect { queue; _ } = Queue.to_list queue
end

exception
  BuckError of {
    arguments: ArgumentList.t;
    description: string;
    exit_code: int option;
    additional_logs: string list;
  }
[@@deriving sexp_of]

type t = {
  query: ?isolation_prefix:string -> string list -> string Lwt.t;
  build: ?isolation_prefix:string -> string list -> string Lwt.t;
}

let create_for_testing ~query ~build () = { query; build }

let isolation_prefix_to_buck_arguments = function
  | None
  | Some "" ->
      []
  | Some isolation_prefix -> ["--isolation_prefix"; isolation_prefix]


let create ?(additional_log_size = 0) () =
  let open Lwt.Infix in
  let invoke_buck ?isolation_prefix arguments =
    arguments
    |> Core.List.map ~f:(Format.asprintf "'%s'")
    |> Core.String.concat ~sep:" "
    |> Log.debug "Running buck command: buck %s";
    (* Preserve the last several lines of Buck log for error reporting purpose. *)
    let log_buffer = BoundedQueue.create additional_log_size in
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
            BoundedQueue.add log_buffer ~item:line;
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
        let arguments =
          List.append
            (isolation_prefix_to_buck_arguments isolation_prefix)
            [Format.sprintf "@%s" filename]
        in
        LwtSubprocess.run "buck" ~arguments ~consume_stderr)
    >>= function
    | { LwtSubprocess.Completed.status; stdout; _ } -> (
        Log.debug "buck command finished";
        let fail_with_error ?exit_code description =
          let arguments =
            List.append (isolation_prefix_to_buck_arguments isolation_prefix) arguments
          in
          Lwt.fail
            (BuckError
               {
                 arguments;
                 description;
                 exit_code;
                 additional_logs = BoundedQueue.collect log_buffer;
               })
        in
        match status with
        | Unix.WEXITED 0 -> Lwt.return stdout
        | WEXITED 127 ->
            let description = Format.sprintf "Cannot find buck exectuable under PATH." in
            fail_with_error ~exit_code:127 description
        | WEXITED code ->
            let description = Format.sprintf "Buck exited with code %d" code in
            fail_with_error ~exit_code:code description
        | WSIGNALED signal ->
            let description =
              Format.sprintf "Buck signaled with %s signal" (PrintSignal.string_of_signal signal)
            in
            fail_with_error description
        | WSTOPPED signal ->
            let description =
              Format.sprintf "Buck stopped with %s signal" (PrintSignal.string_of_signal signal)
            in
            fail_with_error description)
  in
  let query ?isolation_prefix arguments = invoke_buck ?isolation_prefix ("query" :: arguments) in
  let build ?isolation_prefix arguments = invoke_buck ?isolation_prefix ("build" :: arguments) in
  { query; build }


let query { query; _ } = query

let build { build; _ } = build
