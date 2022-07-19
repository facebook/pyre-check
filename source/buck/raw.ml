(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module ArgumentList = struct
  type t = string list [@@deriving sexp_of]

  let to_buck_command ~buck_command arguments =
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
    String.concat ~sep:" " (buck_command :: escaped_arguments)


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
    buck_command: string;
    arguments: ArgumentList.t;
    description: string;
    exit_code: int option;
    additional_logs: string list;
  }
[@@deriving sexp_of]

type t = {
  query: ?mode:string -> ?isolation_prefix:string -> string list -> string Lwt.t;
  build: ?mode:string -> ?isolation_prefix:string -> string list -> string Lwt.t;
}

let create_for_testing ~query ~build () = { query; build }

let isolation_prefix_to_buck_arguments = function
  | None
  | Some "" ->
      []
  | Some isolation_prefix -> ["--isolation_prefix"; isolation_prefix]


let isolation_prefix_to_buck2_arguments = function
  | None
  | Some "" ->
      []
  | Some isolation_prefix ->
      (* Consistent directory location with Buck v1 *)
      ["--isolation-dir"; Format.sprintf "%s-buck-out" isolation_prefix]


let mode_to_buck_arguments = function
  | None -> []
  | Some mode -> [mode]


let consume_stderr ~log_buffer stderr_channel =
  let open Lwt.Infix in
  (* Forward the buck progress message from subprocess stderr to our users, so they get a sense of
     what is being done under the hood. *)
  let rec consume_line channel =
    Lwt_io.read_line_opt channel
    >>= function
    | None ->
        (* We don't care what's returned here since we'll ignore `Completed.stderr` later. *)
        Lwt.return ""
    | Some line ->
        Log.info "[Buck] %s" line;
        BoundedQueue.add log_buffer ~item:line;
        consume_line channel
  in
  consume_line stderr_channel


let on_completion ~buck_command ~arguments ~log_buffer = function
  | { LwtSubprocess.Completed.status; stdout; _ } -> (
      Log.debug "buck command finished";
      let fail_with_error ?exit_code description =
        Lwt.fail
          (BuckError
             {
               buck_command;
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


let create ?(additional_log_size = 0) () =
  let buck1 = "buck1" in
  let open Lwt.Infix in
  let invoke_buck ?mode ?isolation_prefix ~command arguments =
    command :: arguments
    |> Core.List.map ~f:(Format.sprintf "'%s'")
    |> Core.String.concat ~sep:" "
    |> Log.debug "Running buck command: buck %s";
    (* Preserve the last several lines of Buck log for error reporting purpose. *)
    let log_buffer = BoundedQueue.create additional_log_size in
    (* Sometimes the total length of buck cli arguments can go beyond the limit of the underlying
       operating system. Pass all the arguments via a temporary file instead. *)
    Lwt_io.with_temp_file ~prefix:"buck_arguments_" (fun (filename, output_channel) ->
        Lwt_io.write_lines output_channel (Lwt_stream.of_list arguments)
        >>= fun () ->
        Lwt_io.flush output_channel
        >>= fun () ->
        let arguments =
          List.concat
            [
              isolation_prefix_to_buck_arguments isolation_prefix;
              [command];
              mode_to_buck_arguments mode;
              [Format.sprintf "@%s" filename];
            ]
        in
        let consume_stderr = consume_stderr ~log_buffer in
        LwtSubprocess.run buck1 ~arguments ~consume_stderr)
    >>= fun result ->
    let arguments =
      List.concat
        [
          isolation_prefix_to_buck_arguments isolation_prefix;
          [command];
          mode_to_buck_arguments mode;
          arguments;
        ]
    in
    on_completion ~buck_command:buck1 ~arguments ~log_buffer result
  in
  let query ?mode ?isolation_prefix arguments =
    invoke_buck ?mode ?isolation_prefix ~command:"query" arguments
  in
  let build ?mode ?isolation_prefix arguments =
    invoke_buck ?mode ?isolation_prefix ~command:"build" arguments
  in
  { query; build }


let create_v2 ?(additional_log_size = 0) () =
  let buck2 = "buck2" in
  let open Lwt.Infix in
  let invoke_buck ?mode ?isolation_prefix ~command arguments =
    command :: arguments
    |> Core.List.map ~f:(Format.sprintf "'%s'")
    |> Core.String.concat ~sep:" "
    |> Log.debug "Running buck2 command: buck2 %s";
    (* Preserve the last several lines of Buck log for error reporting purpose. *)
    let log_buffer = BoundedQueue.create additional_log_size in
    (* Sometimes the total length of buck cli arguments can go beyond the limit of the underlying
       operating system. Pass all the arguments via a temporary file instead. *)
    Lwt_io.with_temp_file ~prefix:"buck_arguments_" (fun (filename, output_channel) ->
        Lwt_io.write_lines output_channel (Lwt_stream.of_list arguments)
        >>= fun () ->
        Lwt_io.flush output_channel
        >>= fun () ->
        let arguments =
          List.concat
            [
              isolation_prefix_to_buck2_arguments isolation_prefix;
              [command];
              mode_to_buck_arguments mode;
              [Format.sprintf "@%s" filename];
            ]
        in
        let consume_stderr = consume_stderr ~log_buffer in
        LwtSubprocess.run buck2 ~arguments ~consume_stderr)
    >>= fun result ->
    let arguments =
      List.concat
        [
          isolation_prefix_to_buck2_arguments isolation_prefix;
          [command];
          mode_to_buck_arguments mode;
          arguments;
        ]
    in
    on_completion ~buck_command:buck2 ~arguments ~log_buffer result
  in
  let query ?mode ?isolation_prefix arguments =
    invoke_buck ?mode ?isolation_prefix ~command:"query" arguments
  in
  let build ?mode ?isolation_prefix arguments =
    invoke_buck ?mode ?isolation_prefix ~command:"build" arguments
  in
  { query; build }


let query { query; _ } = query

let build { build; _ } = build
