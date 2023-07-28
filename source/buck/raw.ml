(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module contains low-level logic for constructing buck queries, including how to switch
   between buck1 and buck2 (which was needed to support gradual rollout of buck2). The actual
   queries we use for Pyre can be found in `interface.ml` *)

open Base
module PrintSignal = Hack_parallel.Std.PrintSignal

module ArgumentList = struct
  type t = string list [@@deriving sexp_of]

  let to_buck_command ~buck_command arguments = Stdlib.Filename.quote_command buck_command arguments

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

module Command = struct
  module Output = struct
    type t = {
      stdout: string;
      build_id: string option;
    }

    let create ?build_id stdout = { stdout; build_id }
  end

  type t = ?mode:string -> ?isolation_prefix:string -> string list -> Output.t Lwt.t
end

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
        Log.log ~section:`Progress "[Buck] %s" line;
        BoundedQueue.add log_buffer ~item:line;
        consume_line channel
  in
  consume_line stderr_channel


let on_completion ?build_id ~buck_command ~arguments ~log_buffer = function
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
      | Unix.WEXITED 0 -> Lwt.return (Command.Output.create ?build_id stdout)
      | Unix.WEXITED 127 ->
          let description = Stdlib.Format.sprintf "Cannot find buck exectuable under PATH." in
          fail_with_error ~exit_code:127 description
      | Unix.WEXITED code ->
          let description = Stdlib.Format.sprintf "Buck exited with code %d" code in
          fail_with_error ~exit_code:code description
      | Unix.WSIGNALED signal ->
          let description =
            Stdlib.Format.sprintf
              "Buck signaled with %s signal"
              (PrintSignal.string_of_signal signal)
          in
          fail_with_error description
      | Unix.WSTOPPED signal ->
          let description =
            Stdlib.Format.sprintf
              "Buck stopped with %s signal"
              (PrintSignal.string_of_signal signal)
          in
          fail_with_error description)


module V1 = struct
  type t = {
    query: Command.t;
    build: Command.t;
  }

  let create_for_testing ~query ~build () = { query; build }

  let isolation_prefix_to_buck_arguments = function
    | None
    | Some "" ->
        []
    | Some isolation_prefix -> ["--isolation_prefix"; isolation_prefix]


  let create ?(additional_log_size = 0) () =
    let buck1 = "buck1" in
    let open Lwt.Infix in
    let invoke_buck ?mode ?isolation_prefix ~command user_supplied_arguments =
      (* Preserve the last several lines of Buck log for error reporting purpose. *)
      let log_buffer = BoundedQueue.create additional_log_size in
      let common_buck_arguments =
        List.concat
          [
            isolation_prefix_to_buck_arguments isolation_prefix;
            [command];
            mode_to_buck_arguments mode;
          ]
      in
      let expanded_buck_arguments = List.append common_buck_arguments user_supplied_arguments in
      Log.debug
        "Running command: %s"
        (Stdlib.Filename.quote_command "buck2" expanded_buck_arguments);
      (* Sometimes the total length of buck cli arguments can go beyond the limit of the underlying
         operating system. Pass all the user-supplied arguments via a temporary file instead. *)
      Lwt_io.with_temp_file ~prefix:"buck_arguments_" (fun (filename, output_channel) ->
          Lwt_io.write_lines output_channel (Lwt_stream.of_list user_supplied_arguments)
          >>= fun () ->
          Lwt_io.flush output_channel
          >>= fun () ->
          let actual_buck_arguments =
            List.concat [common_buck_arguments; [Stdlib.Format.sprintf "@%s" filename]]
          in
          let consume_stderr = consume_stderr ~log_buffer in
          LwtSubprocess.run buck1 ~arguments:actual_buck_arguments ~consume_stderr)
      >>= fun result ->
      on_completion ~buck_command:buck1 ~arguments:expanded_buck_arguments ~log_buffer result
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
end

module V2 = struct
  type t = { bxl: Command.t }

  let create_for_testing ~bxl () = { bxl }

  let isolation_prefix_to_buck_arguments = function
    | None
    | Some "" ->
        []
    | Some isolation_prefix -> ["--isolation-dir"; isolation_prefix]


  let read_build_id_from filename =
    let open Lwt.Infix in
    let read () =
      Lwt_io.with_file ~mode:Lwt_io.Input filename Lwt_io.read
      >>= fun build_id ->
      Log.info "UUID of the build = %s" build_id;
      Lwt.return_some build_id
    in
    let error exn =
      Log.info "Failed to read build UUID from %s: %s" filename (Exn.to_string exn);
      Lwt.return_none
    in
    Lwt.catch read error


  let create ?(additional_log_size = 0) () =
    let buck2 = "buck2" in
    let open Lwt.Infix in
    let invoke_buck ?mode ?isolation_prefix ~command user_supplied_arguments =
      (* Preserve the last several lines of Buck log for error reporting purpose. *)
      let log_buffer = BoundedQueue.create additional_log_size in
      let common_buck_arguments =
        List.concat
          [
            isolation_prefix_to_buck_arguments isolation_prefix;
            [command];
            mode_to_buck_arguments mode;
          ]
      in
      let expanded_buck_arguments = List.append common_buck_arguments user_supplied_arguments in
      Log.debug
        "Running command: %s"
        (Stdlib.Filename.quote_command "buck2" expanded_buck_arguments);
      (* Sometimes the total length of buck cli arguments can go beyond the limit of the underlying
         operating system. Pass all the user-supplied arguments via a temporary file instead. *)
      Lwt_io.with_temp_dir ~prefix:"pyre_buck" (fun directory_name ->
          let argument_filename = Stdlib.Filename.concat directory_name "arguments" in
          Lwt_io.with_file ~mode:Lwt_io.Output argument_filename (fun output_channel ->
              Lwt_io.write_lines output_channel (Lwt_stream.of_list user_supplied_arguments))
          >>= fun () ->
          let build_id_filename = Stdlib.Filename.concat directory_name "build_id" in
          let actual_buck_arguments =
            List.concat
              [
                common_buck_arguments;
                ["--write-build-id"; build_id_filename];
                [Stdlib.Format.sprintf "@%s" argument_filename];
              ]
          in
          let consume_stderr = consume_stderr ~log_buffer in
          LwtSubprocess.run buck2 ~arguments:actual_buck_arguments ~consume_stderr
          >>= fun result ->
          read_build_id_from build_id_filename
          >>= fun build_id ->
          on_completion
            ~buck_command:buck2
            ~arguments:expanded_buck_arguments
            ~log_buffer
            ?build_id
            result)
    in
    let bxl ?mode ?isolation_prefix arguments =
      invoke_buck ?mode ?isolation_prefix ~command:"bxl" arguments
    in
    { bxl }


  let bxl { bxl; _ } = bxl
end
