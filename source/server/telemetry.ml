(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module Message = struct
  type message =
    | Base of {
        root: string;
        hash: string;
      }
    | Update of {
        (* The root of the project, relative to `hg root`. *)
        root: string;
        hash: string;
        (* Currently, the output of `hg diff`. *)
        content: string;
      }

  type t = { message: message }

  let to_normals { message } =
    match message with
    | Base { root; hash } -> ["kind", "base"; "root", root; "hash", hash]
    | Update { root; hash; content } ->
        ["kind", "update"; "root", root; "hash", hash; "content", content]
end

let get_shell_output ~command =
  let { Unix.Process_channels.stdout; stderr; stdin } = Unix.open_process_full ~env:[||] command in
  Out_channel.close stdin;
  let data = In_channel.input_all stdout |> String.strip in
  let error_output = In_channel.input_all stderr |> String.strip in
  In_channel.close stdout;
  In_channel.close stderr;
  if String.length error_output > 0 then
    Log.log
      ~section:`Server
      "[Telemetry] Command '%s' has non-empty stderr: %s"
      command
      error_output;
  data


let hg_commit_hash project_root =
  let command = Format.sprintf "cd %s && hg whereami" (Path.absolute project_root) in
  get_shell_output ~command


let hg_diff project_root =
  let command = Format.sprintf "cd %s && hg diff" (Path.absolute project_root) in
  (* `patch` complains if there is no trailing newline. *)
  get_shell_output ~command ^ "\n"


let hg_root project_root =
  let command = Format.sprintf "cd %s && hg root" (Path.absolute project_root) in
  get_shell_output ~command


(* Maximum diff size in characters. *)
let maximum_diff_size = 5000

let create_session_start_message ~local_root ~project_root =
  {
    Message.message =
      Message.Base { root = Path.last local_root; hash = hg_commit_hash project_root };
  }


let create_update_message ~local_root ~project_root ~filter_directories =
  Log.info "Sending telemetry update message...";
  let hg_root_absolute = hg_root project_root in
  let project_directory_absolute =
    Path.create_absolute
      ~follow_symbolic_links:true
      (Path.project_directory
         ~local_root:(Path.show local_root)
         ~filter_directories:
           (Option.value_map ~default:[] ~f:(List.map ~f:Path.show) filter_directories))
  in
  let project_directory_relative =
    Path.get_relative_to_root
      ~root:(Path.create_absolute ~follow_symbolic_links:true hg_root_absolute)
      ~path:project_directory_absolute
  in
  let diff = hg_diff project_root in
  {
    Message.message =
      Message.Update
        {
          root = Option.value ~default:"<error>" project_directory_relative;
          hash = hg_commit_hash project_root;
          content = (if String.length diff > maximum_diff_size then "" else diff);
        };
  }


let budget = ref 0

let reset_budget ?(value = 6) () = budget := value

let send_telemetry ~f () =
  match !budget with
  | x when x <= 0 -> ()
  | current_budget -> (
      (* Call the function with () to evaluate it only when budget is available.*)
      try
        f () |> Message.to_normals |> Statistics.server_telemetry;
        budget := current_budget - 1
      with
      | _ -> ())
