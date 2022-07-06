(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type section =
  [ `Check
  | `Debug
  | `Dependencies
  | `DependencyGraph
  | `Dump
  | `Environment
  | `Error
  | `Event
  | `Fixpoint
  | `Infer
  | `Info
  | `Interprocedural
  | `Memory
  | `Performance
  | `Progress
  | `Protocols
  | `Server
  | `Taint
  | `Warning
  ]

let section_to_string = function
  | `Check -> "Check"
  | `Debug -> "Debug"
  | `Dependencies -> "Dependencies"
  | `DependencyGraph -> "DependencyGraph"
  | `Dump -> "Dump"
  | `Environment -> "Environment"
  | `Error -> "Error"
  | `Event -> "Event"
  | `Fixpoint -> "Fixpoint"
  | `Info -> "Info"
  | `Infer -> "Infer"
  | `Interprocedural -> "Interprocedural"
  | `Memory -> "Memory"
  | `Performance -> "Performance"
  | `Progress -> "Progress"
  | `Protocols -> "Protocols"
  | `Server -> "Server"
  | `Taint -> "Taint"
  | `Warning -> "Warning"


module GlobalState = struct
  let enabled =
    String.Hash_set.of_list
      ["Dump"; "Error"; "Info"; "Memory"; "Progress"; "Performance"; "Warning"]


  let initialize ~debug ~sections =
    if debug then
      Hash_set.add enabled "Debug";
    let handle_section section =
      let normalize section = String.lowercase section |> String.capitalize in
      match String.chop_prefix ~prefix:"-" section with
      | Some section -> normalize section |> Hash_set.remove enabled
      | None -> normalize section |> Hash_set.add enabled
    in
    List.iter ~f:handle_section sections


  let initialize_for_tests () =
    Hash_set.clear enabled;
    Hash_set.add enabled "Dump"


  type t = string list

  let get () = Hash_set.to_list enabled

  let restore saved_state =
    Hash_set.clear enabled;
    List.iter saved_state ~f:(Hash_set.add enabled)
end

let is_enabled section = Hash_set.mem GlobalState.enabled (section_to_string section)

let time_zone = ref None

(* A safer version of Time.Zone.local, which defaults to UTC instead of throwing an exception if we
   cannot figure out local time. See https://github.com/janestreet/core/issues/96 for one example
   when this can happen *)
let get_time_zone () =
  match !time_zone with
  | Some zone -> zone
  | None ->
      let zone =
        try force Time.Zone.local with
        | _ -> Time.Zone.utc
      in
      time_zone := Some zone;
      zone


let log ~section format =
  let section = section_to_string section in
  if Hash_set.mem GlobalState.enabled section then
    let zone = get_time_zone () in
    Format.fprintf
      Format.err_formatter
      ("%s %s " ^^ format ^^ "\n%!")
      (Time.format ~zone (Time.now ()) "%Y-%m-%d %H:%M:%S")
      (String.uppercase section)
  else
    Format.ifprintf Format.err_formatter format


let debug format = log ~section:`Debug format

let dump format = log ~section:`Dump format

let info format = log ~section:`Info format

let error format = log ~section:`Error format

let warning format = log ~section:`Warning format

let print format = Printf.printf format

let log_unix_error ?(section = `Error) (error_kind, name, parameters) =
  log ~section "Unix error %s: %s(%s)" (Unix.error_message error_kind) name parameters


let log_exception message exception_to_log backtrace =
  error
    "%s\nException: %s\nBacktrace:\n%s"
    message
    (Exn.to_string exception_to_log)
    (Caml.Printexc.raw_backtrace_to_string backtrace)


module Color = struct
  let cyan string = Format.asprintf "\027[36m%s\027[0m" string

  let red string = Format.asprintf "\027[31m%s\027[0m" string

  let yellow string = Format.asprintf "\027[33m%s\027[0m" string
end

let rotate ?(number_to_keep = 10) basename =
  let timestamp = Time.to_filename_string ~zone:(get_time_zone ()) (Time.now ()) in
  let suppress_system_error f =
    try f () with
    | Sys_error _
    | Unix.Unix_error _ ->
        ()
  in
  let rotate_old_logs () =
    Filename.dirname basename
    |> Sys.ls_dir
    (* The "." is to prevent us from counting a symlinked log as a log to keep. *)
    |> List.filter ~f:(String.is_prefix ~prefix:(Filename.basename basename ^ "."))
    |> List.sort ~compare:String.compare (* Sorts by earliest date, i.e. least recent *)
    |> List.rev
    |> (fun list -> List.drop list number_to_keep)
    |> List.iter ~f:(fun path ->
           suppress_system_error (fun () -> Unix.remove (Filename.dirname basename ^/ path)))
  in
  suppress_system_error rotate_old_logs;
  let is_file_or_link path =
    try
      let { Unix.st_kind; _ } = Unix.lstat path in
      match st_kind with
      | Unix.S_LNK
      | Unix.S_REG ->
          true
      | _ -> false
    with
    | Unix.Unix_error _ -> false
  in
  if is_file_or_link basename then
    suppress_system_error (fun () -> Unix.unlink basename);
  let actual_path = Format.sprintf "%s.%s" basename timestamp in
  suppress_system_error (fun () -> Unix.symlink ~target:actual_path ~link_name:basename);
  actual_path
