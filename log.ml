(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type section = [
    `Check
  | `Debug
  | `Dependencies
  | `Dotty
  | `Dump
  | `Environment
  | `Error
  | `Event
  | `Fixpoint
  | `Info
  | `Parser
  | `Performance
  | `Protocols
  | `Server
  | `Warning
]


let section_to_string = function
  | `Check -> "Check"
  | `Debug -> "Debug"
  | `Dependencies -> "Dependencies"
  | `Dotty -> "Dotty"
  | `Dump -> "Dump"
  | `Environment -> "Environment"
  | `Error -> "Error"
  | `Event -> "Event"
  | `Fixpoint -> "Fixpoint"
  | `Info -> "Info"
  | `Parser -> "Parser"
  | `Performance -> "Performance"
  | `Protocols -> "Protocols"
  | `Server -> "Server"
  | `Warning -> "Warning"


let enabled =
  String.Hash_set.of_list [
    "Dump";
    "Error";
    "Info";
    "Performance";
    "Warning";
  ]


let is_enabled section =
  Hash_set.mem enabled (section_to_string section)


let initialize ~verbose ~sections =
  if verbose then
    Hash_set.add enabled "Debug";

  List.map ~f:(fun section -> String.lowercase section |> String.capitalize) sections
  |> List.iter ~f:(fun section -> Hash_set.add enabled section)


let initialize_for_tests () =
  Hash_set.clear enabled;
  Hash_set.add enabled "Dump"


let log ~section format =
  let section = section_to_string section in
  if Hash_set.mem enabled section then
    let zone = force Time.Zone.local in
    Format.fprintf
      Format.err_formatter
      ("%s %s " ^^ format ^^ "\n%!")
      (Time.format ~zone (Time.now ()) "%Y-%m-%d %H:%M:%S")
      (String.uppercase section)
  else
    Format.ifprintf Format.err_formatter format


let debug format =
  log ~section:`Debug format


let dump format =
  log ~section:`Dump format


let info format =
  log ~section:`Info format


let error format =
  log ~section:`Error format


let warning format =
  log ~section:`Warning format


let print format =
  Format.printf format


module Color = struct
  let yellow string =
    Format.asprintf "\027[33m%s\027[0m" string
end


let rotate ?(number_to_keep = 10) basename =
  let timestamp =
    Time.to_filename_string ~zone:(force Time.Zone.local) (Time.now ())
  in
  let suppress_system_error f =
    try
      f ()
    with
    | Sys_error _
    | Unix.Unix_error _ ->
        ()
  in
  let rotate_old_logs () =
    Filename.dirname basename
    |> Sys.ls_dir
    (* The "." is to prevent us from counting a symlinked log as a log to keep. *)
    |> List.filter ~f:(String.is_prefix ~prefix:((Filename.basename basename) ^ "."))
    |> List.sort ~compare:String.compare (* Sorts by earliest date, i.e. least recent *)
    |> List.rev
    |> (fun list -> List.drop list number_to_keep)
    |> List.iter
      ~f:(fun path ->
          suppress_system_error (fun () -> Unix.remove (Filename.dirname basename ^/ path)))
  in
  suppress_system_error rotate_old_logs;
  let is_file_or_link path =
    try
      let { Unix.st_kind; _ } = Unix.lstat path in
      st_kind = Unix.S_LNK || st_kind = Unix.S_REG
    with Unix.Unix_error _ ->
      false
  in
  if is_file_or_link basename then
    suppress_system_error (fun () -> Unix.unlink basename);
  let actual_path = Format.sprintf "%s.%s" basename timestamp in
  suppress_system_error (fun () -> Unix.symlink ~src:actual_path ~dst:basename);
  actual_path
