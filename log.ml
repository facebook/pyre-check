(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type section = [
    `Check
  | `Debug
  | `Dependencies
  | `Dump
  | `Environment
  | `Error
  | `Info
  | `Performance
  | `Protocols
  | `Server
  | `Warning
]


let section_to_string = function
  | `Check -> "Check"
  | `Debug -> "Debug"
  | `Dependencies -> "Dependencies"
  | `Dump -> "Dump"
  | `Environment -> "Environment"
  | `Error -> "Error"
  | `Info -> "Info"
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

let statistics_logging_enabled = ref true

let initialize ~verbose ~sections =
  if verbose then
    Hash_set.add enabled "Debug";

  List.map ~f:(fun section -> String.lowercase section |> String.capitalize) sections
  |> List.iter ~f:(fun section -> Hash_set.add enabled section)


let initialize_for_tests () =
  Hash_set.clear enabled;
  Hash_set.add enabled "Dump";
  statistics_logging_enabled := false


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


let performance ?(flush = false) ?(randomly_log_every=1) ~name ~timer ~root ~normals () =
  if Random.int randomly_log_every = 0 then
    begin
      let seconds = Timer.stop timer in
      let milliseconds = Int.of_float (seconds *. 1000.0) in
      log ~section:`Performance "Time elapsed for %s: %fs" name seconds;
      if !statistics_logging_enabled then
        Statistics.performance ~flush ~time:milliseconds ~root ~normals |> ignore
    end

let coverage ?(flush = false) ~coverage ~root ~normals () =
  if !statistics_logging_enabled then
    Statistics.coverage ~flush ~coverage ~root ~normals |> ignore


let event ?(flush = false) ~name ~root ~integers ~normals () =
  if !statistics_logging_enabled then
    Statistics.event ~flush ~name ~root ~integers ~normals


module Color = struct
  let yellow string =
    Format.asprintf "\027[33m%s\027[0m" string
end
