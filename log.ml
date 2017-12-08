(** Copyright 2016-present Facebook. All rights reserved. **)

open Core


type section = [
    `Check
  | `Debug
  | `Dump
  | `Environment
  | `Error
  | `Info
  | `Performance
  | `Server
  | `Warning
]


let section_to_string = function
  | `Check -> "Check"
  | `Debug -> "Debug"
  | `Dump -> "Dump"
  | `Environment -> "Environment"
  | `Error -> "Error"
  | `Info -> "Info"
  | `Performance -> "Performance"
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


let performance ?(flush = false) ~name ~timer ~labels =
  let seconds = Timer.stop timer in
  let milliseconds = Int.of_float (seconds *. 1000.0) in
  log ~section:`Performance "Time elapsed for %s: %fs" name seconds;
  Statistics.performance ~flush ~time:milliseconds ~labels |> ignore


module Color = struct
  let yellow string =
    Format.asprintf "\027[33m%s\027[0m" string
end
