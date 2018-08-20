(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


let enabled = ref true


let cache = String.Table.create ()
let size = 500


let disable () =
  enabled := false


let sample
    ?(integers = [])
    ?(normals = [])
    ?(metadata = true)
    () =
  let local_root, start_time, log_identifier =
    match Configuration.get_global () with
    | Some { Configuration.local_root; start_time; log_identifier; _ } ->
        Path.last local_root, start_time, log_identifier
    | _ ->
        Log.warning "Trying to log without a global configuration";
        "LOGGED WITHOUT CONFIGURATION", 0.0, "no configuration"
  in
  let normals =
    if metadata then
      [
        "binary", Sys.argv.(0);
        "root", local_root;
        "username", Option.value ~default:"" (Sys.getenv "USER");
        "hostname", Option.value ~default:"" (Sys.getenv "HOSTNAME");
        "identifier", log_identifier;
      ] @ normals
    else
      normals
  in
  let integers =
    if metadata then
      [
        "time", Unix.time () |> Int.of_float;
        "start_time", start_time |> Int.of_float;
      ] @ integers
    else
      integers
  in
  Yojson.Safe.to_string (
    `Assoc
      [
        "int", `Assoc (List.map ~f:(fun (label, data) -> label, `Int data) integers);
        "normal", `Assoc (List.map ~f:(fun (label, data) -> label, `String data) normals);
      ])


let flush () =
  let flush_category ~key ~data =
    Configuration.get_global ()
    >>= (fun { Configuration.logger = logger; _ } -> logger)
    >>| (fun logger -> Format.sprintf "%s %s" logger key)
    >>| (fun command ->
        let out_channel = Unix.open_process_out command in
        List.iter ~f:(Printf.fprintf out_channel "%s\n") data;
        Out_channel.flush out_channel;
        Unix.close_process_out out_channel
        |> ignore)
    |> ignore
  in
  if !enabled then
    Hashtbl.iteri ~f:flush_category cache;
  Hashtbl.clear cache


let flush_cache = flush


let log ?(flush = false) ?(randomly_log_every = 1) category sample =
  if Random.int randomly_log_every = 0 then
    begin
      match Hashtbl.find cache category with
      | Some samples -> Hashtbl.set ~key:category ~data:(sample :: samples) cache
      | _ -> Hashtbl.set ~key:category ~data:[sample] cache
    end;
  if flush || Hashtbl.length cache >= size then
    flush_cache ()


let performance
    ?(flush = false)
    ?(randomly_log_every = 1)
    ?(section = `Performance)
    ~name
    ~timer
    ?(integers = [])
    ?(normals = []) () =
  let seconds = Timer.stop timer in
  let milliseconds = Int.of_float (seconds *. 1000.0) in
  Log.log ~section "%s: %fs" (String.capitalize name) seconds;
  sample
    ~integers:(("elapsed_time", milliseconds) :: integers)
    ~normals:(("name", name) :: normals)
    ()
  |> log ~flush ~randomly_log_every "perfpipe_pyre_performance"


let coverage ?(flush = false) ~coverage ?(normals = []) () =
  sample ~integers:coverage ~normals ()
  |> log ~flush "perfpipe_pyre_coverage"


let event ?(flush = false) ?(section = `Event) ~name ?(integers = []) ?(normals = []) () =
  let integer (name, value) = Format.asprintf "%s: %d" name value in
  let normal (name, value) = Format.asprintf "%s: %s" name value in
  Log.log
    ~section
    "%s (%s)"
    (String.capitalize name)
    (List.map ~f:integer integers @ List.map ~f:normal normals
     |> String.concat ~sep:", ");
  sample ~integers:integers ~normals:(("name", name) :: normals) ()
  |> log ~flush "perfpipe_pyre_events"
