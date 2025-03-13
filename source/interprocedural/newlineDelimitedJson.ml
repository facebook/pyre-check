(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* NewlineDelimitedJson: functions to write files in the newline-delimited json
 * format.
 *
 * The newline-delimited JSON format is a format where each line is a JSON
 * object with a `kind` and `data` attribute. The format starts with a header
 * which is also a JSON object.
 *
 * Here is an example of a newline-delimited JSON file:
 * ```
 * {"file_version": 3, "config": {"repo", "/some/path"}}
 * {"kind": "message", "data": "hello world"}
 * {"kind": "integer", "data": 42}
 * {"kind": "person", "data": {"first_name": "Joe", "last_name": "Smith"}}
 * ```
 * *)

open Core

module Kind = struct
  type t =
    | Model
    | Issue
    | CallGraph
    | HigherOrderCallGraph
    | Module
    | Function

  let show = function
    | Model -> "model"
    | Issue -> "issue"
    | CallGraph -> "call_graph"
    | HigherOrderCallGraph -> "higher_order_call_graph"
    | Module -> "module"
    | Function -> "function"
end

module Line = struct
  type t = {
    kind: Kind.t;
    data: Yojson.Safe.t;
  }

  let to_json { kind; data } = `Assoc ["kind", `String (Kind.show kind); "data", data]
end

let write_header ~out_channel configuration =
  `Assoc ["file_version", `Int 3; "config", configuration] |> Yojson.Safe.to_channel out_channel;
  Printf.fprintf out_channel "\n"


let write_line ~out_channel line =
  Yojson.Safe.to_channel out_channel (Line.to_json line);
  Printf.fprintf out_channel "\n"


let write_file ~path ~configuration ~to_json_lines elements =
  let out_channel = Out_channel.create (PyrePath.absolute path) in
  write_header ~out_channel configuration;
  List.iter elements ~f:(fun element ->
      element |> to_json_lines |> List.iter ~f:(write_line ~out_channel));
  Out_channel.close out_channel


let remove_sharded_files ~directory ~filename_prefix =
  PyrePath.read_directory_ordered directory
  |> List.filter ~f:(fun path ->
         let filename = PyrePath.last path in
         String.is_prefix filename ~prefix:filename_prefix
         && String.is_suffix filename ~suffix:".json")
  |> List.iter ~f:PyrePath.unlink_if_exists


let write_sharded_files
    ~scheduler
    ~directory
    ~filename_prefix
    ~configuration
    ~to_json_lines
    elements
  =
  let shard_size = Int.max 1 (List.length elements / Scheduler.number_workers scheduler) in
  let shards =
    elements
    |> List.chunks_of ~length:shard_size
    |> List.mapi ~f:(fun shard_index element -> shard_index, element)
  in
  let number_shards = List.length shards in
  let write_json_shard (shard_index, elements) =
    let filename =
      Format.sprintf "%s@%05d-of-%05d.json" filename_prefix shard_index number_shards
    in
    let path = PyrePath.append directory ~element:filename in
    write_file ~path ~configuration ~to_json_lines elements
  in
  Scheduler.map_reduce
    scheduler
    ~policy:(Scheduler.Policy.legacy_fixed_chunk_size 1)
    ~initial:()
    ~map:(List.iter ~f:write_json_shard)
    ~reduce:(fun () () -> ())
    ~inputs:shards
    ()
