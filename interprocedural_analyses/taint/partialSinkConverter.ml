(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t = (Sources.t * Sinks.t) list String.Table.t

let mangle { Sinks.kind; label } = Format.sprintf "%s$%s" kind label

let create { Configuration.combined_rules; _ } =
  let table = String.Table.create () in
  let add_entries_for_combined_rule
      { Configuration.first_source; first_sinks; second_source; second_sinks }
    =
    let add (first_sink, second_sink) =
      (* Trigger second sink when the first sink matches a source, and vice versa. *)
      Hashtbl.add_multi
        table
        ~key:(mangle first_sink)
        ~data:(first_source, Sinks.TriggeredPartialSink second_sink);
      Hashtbl.add_multi
        table
        ~key:(mangle second_sink)
        ~data:(second_source, Sinks.TriggeredPartialSink first_sink);
      ()
    in
    List.cartesian_product first_sinks second_sinks |> List.iter ~f:add
  in
  List.iter combined_rules ~f:add_entries_for_combined_rule;
  table


let get_triggered_sink sink_to_sources ~partial_sink ~source =
  match mangle partial_sink |> String.Table.find sink_to_sources with
  | Some source_and_sink_list ->
      List.find source_and_sink_list ~f:(fun (supported_source, _) ->
          Sources.equal source supported_source)
      >>| snd
  | _ -> None
