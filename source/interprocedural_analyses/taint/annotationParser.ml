(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type taint_kind =
  | Named
  | Parametric

type source_or_sink = {
  name: string;
  kind: taint_kind;
}

let parse_source ~allowed ?subkind name =
  match
    List.find allowed ~f:(fun { name = source_name; _ } -> String.equal source_name name), subkind
  with
  (* In order to support parsing parametric sources and sinks for rules, we allow matching
     parametric source kinds with no subkind. *)
  | Some _, None -> Sources.NamedSource name
  | Some { kind = Parametric; _ }, Some subkind ->
      Sources.ParametricSource { source_name = name; subkind }
  | _ -> failwith (Format.sprintf "Unsupported taint source `%s`" name)


let parse_sink ~allowed ?subkind name =
  let create = function
    | "LocalReturn" -> Sinks.LocalReturn
    | update when String.is_prefix update ~prefix:"ParameterUpdate" ->
        let index = String.chop_prefix_exn update ~prefix:"ParameterUpdate" in
        ParameterUpdate (Int.of_string index)
    | name -> failwith (Format.sprintf "Unsupported taint sink `%s`" name)
  in
  match
    List.find allowed ~f:(fun { name = sink_name; _ } -> String.equal sink_name name), subkind
  with
  (* In order to support parsing parametric sources and sinks for rules, we allow matching
     parametric source kinds with no subkind. *)
  | Some _, None -> Sinks.NamedSink name
  | Some { kind = Parametric; _ }, Some subkind ->
      Sinks.ParametricSink { sink_name = name; subkind }
  | _ -> create name
