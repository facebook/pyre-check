(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AnnotationParser: implements logic to parse source, sink and transform from
 * arbitrary python expressions.
 *)

open Core
open Result

type taint_kind =
  | Named
  | Parametric

type source_or_sink = {
  name: string;
  kind: taint_kind;
  location: JsonParsing.JsonAst.LocationWithPath.t option;
}

let parse_source ~allowed ?subkind name =
  match
    List.find allowed ~f:(fun { name = source_name; _ } -> String.equal source_name name), subkind
  with
  (* In order to support parsing parametric sources and sinks for rules, we allow matching
     parametric source kinds with no subkind. *)
  | Some _, None -> Ok (Sources.NamedSource name)
  | Some { kind = Parametric; _ }, Some subkind ->
      Ok (Sources.ParametricSource { source_name = name; subkind })
  | _ -> Error (Format.sprintf "Unsupported taint source `%s`" name)


let parse_sink ~allowed ?subkind name =
  match
    List.find allowed ~f:(fun { name = sink_name; _ } -> String.equal sink_name name), subkind
  with
  (* In order to support parsing parametric sources and sinks for rules, we allow matching
     parametric source kinds with no subkind. *)
  | Some _, None -> Ok (Sinks.NamedSink name)
  | Some { kind = Parametric; _ }, Some subkind ->
      Ok (Sinks.ParametricSink { sink_name = name; subkind })
  | _ -> Error (Format.sprintf "Unsupported taint sink `%s`" name)


let parse_transform ~allowed reference_name =
  match
    List.find allowed ~f:(function
        | TaintTransform.Named { name; _ } -> String.equal name reference_name
        | _ -> false)
  with
  | Some (TaintTransform.Named { name; _ }) -> Ok (TaintTransform.Named { name; location = None })
  | Some (TaintTransform.Sanitize _ as transform) -> Ok transform
  | None -> Error (Format.sprintf "Unsupported transform `%s`" reference_name)


let parse_tito ~allowed_transforms ?subkind name =
  match name, subkind with
  | "LocalReturn", _ -> Ok Sinks.LocalReturn
  | update, _ when String.is_prefix update ~prefix:"ParameterUpdate" ->
      let index = String.chop_prefix_exn update ~prefix:"ParameterUpdate" in
      Ok (ParameterUpdate (Int.of_string index))
  | "Transform", None -> Error "Tito transform requires name of the transform as parameter"
  | "Transform", Some transform ->
      parse_transform ~allowed:allowed_transforms transform
      >>= fun transform ->
      Ok
        (Sinks.make_transform
           ~local:[transform]
           ~global:TaintTransforms.empty
           ~base:Sinks.LocalReturn)
  | name, _ -> Error (Format.sprintf "Unsupported taint in taint out specification `%s`" name)
