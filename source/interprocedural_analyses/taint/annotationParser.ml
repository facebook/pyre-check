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
module AccessPath = Analysis.TaintAccessPath

(* Represents a source or sink kind from a model (e.g, UserControlled). *)
module KindExpression = struct
  type t =
    | Name of {
        name: string;
        subkind: string option;
      }
    | Updates of AccessPath.Root.t
  [@@deriving equal]

  let from_name name = Name { name; subkind = None }
end

(* Represents a source or sink defined in a taint.config file. *)
module KindDefinition = struct
  type taint_kind =
    | Named
    | Parametric

  type t = {
    name: string;
    kind: taint_kind;
    location: JsonParsing.JsonAst.LocationWithPath.t option;
  }
end

let parse_source ~allowed = function
  | KindExpression.Name { name; subkind } -> (
      match
        ( List.find allowed ~f:(fun { KindDefinition.name = source_name; _ } ->
              String.equal source_name name),
          subkind )
      with
      (* In order to support parsing parametric sources and sinks for rules, we allow matching
         parametric source kinds with no subkind. *)
      | Some _, None -> Ok (Sources.NamedSource name)
      | Some { kind = Parametric; _ }, Some subkind ->
          Ok (Sources.ParametricSource { source_name = name; subkind })
      | _ -> Error (Format.sprintf "Unsupported taint source `%s`" name))
  | KindExpression.Updates _ -> Error "Unsupported taint source `Updates[]`"


let parse_sink ~allowed = function
  | KindExpression.Name { name; subkind } -> (
      match
        ( List.find allowed ~f:(fun { KindDefinition.name = sink_name; _ } ->
              String.equal sink_name name),
          subkind )
      with
      (* In order to support parsing parametric sources and sinks for rules, we allow matching
         parametric source kinds with no subkind. *)
      | Some _, None -> Ok (Sinks.NamedSink name)
      | Some { kind = Parametric; _ }, Some subkind ->
          Ok (Sinks.ParametricSink { sink_name = name; subkind })
      | _ -> Error (Format.sprintf "Unsupported taint sink `%s`" name))
  | KindExpression.Updates _ -> Error "Unsupported taint sink `Updates[]`"


let parse_transform ~allowed name =
  match List.find allowed ~f:(TaintTransform.equal (TaintTransform.Named name)) with
  | Some transform -> Ok transform
  | None -> Error (Format.sprintf "Unsupported transform `%s`" name)


let parse_tito ~allowed_transforms = function
  | KindExpression.Name { name = "LocalReturn"; _ } -> Ok Sinks.LocalReturn
  | KindExpression.Name { name = "Transform"; subkind = None } ->
      Error "Tito transform requires name of the transform as parameter"
  | KindExpression.Name { name = "Transform"; subkind = Some transform } ->
      parse_transform ~allowed:allowed_transforms transform
      >>= fun transform ->
      Ok
        (Sinks.make_transform
           ~local:[transform]
           ~global:TaintTransforms.empty
           ~base:Sinks.LocalReturn)
  | KindExpression.Name { name; _ } ->
      Error (Format.sprintf "Unsupported taint in taint out specification `%s`" name)
  | KindExpression.Updates root when AccessPath.Root.is_parameter root ->
      Ok (Sinks.ParameterUpdate root)
  | KindExpression.Updates root ->
      Error
        (Format.asprintf
           "Unsupported taint in taint out specification `Updates[%a]`"
           AccessPath.Root.pp
           root)
