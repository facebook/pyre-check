(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast

type t = {
  global_resolution: GlobalResolution.t;
  annotations: Annotation.t Reference.Map.t;
  type_variables: Type.Variable.Set.t;
  resolve: resolution:t -> Expression.t -> Annotation.t;
  resolve_assignment: resolution:t -> Statement.Assign.t -> t;
  parent: Reference.t option;
}

let create ~global_resolution ~annotations ~resolve ~resolve_assignment ?parent () =
  {
    global_resolution;
    annotations;
    type_variables = Type.Variable.Set.empty;
    resolve;
    resolve_assignment;
    parent;
  }


let pp format { annotations; type_variables; _ } =
  let annotation_map_entry (reference, annotation) =
    Format.asprintf "%a -> %a" Reference.pp reference Annotation.pp annotation
  in
  Type.Variable.Set.to_list type_variables
  |> List.map ~f:Type.Variable.show
  |> String.concat ~sep:", "
  |> Format.fprintf format "Type variables: [%s]\n";
  Map.to_alist annotations
  |> List.map ~f:annotation_map_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Annotations: [%s]"


let show resolution = Format.asprintf "%a" pp resolution

let is_global { global_resolution; _ } ~reference =
  Reference.delocalize reference |> GlobalResolution.global global_resolution |> Option.is_some


let set_local ({ annotations; _ } as resolution) ~reference ~annotation =
  { resolution with annotations = Map.set annotations ~key:reference ~data:annotation }


let get_local ?(global_fallback = true) ~reference { annotations; global_resolution; _ } =
  match Map.find annotations reference with
  | Some result when global_fallback || not (Annotation.is_global result) -> Some result
  | _ when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.delocalize reference |> global >>| Node.value
  | _ -> None


let unset_local ({ annotations; _ } as resolution) ~reference =
  { resolution with annotations = Map.remove annotations reference }


let add_type_variable ({ type_variables; _ } as resolution) ~variable =
  { resolution with type_variables = Type.Variable.Set.add type_variables variable }


let type_variable_exists { type_variables; _ } ~variable =
  Type.Variable.Set.mem type_variables variable


let all_type_variables_in_scope { type_variables; _ } = Type.Variable.Set.to_list type_variables

let annotations { annotations; _ } = annotations

let with_annotations resolution ~annotations = { resolution with annotations }

let parent { parent; _ } = parent

let with_parent resolution ~parent = { resolution with parent }

let resolve ({ resolve; _ } as resolution) expression =
  resolve ~resolution expression |> Annotation.annotation


let resolve_to_annotation ({ resolve; _ } as resolution) expression = resolve ~resolution expression

let resolve_reference ({ resolve; _ } as resolution) reference =
  Expression.from_reference ~location:Location.any reference
  |> resolve ~resolution
  |> Annotation.annotation


let resolve_assignment ({ resolve_assignment; _ } as resolution) assignment =
  resolve_assignment ~resolution assignment


let resolve_mutable_literals ({ global_resolution; _ } as resolution) =
  GlobalResolution.resolve_mutable_literals global_resolution ~resolve:(resolve resolution)


let is_consistent_with ({ global_resolution; _ } as resolution) =
  GlobalResolution.is_consistent_with global_resolution ~resolve:(resolve resolution)


let global_resolution { global_resolution; _ } = global_resolution
