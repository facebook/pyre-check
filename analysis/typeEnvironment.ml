(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open Pyre
module Error = AnalysisError

type t = {
  global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
  set_errors: Reference.t -> Error.t list -> unit;
  set_local_annotations: Reference.t -> (Reference.t * LocalAnnotationMap.t) list -> unit;
  invalidate: Reference.t list -> unit;
  invalidate_local_annotations: Reference.t list -> unit;
  get_errors: Reference.t -> Error.t list;
  get_local_annotations: Reference.t -> (Reference.t * LocalAnnotationMap.t) list option;
}

let global_environment { global_environment; _ } = global_environment

let global_resolution { global_environment; _ } = GlobalResolution.create global_environment

let ast_environment { global_environment; _ } =
  AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment


let set_errors { set_errors; _ } = set_errors

let get_errors { get_errors; _ } = get_errors

let set_local_annotations { set_local_annotations; _ } = set_local_annotations

let get_local_annotations { get_local_annotations; _ } = get_local_annotations

let invalidate { invalidate; _ } = invalidate

let invalidate_local_annotations { invalidate_local_annotations; _ } = invalidate_local_annotations

module AnalysisErrorValue = struct
  type t = Error.t list

  let prefix = Prefix.make ()

  let description = "Raw analysis errors"

  let unmarshall value = Marshal.from_string value 0
end

module LocalAnnotationsValue = struct
  type t = (Reference.t * LocalAnnotationMap.t) list

  let prefix = Prefix.make ()

  let description = "Node type resolution"

  let unmarshall value = Marshal.from_string value 0
end

module RawErrors = Memory.NoCache.Make (SharedMemoryKeys.ReferenceKey) (AnalysisErrorValue)
module LocalAnnotations =
  Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (LocalAnnotationsValue)

let create global_environment =
  let get_errors reference = RawErrors.get reference |> Option.value ~default:[] in
  let set_errors = RawErrors.add in
  let get_local_annotations = LocalAnnotations.get in
  let set_local_annotations = LocalAnnotations.add in
  let invalidate qualifiers =
    RawErrors.KeySet.of_list qualifiers |> RawErrors.remove_batch;
    LocalAnnotations.KeySet.of_list qualifiers |> LocalAnnotations.remove_batch
  in
  let invalidate_local_annotations qualifiers =
    LocalAnnotations.KeySet.of_list qualifiers |> LocalAnnotations.remove_batch
  in
  {
    global_environment;
    set_errors;
    set_local_annotations;
    invalidate;
    invalidate_local_annotations;
    get_errors;
    get_local_annotations;
  }


module ReadOnly = struct
  type t = {
    global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
    get_errors: Reference.t -> Error.t list;
    get_local_annotations: Reference.t -> (Reference.t * LocalAnnotationMap.t) list option;
  }

  let create ?(get_errors = fun _ -> []) ?(get_local_annotations = fun _ -> None) global_environment
    =
    { global_environment; get_errors; get_local_annotations }


  let global_environment { global_environment; _ } = global_environment

  let global_resolution { global_environment; _ } = GlobalResolution.create global_environment

  let ast_environment { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment


  let get_errors { get_errors; _ } = get_errors

  let get_local_annotations { get_local_annotations; _ } = get_local_annotations

  let get_local_annotation_map_for_define { get_local_annotations; _ } ~qualifier name =
    get_local_annotations qualifier
    >>= fun local_annotations -> List.Assoc.find local_annotations name ~equal:Reference.equal
end

let read_only { global_environment; get_errors; get_local_annotations; _ } =
  ReadOnly.create ~get_errors ~get_local_annotations global_environment
