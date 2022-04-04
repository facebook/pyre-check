(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
module Error = AnalysisError

type t = {
  global_environment: AnnotatedGlobalEnvironment.t;
  set_errors: Reference.t -> Error.t list -> unit;
  set_local_annotations: Reference.t -> LocalAnnotationMap.ReadOnly.t -> unit;
  invalidate: Reference.t list -> unit;
  get_errors: Reference.t -> Error.t list;
  get_local_annotations: Reference.t -> LocalAnnotationMap.ReadOnly.t option;
}

let global_environment { global_environment; _ } = global_environment

let ast_environment { global_environment; _ } =
  AnnotatedGlobalEnvironment.ast_environment global_environment


let module_tracker type_environment =
  ast_environment type_environment |> AstEnvironment.module_tracker


let set_errors { set_errors; _ } = set_errors

let get_errors { get_errors; _ } = get_errors

let set_local_annotations { set_local_annotations; _ } = set_local_annotations

let get_local_annotations { get_local_annotations; _ } = get_local_annotations

let invalidate { invalidate; _ } = invalidate

module AnalysisErrorValue = struct
  type t = Error.t list

  let prefix = Prefix.make ()

  let description = "Raw analysis errors"

  let unmarshall value = Marshal.from_string value 0
end

module LocalAnnotationsValue = struct
  type t = LocalAnnotationMap.ReadOnly.t

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
  {
    global_environment;
    set_errors;
    set_local_annotations;
    invalidate;
    get_errors;
    get_local_annotations;
  }


module ReadOnly = struct
  type t = {
    global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
    get_errors: Reference.t -> Error.t list;
    get_local_annotations: Reference.t -> LocalAnnotationMap.ReadOnly.t option;
  }

  let create ?(get_errors = fun _ -> []) ?(get_local_annotations = fun _ -> None) global_environment
    =
    { global_environment; get_errors; get_local_annotations }


  let global_environment { global_environment; _ } = global_environment

  let global_resolution { global_environment; _ } = GlobalResolution.create global_environment

  let ast_environment { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment


  let unannotated_global_environment { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment global_environment


  let get_errors { get_errors; _ } = get_errors

  let get_local_annotations { get_local_annotations; _ } = get_local_annotations
end

let read_only { global_environment; get_errors; get_local_annotations; _ } =
  ReadOnly.create
    ~get_errors
    ~get_local_annotations
    (AnnotatedGlobalEnvironment.read_only global_environment)
