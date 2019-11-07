(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
module Error = AnalysisError

type t = {
  global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
  set_errors: Reference.t -> Error.t list -> unit;
  invalidate: Reference.t list -> unit;
  get_errors: Reference.t -> Error.t list;
}

let global_environment { global_environment; _ } = global_environment

let global_resolution { global_environment; _ } =
  AnnotatedGlobalEnvironment.ReadOnly.resolution global_environment


let ast_environment { global_environment; _ } =
  AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment


let set_errors { set_errors; _ } = set_errors

let get_errors { get_errors; _ } = get_errors

let invalidate { invalidate; _ } = invalidate

module AnalysisErrorValue = struct
  type t = Error.t list

  let prefix = Prefix.make ()

  let description = "Raw analysis errors"

  let unmarshall value = Marshal.from_string value 0
end

module RawErrors = Memory.NoCache.Make (SharedMemoryKeys.ReferenceKey) (AnalysisErrorValue)

let create global_environment =
  let get_errors reference = RawErrors.get reference |> Option.value ~default:[] in
  let set_errors = RawErrors.add in
  let invalidate qualifiers = RawErrors.KeySet.of_list qualifiers |> RawErrors.remove_batch in
  { global_environment; set_errors; invalidate; get_errors }


module ReadOnly = struct
  type t = {
    global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
    get_errors: Reference.t -> Error.t list;
  }

  let create ?(get_errors = fun _ -> []) global_environment = { global_environment; get_errors }

  let global_environment { global_environment; _ } = global_environment

  let global_resolution { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.resolution global_environment


  let ast_environment { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment


  let get_errors { get_errors; _ } = get_errors
end

let read_only { global_environment; get_errors; _ } =
  ReadOnly.create ~get_errors global_environment
